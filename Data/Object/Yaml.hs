{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
module Data.Object.Yaml
    ( -- * Definition of 'YamlObject'
      YamlScalar (..)
    , YamlObject
      -- * Automatic scalar conversions
    , IsYamlScalar (..)
    , toYamlObject
    , fromYamlObject
      -- * Encoding/decoding
    , encode
    , encodeFile
    , decode
    , decodeFile
      -- * Exceptions
    , ParseException (..)
    ) where

import qualified Text.Libyaml as Y
import Text.Libyaml hiding (encode, decode, encodeFile, decodeFile)
import Data.Object
import Data.ByteString (ByteString)
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)
import Control.Failure

import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.ByteString
import qualified Data.ByteString.Lazy

import Data.Convertible.Text (cs)
import Data.Data

import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Monad
import qualified Data.Enumerator as E
import Data.Enumerator (($$))
import "MonadCatchIO-transformers" Control.Monad.CatchIO hiding (try)
import Prelude hiding (catch)
import Control.Exception (throwIO)

-- | Equality depends on 'value' and 'tag', not 'style'.
data YamlScalar = YamlScalar
    { value :: ByteString
    , tag :: Tag
    , style :: Style
    }
    deriving (Show, Read, Data, Typeable)
instance Eq YamlScalar where
    (YamlScalar v t _) == (YamlScalar v' t' _) = v == v' && t == t'

type YamlObject = Object YamlScalar YamlScalar

class (Eq a) => IsYamlScalar a where
    fromYamlScalar :: YamlScalar -> a
    toYamlScalar :: a -> YamlScalar
instance IsYamlScalar YamlScalar where
    fromYamlScalar = id
    toYamlScalar = id
instance IsYamlScalar Data.Text.Text where
    fromYamlScalar = cs . value
    toYamlScalar t = YamlScalar (cs t) NoTag Any
instance IsYamlScalar Data.Text.Lazy.Text where
    fromYamlScalar = cs . value
    toYamlScalar t = YamlScalar (cs t) NoTag Any
instance IsYamlScalar [Char] where
    fromYamlScalar = cs . value
    toYamlScalar s = YamlScalar (cs s) NoTag Any
instance IsYamlScalar Data.ByteString.ByteString where
    fromYamlScalar = value
    toYamlScalar b = YamlScalar b NoTag Any
instance IsYamlScalar Data.ByteString.Lazy.ByteString where
    fromYamlScalar = cs . value
    toYamlScalar b = YamlScalar (cs b) NoTag Any

-- | Merge assoc-lists by keys.
-- First list overrides second:
--     [(k1, x), (k2, y)] `mergeAssocLists` [(k3, z)] == [(k1, x), (k2, y), (k3, z)]
--     [(k1, x), (k2, y)] `mergeAssocLists` [(k2, z)] == [(k1, x), (k2, y)]
mergeAssocLists :: (Eq k) => [(k, v)] -> [(k, v)] -> [(k, v)]
mergeAssocLists a [] = a
mergeAssocLists [] b = b
mergeAssocLists a ((bk, bv):bs) =
    case lookup bk a of
      Nothing -> mergeAssocLists ((bk, bv) : a) bs
      Just _  -> mergeAssocLists a bs

toYamlObject :: IsYamlScalar k
             => IsYamlScalar v
             => Object k v
             -> YamlObject
toYamlObject = mapKeysValues toYamlScalar toYamlScalar

fromYamlObject :: IsYamlScalar k
               => IsYamlScalar v
               => YamlObject
               -> Object k v
fromYamlObject = mapKeysValues fromYamlScalar fromYamlScalar

encode :: (IsYamlScalar k, IsYamlScalar v) => Object k v -> ByteString
encode obj = unsafePerformIO $ do
    x <- E.run $ E.enumList 1 (objToEvents $ toYamlObject obj) $$ Y.encode
    case x of
        Left err -> throwIO err
        Right y -> return y

encodeFile :: (IsYamlScalar k, IsYamlScalar v)
           => FilePath
           -> Object k v
           -> IO ()
encodeFile fp obj = do
    x <- E.run $ E.enumList 1 (objToEvents $ toYamlObject obj)
              $$ Y.encodeFile fp
    case x of
        Left err -> throwIO err
        Right () -> return ()

objToEvents :: YamlObject -> [Y.Event]
objToEvents o = (:) EventStreamStart
              . (:) EventDocumentStart
              $ objToEvents' o
              [ EventDocumentEnd
              , EventStreamEnd
              ]

scalarToEvent :: YamlScalar -> Event
scalarToEvent (YamlScalar v t s) = EventScalar v t s Nothing

objToEvents' :: YamlObject -> [Y.Event] -> [Y.Event]
objToEvents' (Scalar s) rest = scalarToEvent s : rest
objToEvents' (Sequence list) rest =
    EventSequenceStart Nothing
  : foldr ($) (EventSequenceEnd : rest) (map objToEvents' list)
objToEvents' (Mapping pairs) rest =
    EventMappingStart Nothing
  : foldr ($) (EventMappingEnd : rest) (map pairToEvents pairs)

pairToEvents :: (YamlScalar, YamlObject) -> [Y.Event] -> [Y.Event]
pairToEvents (k, v) rest =
    scalarToEvent k
  : objToEvents' v rest

-- Parsing

data ParseException = NonScalarKey
                    | UnknownAlias { _anchorName :: Y.AnchorName }
                    | UnexpectedEvent { _received :: Maybe Event
                                      , _expected :: Maybe Event
                                      }
                    | InvalidYaml (Maybe String)
    deriving (Show, Typeable)
instance Exception ParseException

newtype PErrorT m a = PErrorT { runPErrorT :: m (Either ParseException a) }
instance Monad m => Monad (PErrorT m) where
    return = PErrorT . return . Right
    (PErrorT m) >>= f = PErrorT $ do
        e <- m
        case e of
            Left e' -> return $ Left e'
            Right a -> runPErrorT $ f a
instance MonadTrans PErrorT where
    lift = PErrorT . liftM Right
instance MonadIO m => MonadIO (PErrorT m) where
    liftIO = lift . liftIO
instance MonadCatchIO m => MonadCatchIO (PErrorT m) where
  m `catch` f = mapPErrorT (\m' -> m' `catch` \e -> runPErrorT $ f e) m
  block       = mapPErrorT block
  unblock     = mapPErrorT unblock

mapPErrorT :: (m (Either ParseException a) -> n (Either ParseException b))
           -> PErrorT m a
           -> PErrorT n b
mapPErrorT f m = PErrorT $ f (runPErrorT m)

pfailure :: Monad m => ParseException -> PErrorT m a
pfailure = PErrorT . return . Left

type Parser = PErrorT (StateT (Map.Map String YamlObject) IO)

requireEvent :: Event -> E.Iteratee Event Parser ()
requireEvent e = do
    f <- E.head
    if f == Just e
        then return ()
        else lift $ pfailure $ UnexpectedEvent f $ Just e

parse :: E.Iteratee Event Parser YamlObject
parse = do
    requireEvent EventStreamStart
    requireEvent EventDocumentStart
    res <- parseO
    requireEvent EventDocumentEnd
    requireEvent EventStreamEnd
    return res

parseScalar :: ByteString -> Tag -> Style -> Anchor
            -> E.Iteratee Event Parser YamlScalar
parseScalar v t s a = do
    let res = YamlScalar v t s
    case a of
        Nothing -> return res
        Just an -> do
            lift $ lift $ modify (Map.insert an $ Scalar res)
            return res

parseO :: E.Iteratee Event Parser YamlObject
parseO = do
    me <- E.head
    case me of
        Just (EventScalar v t s a) -> Scalar `liftM` parseScalar v t s a
        Just (EventSequenceStart a) -> parseS a id
        Just (EventMappingStart a) -> parseM a id
        Just (EventAlias an) -> do
            m <- lift $ lift get
            case Map.lookup an m of
                Nothing -> lift $ pfailure $ UnknownAlias an
                Just v -> return v
        _ -> lift $ pfailure $ UnexpectedEvent me Nothing

parseS :: Y.Anchor
       -> ([YamlObject] -> [YamlObject])
       -> E.Iteratee Event Parser YamlObject
parseS a front = do
    me <- E.peek
    case me of
        Just EventSequenceEnd -> do
            E.drop 1
            let res = Sequence $ front []
            case a of
                Nothing -> return res
                Just an -> do
                    lift $ lift $ modify $ Map.insert an res
                    return res
        _ -> do
            o <- parseO
            parseS a $ front . (:) o

parseM :: Y.Anchor
       -> ([(YamlScalar, YamlObject)] -> [(YamlScalar, YamlObject)])
       -> E.Iteratee Event Parser YamlObject
parseM a front = do
    me <- E.peek
    case me of
        Just EventMappingEnd -> do
            E.drop 1
            let res = Mapping $ front []
            case a of
                Nothing -> return res
                Just an -> do
                    lift $ lift $ modify $ Map.insert an res
                    return res
        _ -> do
            me' <- E.head
            s <- case me' of
                    Just (EventScalar v t s a') -> parseScalar v t s a'
                    _ -> lift $ pfailure $ UnexpectedEvent me' Nothing
            o <- parseO
            let al  = mergeAssocLists [(s, o)] $ front []
                al' = if fromYamlScalar s == "<<"
                         then case o of
                                  Scalar _    -> al
                                  Mapping l  -> mergeAssocLists al l
                                  Sequence l -> mergeAssocLists al $ foldl merge' [] l
                         else al
            parseM a (`mergeAssocLists` al')
    where merge' :: (Eq k) => [(k, Object k v)] -> Object k v -> [(k, Object k v)]
          merge' al (Mapping om) = mergeAssocLists al om
          merge' al _            = al

decode :: (Failure ParseException m, IsYamlScalar k, IsYamlScalar v)
       => ByteString
       -> m (Object k v)
decode bs = unsafePerformIO $ do
    x <- flip evalStateT Map.empty $ runPErrorT $ E.run $ Y.decode bs $$ parse
    case x of
        Left err -> return $ failure err
        Right (Left err) -> return $ failure $ InvalidYaml $ Just $ show err
        Right (Right y) -> return $ return $ fromYamlObject y

decodeFile :: (Failure ParseException m, IsYamlScalar k, IsYamlScalar v)
           => FilePath
           -> IO (m (Object k v))
decodeFile fp = do
    x <- flip evalStateT Map.empty $ runPErrorT $ E.run $ Y.decodeFile fp $$ parse
    case x of
        Left err -> return $ failure err
        Right (Left err) -> return $ failure $ InvalidYaml $ Just $ show err
        Right (Right y) -> return $ return $ fromYamlObject y
