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
      -- * Encoding/decoding
    , encode
    , encodeFile
    , decode
    , decodeFile
#if TEST
    , testSuite
#endif
    ) where

import qualified Text.Libyaml as Y
import Text.Libyaml hiding (encode, decode, encodeFile, decodeFile)
import Data.Object
import Data.ByteString (ByteString)
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (Exception, SomeException (..))
import Data.Typeable (Typeable)
-- debugging purposes import Debug.Trace
import Control.Failure
import Control.Applicative
import qualified Data.Text
import qualified Data.Text.Lazy
#if MIN_VERSION_transformers(0,2,0)
import "transformers" Control.Monad.Trans.Class
import "transformers" Control.Monad.IO.Class
#else
import "transformers" Control.Monad.Trans
#endif
import "transformers" Control.Monad.Trans.State
import Control.Monad

#if TEST
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
--import Test.Framework.Providers.QuickCheck (testProperty)
import Test.HUnit hiding (Test, path)
--import Test.QuickCheck
import qualified Data.ByteString.Char8 as B8
#endif

-- | Equality depends on 'value' and 'tag', not 'style'.
data YamlScalar = YamlScalar
    { value :: ByteString
    , tag :: Tag
    , style :: Style
    }
    deriving (Show)
instance Eq YamlScalar where
    (YamlScalar v t _) == (YamlScalar v' t' _) = v == v' && t == t'

type YamlObject = Object YamlScalar YamlScalar

class IsYamlScalar a where
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
instance IsYamlScalar ByteString where
    fromYamlScalar = value
    toYamlScalar b = YamlScalar b NoTag Any

encode :: (IsYamlScalar k, IsYamlScalar v) => Object k v -> ByteString
encode = unsafePerformIO . Y.encode . ge

encodeFile :: (IsYamlScalar k, IsYamlScalar v, MonadFailure YamlException m,
               With m)
           => FilePath
           -> Object k v
           -> m ()
encodeFile fp = Y.encodeFile fp . ge

emitEvents :: (MonadIO m, MonadFailure YamlException m)
           => Event -> Event -> YamlEncoder m () -> YamlEncoder m ()
emitEvents start stop body = emitEvent start >> body >> emitEvent stop

ge :: (MonadIO m, MonadFailure YamlException m, IsYamlScalar k,
       IsYamlScalar v)
   => Object k v
   -> YamlEncoder m ()
ge yo = emitEvents EventStreamStart EventStreamEnd
      $ emitEvents EventDocumentStart EventDocumentEnd
      $ geO yo

geO :: (MonadIO m, MonadFailure YamlException m, IsYamlScalar k,
        IsYamlScalar v)
    => Object k v
    -> YamlEncoder m ()
geO (Scalar s) = geS s
geO (Sequence yos)  = emitEvents (EventSequenceStart Nothing) EventSequenceEnd
                    $ mapM_ geO yos
geO (Mapping pairs) = emitEvents (EventMappingStart Nothing) EventMappingEnd
                    $ mapM_ gePair pairs

gePair :: (MonadIO m, MonadFailure YamlException m, IsYamlScalar k,
           IsYamlScalar v)
       => (k, Object k v)
       -> YamlEncoder m ()
gePair (ys, yo) = geS ys >> geO yo

geS :: (MonadIO m, IsYamlScalar a, MonadFailure YamlException m)
    => a
    -> YamlEncoder m ()
geS = emitEvent . toEventScalar . toYamlScalar

toEventScalar :: YamlScalar -> Event
toEventScalar (YamlScalar v t s) = EventScalar v t s Nothing

decode :: (MonadFailure YamlException m, IsYamlScalar k, IsYamlScalar v)
       => ByteString
       -> m (Object k v)
decode bs = try $ unsafePerformIO $ unYAttemptIO $ Y.decode bs parse

newtype YAttemptIO v = YAttemptIO
    { unYAttemptIO :: IO (Either YamlException v)
    }
instance Monad YAttemptIO where
    return = YAttemptIO . return . Right
    (YAttemptIO io) >>= f = YAttemptIO $ do
        x <- io
        case x of
            Left e -> return $ Left e
            Right y -> unYAttemptIO $ f y
instance Functor YAttemptIO where
    fmap = liftM
instance Applicative YAttemptIO where
    pure = return
    (<*>) = ap
instance Failure YamlException YAttemptIO where
    failure = YAttemptIO . return . Left
instance MonadIO YAttemptIO where
    liftIO = YAttemptIO . fmap Right
instance With YAttemptIO where
    with orig = YAttemptIO . orig . (unYAttemptIO .)

decodeFile :: (MonadFailure YamlException m, IsYamlScalar k, IsYamlScalar v,
               With m)
           => FilePath
           -> m (Object k v)
decodeFile fp = Y.decodeFile fp parse

requireEvent :: (With m, MonadFailure YamlException m)
             => Event
             -> YamlDecoder m ()
requireEvent e = do
    e' <- parseEvent
    unless (e == e')
        $ failure $ YamlOtherException $ SomeException
        $ UnexpectedEvent e' $ Just e

data UnexpectedEvent = UnexpectedEvent
    { _received :: Event
    , _expected :: Maybe Event
    }
    deriving (Show, Typeable)
instance Exception UnexpectedEvent

type Parser s m = StateT (Map.Map String s) (YamlDecoder m)

parse :: (With m, MonadFailure YamlException m, IsYamlScalar k,
          IsYamlScalar v)
      => YamlDecoder m (Object k v)
parse = do
    requireEvent EventStreamStart
    requireEvent EventDocumentStart
    e <- parseEvent
    res <- evalStateT (parseO e) Map.empty
    requireEvent EventDocumentEnd
    requireEvent EventStreamEnd
    requireEvent EventNone
    return res

parseO :: (IsYamlScalar k, IsYamlScalar v, With m,
           MonadFailure YamlException m)
       => Event
       -> Parser (Object k v) m (Object k v)
parseO (EventScalar v t s a) = do
    let res = Scalar $ fromYamlScalar $ YamlScalar v t s
    case a of
        Nothing -> return res
        Just an -> do
            modify (Map.insert an res)
            return res
parseO (EventSequenceStart a) = parseS a id
parseO (EventMappingStart a) = parseM a id
parseO (EventAlias an) = do
    m <- get
    case Map.lookup an m of
        Nothing -> failure $ YamlOtherException $ SomeException $ UnknownAlias an
        Just v -> return v
parseO e = failure $ YamlOtherException $ SomeException
                   $ UnexpectedEvent e Nothing

parseS :: (IsYamlScalar k, IsYamlScalar v, With m,
           MonadFailure YamlException m)
       => Y.Anchor
       -> ([Object k v] -> [Object k v])
       -> Parser (Object k v) m (Object k v)
parseS a front = do
    e <- lift $ parseEvent
    case e of
        EventSequenceEnd -> do
            let res = Sequence $ front []
            case a of
                Nothing -> return res
                Just an -> do
                    modify (Map.insert an res)
                    return res
        _ -> do
            o <- parseO e
            parseS a $ front . (:) o

parseM :: (IsYamlScalar k, IsYamlScalar v, With m,
           MonadFailure YamlException m)
       => Y.Anchor
       -> ([(k, Object k v)] -> [(k, Object k v)])
       -> Parser (Object k v) m (Object k v)
parseM a front = do
    e <- lift $ parseEvent
    case e of
        EventMappingEnd -> do
            let res = Mapping $ front []
            case a of
                Nothing -> return res
                Just an -> do
                    modify (Map.insert an res)
                    return res
        EventScalar v' t s Nothing -> do
            let k = fromYamlScalar $ YamlScalar v' t s
            v <- (lift $ parseEvent) >>= parseO
            parseM a $ front . (:) (k, v)
        _ -> failure $ YamlOtherException
                     $ SomeException NonScalarKey

data ParseException = NonScalarKey
                    | UnknownAlias { _anchorName :: Y.AnchorName }
    deriving (Show, Typeable)
instance Exception ParseException

#if TEST
mkScalar :: String -> YamlScalar
mkScalar s = YamlScalar (cs s) StrTag Folded

sample :: YamlObject
sample = Sequence
    [ Scalar $ mkScalar "foo"
    , Mapping
        [ (mkScalar "bar1", Scalar $ mkScalar "bar2")
        ]
    ]

sampleStr :: Object String String
sampleStr = mapKeysValues fromYamlScalar fromYamlScalar sample

testSuite :: Test
testSuite = testGroup "Data.Object.Yaml"
    [ testCase "encode/decode" caseEncodeDecode
    , testCase "encode/decode file" caseEncodeDecodeFile
    , testCase "encode/decode strings" caseEncodeDecodeStrings
    , testCase "decode invalid file" caseDecodeInvalid
    ]

caseEncodeDecode :: Assertion
caseEncodeDecode = do
    out <- decode $ encode sample
    out @?= sample

caseEncodeDecodeFile :: Assertion
caseEncodeDecodeFile = do
    let fp = "tmp.yaml"
    encodeFile fp sample
    out <- decodeFile fp
    out @?= sample

caseEncodeDecodeStrings :: Assertion
caseEncodeDecodeStrings = do
    out <- decode $ encode sampleStr
    out @?= sampleStr

caseDecodeInvalid :: Assertion
caseDecodeInvalid = do
    let invalid = B8.pack "\tthis is 'not' valid :-)"
    Nothing @=? (decode invalid :: Maybe YamlObject)

#endif
