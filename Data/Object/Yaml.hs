{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (throw, Exception, SomeException (..))
import Data.Typeable (Typeable)
-- debugging purposes import Debug.Trace
import Control.Failure
import Control.Applicative ((<$>))
import qualified Data.Text

#if TEST
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
--import Test.Framework.Providers.QuickCheck (testProperty)
import Test.HUnit hiding (Test, path)
--import Test.QuickCheck

import Control.Monad (join)
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
instance IsYamlScalar [Char] where
    fromYamlScalar = cs . value
    toYamlScalar s = YamlScalar (cs s) NoTag Any
instance IsYamlScalar ByteString where
    fromYamlScalar = value
    toYamlScalar b = YamlScalar b NoTag Any

encode :: (IsYamlScalar k, IsYamlScalar v) => Object k v -> ByteString
encode yo = either throw id $ unsafePerformIO $ Y.encode ge $ Phase1
          $ mapKeysValues toYamlScalar toYamlScalar yo

encodeFile :: (IsYamlScalar k, IsYamlScalar v, MonadFailure YamlException m)
           => FilePath
           -> Object k v
           -> IO (m ())
encodeFile fp yo =
    try <$> (Y.encodeFile fp ge $ Phase1
           $ mapKeysValues toYamlScalar toYamlScalar yo)

data GenState = Phase1 YamlObject
              | Phase2 YamlObject
              | Phase3 YamlObject GenState
              | Phase4
              | Phase5
              | Phase6
              | PhaseSeq [YamlObject] GenState
              | PhaseMap [(YamlScalar, YamlObject)] GenState
              | PhaseMap' YamlObject [(YamlScalar, YamlObject)] GenState
    deriving (Eq, Show)

{- Debugging purposes
ge' gs =
    let res = ge gs
     in traceShow res res
-}
ge :: GenState -> Maybe (Event, GenState)
ge (Phase1 yo) = Just (EventStreamStart, Phase2 yo)
ge (Phase2 yo) = Just (EventDocumentStart, Phase3 yo Phase4)
ge (Phase3 (Scalar (YamlScalar v t s)) n) = Just (EventScalar v t s, n)
ge (Phase3 (Sequence yos) n) = Just (EventSequenceStart, PhaseSeq yos n)
ge (Phase3 (Mapping pairs) n) = Just (EventMappingStart, PhaseMap pairs n)
ge Phase4 = Just (EventDocumentEnd, Phase5)
ge Phase5 = Just (EventStreamEnd, Phase6)
ge Phase6 = Nothing
ge (PhaseSeq [] n) = Just (EventSequenceEnd, n)
ge (PhaseSeq (yo:yos) n) = ge $ Phase3 yo $ PhaseSeq yos n
ge (PhaseMap [] n) = Just (EventMappingEnd, n)
ge (PhaseMap ((k, v):pairs) n) = ge $ Phase3 (Scalar k) $ PhaseMap' v pairs n
ge (PhaseMap' v pairs n) = ge $ Phase3 v $ PhaseMap pairs n

decode :: (MonadFailure YamlException m, IsYamlScalar k, IsYamlScalar v)
       => ByteString
       -> m (Object k v)
decode bs =
    mapKeysValues fromYamlScalar fromYamlScalar
    <$> ((try $ unsafePerformIO $ Y.decode bs pf Parse1)
    >>= unParseComplete)

decodeFile :: (MonadFailure YamlException m, IsYamlScalar k, IsYamlScalar v)
           => FilePath
           -> IO (m (Object k v))
decodeFile fp = do
    res <- Y.decodeFile fp pf Parse1
    let res' = try res
    return $ mapKeysValues fromYamlScalar fromYamlScalar
          <$> (res' >>= unParseComplete)

data ParseState =
    Parse1
  | Parse2
  | Parse3 (YamlObject -> ParseState)
  | Parse4 YamlObject
  | Parse5 YamlObject
  | ParseComplete (Either YamlException YamlObject)
  | ParseSeq (YamlObject -> ParseState) ([YamlObject] -> [YamlObject])
  | ParseMap (YamlObject -> ParseState)
             ([(YamlScalar, YamlObject)] -> [(YamlScalar, YamlObject)])
  | ParseMap' (YamlObject -> ParseState)
              ([(YamlScalar, YamlObject)] -> [(YamlScalar, YamlObject)])
              YamlScalar
  | ParseException ParseException

unParseComplete :: MonadFailure YamlException m
                => ParseState
                -> m YamlObject
unParseComplete (ParseComplete (Left e)) = failure e
unParseComplete (ParseComplete (Right yo)) = return yo
unParseComplete p = failure $ YamlOtherException $ SomeException
                  $ IncompleteParse p

instance Show ParseState where
    show Parse1{} = "Parse1"
    show Parse2{} = "Parse2"
    show Parse3{} = "Parse3"
    show Parse4{} = "Parse4"
    show Parse5{} = "Parse5"
    show ParseComplete{} = "ParseComplete"
    show ParseSeq{} = "ParseSeq"
    show ParseMap{} = "ParseMap"
    show ParseMap'{} = "ParseMap'"
    show (ParseException e) = "ParseException " ++ show e

-- debugging purposes only pf' p e = traceShow (p, e) $ pf p e

pf :: ParseState -> Event -> Either ParseState ParseState
pf Parse1 EventStreamStart = Right Parse2
pf Parse2 EventDocumentStart = Right $ Parse3 Parse4
pf (Parse3 n) (EventScalar v t s) = Right $ n $ Scalar $ YamlScalar v t s
pf (Parse3 n) EventSequenceStart = Right $ ParseSeq n id
pf (Parse3 n) EventMappingStart = Right $ ParseMap n id
pf (Parse4 yo) EventDocumentEnd = Right $ Parse5 yo
pf (Parse5 yo) EventStreamEnd = Left $ ParseComplete $ Right yo
pf (ParseSeq n front) EventSequenceEnd = Right $ n $ Sequence $ front []
pf (ParseSeq n front) e = pf (Parse3 helper) e where
    helper yo = ParseSeq n $ front . (:) yo
pf (ParseMap n front) EventMappingEnd = Right $ n $ Mapping $ front []
pf (ParseMap n front) e = pf (Parse3 helper) e where
    helper (Scalar ys) = ParseMap' n front ys
    helper _ = ParseException NonScalarKey
pf (ParseMap' n front ys) e = pf (Parse3 helper) e where
    helper yo = ParseMap n $ front . (:) (ys, yo)
pf Parse2 EventStreamEnd =
    Left $ ParseComplete $ Left YamlPrematureEventStreamEnd
pf p e =
    Left $ ParseComplete $ Left $ YamlOtherException
         $ SomeException $ InvalidParseState p e

data ParseException = InvalidParseState ParseState Event
                    | NonScalarKey
                    | IncompleteParse ParseState
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
    ]

caseEncodeDecode :: Assertion
caseEncodeDecode = do
    out <- decode $ encode sample
    out @?= sample

caseEncodeDecodeFile :: Assertion
caseEncodeDecodeFile = do
    let fp = "tmp.yaml"
    join $ encodeFile fp sample
    out <- join $ decodeFile fp
    out @?= sample

caseEncodeDecodeStrings :: Assertion
caseEncodeDecodeStrings = do
    out <- decode $ encode sampleStr
    out @?= sampleStr

#endif
