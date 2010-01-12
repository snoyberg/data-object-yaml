import Data.Object.Yaml
import Data.Object
import Control.Monad
import qualified Text.Libyaml as Y
import Control.Failure
import Control.Applicative
import Debug.Trace
import System.Environment

main = do
    args <- getArgs
    case args of
        ["easy"] -> mainEasy
        ["efficient"] -> mainEfficient
        _ -> putStrLn "Please specify either eays or efficient"

mainEasy = do
    yo <- join $ decodeFile "entries.yaml"
    s <- fromSequence (yo :: YamlObject)
    print $ length s

mainEfficient = do
    l <- fmap stateComplete $ join $ try <$> Y.decodeFile "entries.yaml" helper StateNeedStream
    print l

data Ignore = Seq | Map
    deriving Show

data State = StateNeedStream | StateNeedDoc | StateNeedSeq
           | StateCount Int | StateIgnore [Ignore] Int
           | StateComplete { stateComplete :: Int }
    deriving Show
helper' s e = traceShow (s, e) $ helper s e

helper StateNeedStream Y.EventStreamStart = Right StateNeedDoc
helper StateNeedDoc Y.EventDocumentStart = Right StateNeedSeq
helper StateNeedSeq Y.EventSequenceStart = Right $ StateCount 0
helper (StateCount i) Y.EventSequenceEnd = Left $ StateComplete i
helper (StateCount i) (Y.EventScalar{}) = Right $ StateCount $ i + 1
helper (StateCount i) Y.EventSequenceStart =
    Right $ StateIgnore [Seq] $ i + 1
helper (StateCount i) Y.EventMappingStart =
    Right $ StateIgnore [Map] $ i + 1
helper (StateIgnore [] i) e = helper (StateCount i) e
helper (StateIgnore is i) Y.EventSequenceStart =
    Right $ StateIgnore (Seq : is) i
helper (StateIgnore is i) Y.EventMappingStart =
    Right $ StateIgnore (Map : is) i
helper (StateIgnore (Seq:is) i) Y.EventSequenceEnd =
    Right $ StateIgnore is i
helper (StateIgnore (Map:is) i) Y.EventMappingEnd =
    Right $ StateIgnore is i
helper (StateIgnore is i) _ = Right $ StateIgnore is i
