{-# LANGUAGE NamedFieldPuns #-}
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Data.Maybe
import qualified Data.ByteString.Char8 as B8

import qualified Text.Libyaml as LY

import Data.Object
import Data.Object.Yaml

import Control.Monad

mkFoldedScalar :: String -> YamlScalar
mkFoldedScalar s = YamlScalar (cs s) LY.StrTag LY.Folded

mkScalar :: String -> YamlScalar
mkScalar s = YamlScalar (cs s) LY.NoTag LY.Plain

mkStrScalar :: String -> YamlScalar
mkStrScalar s = YamlScalar (cs s) LY.StrTag LY.Plain

mappingKey :: YamlObject -> String -> YamlObject
mappingKey (Mapping m) k = (fromJust . lookup (mkScalar k) $ m)
mappingKey _ _ = error "expected Mapping"

decodeYaml :: String -> Maybe YamlObject
decodeYaml s = decode $ B8.pack s

sample :: YamlObject
sample = Sequence
    [ Scalar $ mkFoldedScalar "foo"
    , Mapping
        [ (mkFoldedScalar "bar1", Scalar $ mkFoldedScalar "bar2")
        ]
    ]

sampleStr :: Object String String
sampleStr = mapKeysValues fromYamlScalar fromYamlScalar sample

main :: IO ()
main = defaultMain
    [ testSuite
    , testSuiteOfAliases
    , testSuiteOfMergeKeys
    ]

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
    out <- join $ decodeFile fp
    out @?= sample

caseEncodeDecodeStrings :: Assertion
caseEncodeDecodeStrings = do
    out <- decode $ encode $ toYamlObject sampleStr
    fromYamlObject out @?= sampleStr

caseDecodeInvalid :: Assertion
caseDecodeInvalid = do
    let invalid = B8.pack "\tthis is 'not' valid :-)"
    Nothing @=? (decode invalid :: Maybe YamlObject)


testSuiteOfAliases :: Test
testSuiteOfAliases = testGroup "Tests of aliases"
    [ testCase "simple scalar alias" caseSimpleScalarAlias
    , testCase "simple sequence alias" caseSimpleSequenceAlias
    , testCase "simple mapping alias" caseSimpleMappingAlias
    , testCase "mapping alias before anchor" caseMappingAliasBeforeAnchor
    , testCase "mapping alias inside anchor" caseMappingAliasInsideAnchor
    , testCase "scalar alias overriding" caseScalarAliasOverriding
    ]

caseSimpleScalarAlias :: Assertion
caseSimpleScalarAlias = do
    let maybeRes = decodeYaml "- &anch foo\n- baz\n- *anch"
    isJust maybeRes @? "decoder should return Just YamlObject but returned Nothing"
    let res = fromJust maybeRes
    res @?= Sequence [Scalar (mkScalar "foo"), Scalar (mkScalar "baz"), Scalar (mkScalar "foo")]

caseSimpleSequenceAlias :: Assertion
caseSimpleSequenceAlias = do
    let maybeRes = decodeYaml "seq: &anch\n  - foo\n  - baz\nseq2: *anch"
    isJust maybeRes @? "decoder should return Just YamlObject but returned Nothing"
    let res = fromJust maybeRes
    res @?= Mapping [(mkScalar "seq", Sequence [Scalar (mkScalar "foo"), Scalar (mkScalar "baz")]), (mkScalar "seq2", Sequence [Scalar (mkScalar "foo"), Scalar (mkScalar "baz")])]

caseSimpleMappingAlias :: Assertion
caseSimpleMappingAlias = do
    let maybeRes = decodeYaml "map: &anch\n  key1: foo\n  key2: baz\nmap2: *anch"
    isJust maybeRes @? "decoder should return Just YamlObject but returned Nothing"
    let res = fromJust maybeRes
    res @?= Mapping [(mkScalar "map", Mapping [(mkScalar "key1", Scalar (mkScalar "foo")), (mkScalar "key2", Scalar (mkScalar "baz"))]), (mkScalar "map2", Mapping [(mkScalar "key1", Scalar (mkScalar "foo")), (mkScalar "key2", Scalar (mkScalar "baz"))])]

caseMappingAliasBeforeAnchor :: Assertion
caseMappingAliasBeforeAnchor = do
    let res = decodeYaml "map: *anch\nmap2: &anch\n  key1: foo\n  key2: baz"
    isNothing res @? "decode should return Nothing due to unknown alias"

caseMappingAliasInsideAnchor :: Assertion
caseMappingAliasInsideAnchor = do
    let res = decodeYaml "map: &anch\n  key1: foo\n  key2: *anch"
    isNothing res @? "decode should return Nothing due to unknown alias"

caseScalarAliasOverriding :: Assertion
caseScalarAliasOverriding = do
    let maybeRes = decodeYaml "- &anch foo\n- baz\n- *anch\n- &anch boo\n- buz\n- *anch"
    isJust maybeRes @? "decoder should return Just YamlObject but returned Nothing"
    let res = fromJust maybeRes
    res @?= Sequence [Scalar (mkScalar "foo"), Scalar (mkScalar "baz"), Scalar (mkScalar "foo"), Scalar (mkScalar "boo"), Scalar (mkScalar "buz"), Scalar (mkScalar "boo")]


testSuiteOfMergeKeys :: Test
testSuiteOfMergeKeys = testGroup "Tests of 'merge keys' feature"
        [ testCase "test uniqueness of keys" caseAllKeysShouldBeUnique
        , testCase "test mapping merge" caseSimpleMappingMerge
        , testCase "test sequence of mappings merging" caseMergeSequence
        ]

caseAllKeysShouldBeUnique :: Assertion
caseAllKeysShouldBeUnique = do
    let maybeRes = decodeYaml "foo1: foo\nfoo2: baz\nfoo1: buz"
    isJust maybeRes @? "decoder should return Just YamlObject but returned Nothing"
    let res = fromJust maybeRes
    mappingKey res "foo1" @?= Scalar (mkScalar "buz")

caseSimpleMappingMerge :: Assertion
caseSimpleMappingMerge = do
    let maybeRes = decodeYaml "foo1: foo\nfoo2: baz\n<<:\n  foo1: buz\n  foo3: fuz"
    isJust maybeRes @? "decoder should return Just YamlObject but returned Nothing"
    let res = fromJust maybeRes
    mappingKey res "foo1" @?= Scalar (mkScalar "foo")
    mappingKey res "foo3" @?= Scalar (mkScalar "fuz")

caseMergeSequence :: Assertion
caseMergeSequence = do
    let maybeRes = decodeYaml "m1: &m1\n  k1: !!str 1\n  k2: !!str 2\nm2: &m2\n  k1: !!str 3\n  k3: !!str 4\nfoo1: foo\n<<: [ *m1, *m2 ]"
    isJust maybeRes @? "decoder should return Just YamlObject but returned Nothing"
    let res = fromJust maybeRes
    mappingKey res "foo1" @?= Scalar (mkScalar "foo")
    mappingKey res "k1" @?= Scalar (mkStrScalar "1")
    mappingKey res "k2" @?= Scalar (mkStrScalar "2")
    mappingKey res "k3" @?= Scalar (mkStrScalar "4")
