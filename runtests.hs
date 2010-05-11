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
mkScalar = toYamlScalar

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
    let maybeRes = decode yamlBS :: Maybe YamlObject
    isJust maybeRes @? "decoder should return Just YamlObject but returned Nothing"
    let res = fromJust maybeRes
    res @?= Sequence [Scalar (mkScalar "foo"), Scalar (mkScalar "baz"), Scalar (mkScalar "foo")]
    where yamlString = "- &anch foo\n- baz\n- *anch"
          yamlBS = B8.pack yamlString

caseSimpleSequenceAlias :: Assertion
caseSimpleSequenceAlias = do
    let maybeRes = decode yamlBS :: Maybe YamlObject
    isJust maybeRes @? "decoder should return Just YamlObject but returned Nothing"
    let res = fromJust maybeRes
    res @?= Mapping [(mkScalar "seq", Sequence [Scalar (mkScalar "foo"), Scalar (mkScalar "baz")]), (mkScalar "seq2", Sequence [Scalar (mkScalar "foo"), Scalar (mkScalar "baz")])]
    where yamlString = "seq: &anch\n  - foo\n  - baz\nseq2: *anch"
          yamlBS = B8.pack yamlString

caseSimpleMappingAlias :: Assertion
caseSimpleMappingAlias = do
    let maybeRes = decode yamlBS :: Maybe YamlObject
    isJust maybeRes @? "decoder should return Just YamlObject but returned Nothing"
    let res = fromJust maybeRes
    res @?= Mapping [(mkScalar "map", Mapping [(mkScalar "key1", Scalar (mkScalar "foo")), (mkScalar "key2", Scalar (mkScalar "baz"))]), (mkScalar "map2", Mapping [(mkScalar "key1", Scalar (mkScalar "foo")), (mkScalar "key2", Scalar (mkScalar "baz"))])]
    where yamlString = "map: &anch\n  key1: foo\n  key2: baz\nmap2: *anch"
          yamlBS = B8.pack yamlString

caseMappingAliasBeforeAnchor :: Assertion
caseMappingAliasBeforeAnchor = do
    let res = decode yamlBS :: Maybe YamlObject
    isNothing res @? "decode should return Nothing due to unknown alias"
    where yamlString = "map: *anch\nmap2: &anch\n  key1: foo\n  key2: baz"
          yamlBS = B8.pack yamlString

caseMappingAliasInsideAnchor :: Assertion
caseMappingAliasInsideAnchor = do
    let res = decode yamlBS :: Maybe YamlObject
    isNothing res @? "decode should return Nothing due to unknown alias"
    where yamlString = "map: &anch\n  key1: foo\n  key2: *anch"
          yamlBS = B8.pack yamlString

caseScalarAliasOverriding :: Assertion
caseScalarAliasOverriding = do
    let maybeRes = decode yamlBS :: Maybe YamlObject
    isJust maybeRes @? "decoder should return Just YamlObject but returned Nothing"
    let res = fromJust maybeRes
    res @?= Sequence [Scalar (mkScalar "foo"), Scalar (mkScalar "baz"), Scalar (mkScalar "foo"), Scalar (mkScalar "boo"), Scalar (mkScalar "buz"), Scalar (mkScalar "boo")]
    where yamlString = "- &anch foo\n- baz\n- *anch\n- &anch boo\n- buz\n- *anch"
          yamlBS = B8.pack yamlString
