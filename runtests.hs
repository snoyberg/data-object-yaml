{-# LANGUAGE NamedFieldPuns #-}
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Data.Maybe
import qualified Data.ByteString.Char8 as B8

import Data.Object
import qualified Data.Object.Yaml as Y

ys :: (Y.IsYamlScalar a) => a -> Y.YamlScalar
ys = Y.toYamlScalar

main :: IO ()
main = defaultMain
    [ Y.testSuite
    , testSuite
    ]

testSuite :: Test
testSuite = testGroup "Tests using samples"
    [ {- FIXME
      testCase "simple scalar alias" caseSimpleScalarAlias
    , testCase "simple sequence alias" caseSimpleSequenceAlias
    , testCase "simple mapping alias" caseSimpleMappingAlias
    , testCase "mapping alias before anchor" caseMappingAliasBeforeAnchor
    , testCase "mapping alias inside anchor" caseMappingAliasInsideAnchor
    , testCase "scalar alias overriding" caseScalarAliasOverriding
      -}
    ]

{- FIXME
caseSimpleScalarAlias :: Assertion
caseSimpleScalarAlias = do
    let maybeRes = Y.decode yamlBS :: Maybe (Y.YamlObject)
    isJust maybeRes @? "decoder should return Just YamlObject but returned Nothing"
    let res = fromJust maybeRes
    res @?= Sequence [Scalar (ys "foo"), Scalar (ys "baz"), Scalar (ys "foo")]
    where yamlString = "- &anch foo\n- baz\n- *anch"
          yamlBS = B8.pack yamlString

caseSimpleSequenceAlias :: Assertion
caseSimpleSequenceAlias = do
    let maybeRes = Y.decode yamlBS :: Maybe (Y.YamlObject)
    isJust maybeRes @? "decoder should return Just YamlObject but returned Nothing"
    let res = fromJust maybeRes
    res @?= Mapping [(ys "seq", Sequence [Scalar (ys "foo"), Scalar (ys "baz")]), (ys "seq2", Sequence [Scalar (ys "foo"), Scalar (ys "baz")])]
    where yamlString = "seq: &anch\n  - foo\n  - baz\nseq2: *anch"
          yamlBS = B8.pack yamlString

caseSimpleMappingAlias :: Assertion
caseSimpleMappingAlias = do
    let maybeRes = Y.decode yamlBS :: Maybe (Y.YamlObject)
    isJust maybeRes @? "decoder should return Just YamlObject but returned Nothing"
    let res = fromJust maybeRes
    res @?= Mapping [(ys "map", Mapping [(ys "key1", Scalar (ys "foo")), (ys "key2", Scalar (ys "baz"))]), (ys "map2", Mapping [(ys "key1", Scalar (ys "foo")), (ys "key2", Scalar (ys "baz"))])]
    where yamlString = "map: &anch\n  key1: foo\n  key2: baz\nmap2: *anch"
          yamlBS = B8.pack yamlString

caseMappingAliasBeforeAnchor :: Assertion
caseMappingAliasBeforeAnchor = do
    let res = Y.decode yamlBS :: Maybe (Y.YamlObject)
    isNothing res @? "decode should return Nothing due to unknown alias"
    where yamlString = "map: *anch\nmap2: &anch\n  key1: foo\n  key2: baz"
          yamlBS = B8.pack yamlString

caseMappingAliasInsideAnchor :: Assertion
caseMappingAliasInsideAnchor = do
    let res = Y.decode yamlBS :: Maybe (Y.YamlObject)
    isNothing res @? "decode should return Nothing due to unknown alias"
    where yamlString = "map: &anch\n  key1: foo\n  key2: *anch"
          yamlBS = B8.pack yamlString

caseScalarAliasOverriding :: Assertion
caseScalarAliasOverriding = do
    let maybeRes = Y.decode yamlBS :: Maybe (Y.YamlObject)
    isJust maybeRes @? "decoder should return Just YamlObject but returned Nothing"
    let res = fromJust maybeRes
    res @?= Sequence [Scalar (ys "foo"), Scalar (ys "baz"), Scalar (ys "foo"), Scalar (ys "boo"), Scalar (ys "buz"), Scalar (ys "boo")]
    where yamlString = "- &anch foo\n- baz\n- *anch\n- &anch boo\n- buz\n- *anch"
          yamlBS = B8.pack yamlString
-}
