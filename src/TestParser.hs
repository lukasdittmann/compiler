module TestParser where

import Parser

import Scanner

import IR

import Test.HUnit

import Data.Map as M

tests :: Test
tests = TestList [ TestLabel "SimpleTests" 
                    testSimpleText,
                    testSimpleUL,
                    testSimpleOL,
                    testSimpleKursiv,
                    testSimpleBold,
                    testSimpleImage
                ]


testSimpleText :: Test
testSimpleText =
    let expr = [T_Text "blablabla"]
        expectedValue = Just [Sequence [Absatz 0 (Sequence [Text "blablabla "])]]
        linkList = M.empty
    in case (parse expr linkList) of
        Just elems -> TestCase (assertEqual (show elems) expectedValue $ Just [elems])
        Nothing -> TestCase (assertFailure "False")

testSimpleUL :: Test
testSimpleUL =
    let expr = [T_Asterisk, T_Space 1, T_Text "Test"]
        expectedValue = Just [Sequence [Absatz 0 (Sequence [UL [LiE [Sequence [Text "  Test "]]]])]]
        linkList = M.empty
    in case (parse expr linkList) of
        Just elems -> TestCase (assertEqual (show elems) expectedValue $ Just [elems])
        Nothing -> TestCase (assertFailure "False")

testSimpleOL :: Test
testSimpleOL =
    let expr = [T_SLI, T_Space 1,T_Text "Test"]
        expectedValue = Just [Sequence [Absatz 0 (Sequence [OL [LiE [Sequence [Text "  Test "]]]])]]
        linkList = M.empty
    in case (parse expr linkList) of
        Just elems -> TestCase (assertEqual (show elems) expectedValue $ Just [elems])
        Nothing -> TestCase (assertFailure "False")

testSimpleKursiv :: Test
testSimpleKursiv =
    let expr = [T_Asterisk, T_Text "Test", T_Asterisk]
        expectedValue = Just [Sequence [Absatz 0 (Sequence [Kursiv [Text "Test"]])]]
        linkList = M.empty
    in case (parse expr linkList) of
        Just elems -> TestCase (assertEqual (show elems) expectedValue $ Just [elems])
        Nothing -> TestCase (assertFailure "False")

testSimpleBold :: Test
testSimpleBold =
    let expr = [T_Asterisk, T_Asterisk, T_Text "Test", T_Asterisk, T_Asterisk]
        expectedValue = Just [Sequence [Absatz 0 (Sequence [Bold [Text "Test"]])]]
        linkList = M.empty
    in case (parse expr linkList) of
        Just elems -> TestCase (assertEqual (show elems) expectedValue $ Just [elems])
        Nothing -> TestCase (assertFailure "False")

testSimpleImage :: Test
testSimpleImage =
    let expr = [T_Image "http://url"]
        expectedValue = Just [Sequence [Absatz 0 (Sequence [Image "http://url"])]]
        linkList = M.empty
    in case (parse expr linkList) of
        Just elems -> TestCase (assertEqual (show elems) expectedValue $ Just [elems])
        Nothing -> TestCase (assertFailure "False")

main :: IO ()
main = do
        runTestTT tests
        return ()
