module TestScanner where

import Scanner
import Test.HUnit

tests :: Test
tests = TestList [ TestLabel "SimpleTests" 
                    testSimpleText,
                    testSingleTab,
                    testAsterisk,
                    testEmptyLine
                ]


-- Einfacher Text soll zu T_Text String umgewandelt werden
testSimpleText :: Test
testSimpleText =
    let expr = "blablabla"
        expectedValue = Just [ T_Text "blablabla"]
    in TestCase (assertEqual expr expectedValue $ scan expr )


-- Text mit Leerzeichen dazwischen soll zu "T_Text String T_Space T_Text String" umgewandelt werden
testTextWithSpaces :: Test
testTextWithSpaces =
    let expr = "bla bla bla"
        expectedValue = Just [ T_Text "bla" T_Space T_Text "bla" T_Space T_Text "bla"]
    in TestCase (assertEqual expr expectedValue $ scan expr )

 
testEmptyLine :: Test
testEmptyLine =
    let expr = "\n\n"
        expectedValue = Just [ T_EmptyLine{-
                                , FloatNum 1.2
                                , Add
                                , Ident "abc"
                                , Mult
                                , Ident "__2"
                                , Add
                                , NatNum 23-}
                                ]
    in TestCase (assertEqual expr expectedValue $ scan expr )


-- Erkennung einzelnes Tabulatorzeichen und Umwandlung in Token
testSingleTab :: Test
testSingleTab =
    let expr = "    "
        expectedValue = Just [ T_Tab 1{-
                                , FloatNum 1.2
                                , Add
                                , Ident "abc"
                                , Mult
                                , Ident "__2"
                                , Add
                                , NatNum 23-}
                                ]
    in TestCase (assertEqual expr expectedValue $ scan expr )

-- Erkennung eines Sternzeichen und Umwandlung in Token
testAsterisk :: Test
testAsterisk =
    let expr = "*"
        expectedValue = Just [ T_Asterisk ]
    in TestCase (assertEqual expr expectedValue $ scan expr )

    

main :: IO ()
main = do
        runTestTT tests
        return ()