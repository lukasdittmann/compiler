module TestScanner where

import Scanner
import Test.HUnit

tests :: Test
tests = TestList [ TestLabel "SimpleTests" 
                    testSimpleText,
                    testTextWithSpaces,
                    testSingleTab,
                    testAsterisk,
                    testEmptyLine,
                    testEmptyLineInclWhitespaces
                ]


-- Einfacher Text soll zu T_Text String umgewandelt werden
testSimpleText :: Test
testSimpleText =
    let expr = "blablabla"
        expectedValue = Just [ T_Text "blablabla"]
    in TestCase (assertEqual expr expectedValue $ scan expr )


-- Text mit Leerzeichen dazwischen soll zu "T_Text String" umgewandelt werden
testTextWithSpaces :: Test
testTextWithSpaces =
    let expr = "bla bla bla"
        expectedValue = Just [ T_Text "bla bla bla" ]
    in TestCase (assertEqual expr expectedValue $ scan expr )


testEmptyLine :: Test
testEmptyLine =
    let expr = "\n\n"
        expectedValue = Just [ T_EmptyLine ]
    in TestCase (assertEqual expr expectedValue $ scan expr )


testEmptyLineInclWhitespaces :: Test
testEmptyLineInclWhitespaces =
    let expr = "\n \n"
        expectedValue = Just [ T_EmptyLine ]
    in TestCase (assertEqual expr expectedValue $ scan expr )


-- Erkennung einzelnes Tabulatorzeichen und Umwandlung in Token
testSingleTab :: Test
testSingleTab =
    let expr = "    "
        expectedValue = Just [ T_Tab 1 ]
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