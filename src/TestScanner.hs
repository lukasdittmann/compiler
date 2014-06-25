module TestScanner where

import Scanner
import Test.HUnit

tests :: Test
tests = TestList [ TestLabel "SimpleTests" 
                    testSimpleText,
                    testTextWithSpaces,
                    testSimpleNewline,
                    testEmbeddedNewline,
                    testEmptyLine,
                    testEmptyLineInclWhitespaces,
                    testEmptyLineWithWSandSuccessingWSinNextLine,
                    testSingleTab,
                    testMultipleTab,
                    testTabWithSpaces,
                    testAsterisk,
                    testUnderscore,
                    --testRefLinkDef,
                    testRefLink,
                    testRefTextToken,
                    testHLinkWithRefText,
                    testEmbeddedHLink,
                    testImage
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

testSimpleNewline :: Test
testSimpleNewline =
    let expr = "\n"
        expectedValue = Just [T_Newline]
    in TestCase (assertEqual expr expectedValue $ scan expr)


testEmbeddedNewline :: Test
testEmbeddedNewline =
    let expr = "bla\nbla"
        expectedValue = Just [T_Text "bla", T_Newline, T_Text "bla"]
    in TestCase (assertEqual expr expectedValue $ scan expr)


testEmptyLine :: Test
testEmptyLine =
    let expr = "\n\n"
        expectedValue = Just [ T_EmptyLine ]
    in TestCase (assertEqual expr expectedValue $ scan expr )

-- Whitespaces in einer Leerzeile sollen ignoriert werden
testEmptyLineInclWhitespaces :: Test
testEmptyLineInclWhitespaces =
    let expr = "\n \nbla"
        expectedValue = Just [ T_EmptyLine, T_Text "bla"]
    in TestCase (assertEqual expr expectedValue $ scan expr )


testEmptyLineWithWSandSuccessingWSinNextLine :: Test
testEmptyLineWithWSandSuccessingWSinNextLine =
    let expr = "\n \n  bla"
        expectedValue = Just [T_EmptyLine, T_Space 2, T_Text "bla"]
    in TestCase (assertEqual expr expectedValue $ scan expr)

-- Erkennung einzelnes Tabulatorzeichen und Umwandlung in Token
testSingleTab :: Test
testSingleTab =
    let expr = "    "
        expectedValue = Just [ T_Tab 1 ]
    in TestCase (assertEqual expr expectedValue $ scan expr )

-- Erkennung mehrere Tabulatorzeichen
testMultipleTab :: Test
testMultipleTab =
    let expr = "          "
        expectedValue = Just [ T_Tab 2, T_Space 2]
    in TestCase (assertEqual expr expectedValue $ scan expr )

    
-- Erkennung Tabulatorzeichen mit Spaces
testTabWithSpaces :: Test
testTabWithSpaces =
    let expr = "      "
        expectedValue = Just [ T_Tab 1, T_Space 2]
    in TestCase (assertEqual expr expectedValue $ scan expr )

    
-- Erkennung eines Sternzeichen und Umwandlung in Token
testAsterisk :: Test
testAsterisk =
    let expr = "*"
        expectedValue = Just [ T_Asterisk ]
    in TestCase (assertEqual expr expectedValue $ scan expr )

-- Erkennen eines Unterstrichs zur Auszeichnung von kursivem Text
testUnderscore :: Test
testUnderscore =
    let expr = "bla _text_"
        expectedValue = Just [T_Text "bla ", T_Underscore "text"]
    in TestCase (assertEqual expr expectedValue $ scan expr)


-- Erkennung der Definition eines Referenzlinks mit [Reftext]: Link
testRefLinkDef :: Test
testRefLinkDef =
    let expr = "[Test]: http://www.test.de"
        expectedValue = Just [ T_RefLinkDefinition "http://www.test.de"]
    in TestCase (assertEqual expr expectedValue $ scan expr)

    
-- Erkennung eines Referenzlinks, wenn auf einen Referenztext nochmal eckige Klammern kommen
testRefLink :: Test
testRefLink =
    let expr = "[RefText][RefLink]"
        expectedValue = Just [ T_RefText "RefText", T_RefLink "RefLink" ]
    in TestCase (assertEqual expr expectedValue $ scan expr)

    
-- Erkennen eines Tokens, dass einen Referenztext repräsentiert
testRefTextToken :: Test
testRefTextToken =
    let expr = "bla [RefText] bla"
        expectedValue = Just [ T_Text "bla ", T_RefText "RefText", T_Space 1, T_Text "bla" ]
    in TestCase (assertEqual expr expectedValue $ scan expr)


-- Erkennen eines Hyperlinks mit Referenztext
testHLinkWithRefText :: Test
testHLinkWithRefText =
    let expr = "[Referenztext](http://www.google.de)"
        expectedValue = Just [ T_RefText "Referenztext", T_HLink "http://www.google.de" ]
    in TestCase (assertEqual expr expectedValue $ scan expr)

testEmbeddedHLink :: Test
testEmbeddedHLink =
    let expr = "bla [Referenztext](http://www.google.de) bla"
        expectedValue = Just [ T_Text "bla ", T_RefText "Referenztext", T_HLink "http://www.google.de", T_Space 1, T_Text "bla" ]
    in TestCase (assertEqual expr expectedValue $ scan expr)

    
-- Erkennen von eingebundenen Bildern
testImage :: Test
testImage =
    let expr = "text !(http://www.bla.de/img.png)"
        expectedValue = Just [T_Text "text ", T_Image "http://www.bla.de/img.png"]
    in TestCase (assertEqual expr expectedValue $ scan expr)

main :: IO ()
main = do
        runTestTT tests
        return ()