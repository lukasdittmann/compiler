module TestParser where

import Parser

import Scanner

import IR

import Test.HUnit

data MDToken = T_Newline        -- '\n' 
             | T_H Int          -- ein Header mit der Anzahl der Hashes
             | T_Text String    -- Text, aber immer nur bis zum Zeilenende, Text über mehrere Zeilen muss vom Parser zusammengesetzt werden
             | T_Seperator      -- Aufzaehlungselement '-' fuer UL
             | T_Plus           -- Aufzaehlungszeichen '+' fuer UL
             | T_Escape
             | T_ULI Int        -- ein ungeordnetes Listenelement-Marker mit der (Einrückungs-)Ebene
             | T_SLI            -- ein geordnetes Listenelement
             | T_Asterisk       -- '*'
             | T_Underscore String  -- '_' fuer Kursivtext mit zugehoerigem String
             | T_Space Int      -- ein Leerzeichen mit zugehoeriger Anzahl
             | T_Tab Int        -- 
             | T_EmptyLine      -- eine leere Zeile, nach der ein neuer P-Absatz
             | T_RefLinkDefinition String -- Definition eines Referenzlinks mit Angabe des zugehörigen Hyperlinks
             | T_RefLink String -- Referenzlink
             | T_RefText String -- Referenztext für einen Hyperlink
             | T_HLink String   -- Hyperlink
             | T_Image String   -- eingebundenes Bild mit Bildlink

    deriving (Show, Eq)

tests :: Test
tests = TestList [ TestLabel "SimpleTests" 
                    testSimpleText,
                    testSimpleUL,
                    testSimpleOL,
                    testSimpleKursiv,
                    testSimpleBold
                ]


testSimpleText :: Test
testSimpleText =
    let expr = [Scanner.T_Text "blablabla"]
        expectedValue = Just [Sequence [Absatz 0 (Sequence [Text "blablabla "])]]
        linkList = []
    in case (parse expr linkList) of
        Just elems -> TestCase (assertEqual (show elems) expectedValue $ Just [elems])
        Nothing -> TestCase (assertFailure "False")

testSimpleUL :: Test
testSimpleUL =
    let expr = [Scanner.T_Asterisk, Scanner.T_Space 1, Scanner.T_Text "Test"]
        expectedValue = Just [Sequence [Absatz 0 (Sequence [UL [LiE [Sequence [Text "  Test "]]]])]]
        linkList = []
    in case (parse expr linkList) of
        Just elems -> TestCase (assertEqual (show elems) expectedValue $ Just [elems])
        Nothing -> TestCase (assertFailure "False")

testSimpleOL :: Test
testSimpleOL =
    let expr = [Scanner.T_SLI, Scanner.T_Space 1,Scanner.T_Text "Test"]
        expectedValue = Just [Sequence [Absatz 0 (Sequence [OL [LiE [Sequence [Text "  Test "]]]])]]
        linkList = []
    in case (parse expr linkList) of
        Just elems -> TestCase (assertEqual (show elems) expectedValue $ Just [elems])
        Nothing -> TestCase (assertFailure "False")

testSimpleKursiv :: Test
testSimpleKursiv =
    let expr = [Scanner.T_Asterisk, Scanner.T_Text "Test", Scanner.T_Asterisk]
        expectedValue = Just [Sequence [Absatz 0 (Sequence [Kursiv [Text "Test"]])]]
        linkList = []
    in case (parse expr linkList) of
        Just elems -> TestCase (assertEqual (show elems) expectedValue $ Just [elems])
        Nothing -> TestCase (assertFailure "False")

testSimpleBold :: Test
testSimpleBold =
    let expr = [Scanner.T_Asterisk, Scanner.T_Asterisk, Scanner.T_Text "Test", Scanner.T_Asterisk, Scanner.T_Asterisk]
        expectedValue = Just [Sequence [Absatz 0 (Sequence [Bold [Text "Test"]])]]
        linkList = []
    in case (parse expr linkList) of
        Just elems -> TestCase (assertEqual (show elems) expectedValue $ Just [elems])
        Nothing -> TestCase (assertFailure "False")


main :: IO ()
main = do
        runTestTT tests
        return ()
