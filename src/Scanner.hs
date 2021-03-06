module Scanner where

import      Data.Char
import      Text.Regex

-- MD: Markdown
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
             | T_RefLinkDef String String -- Definition eines Referenzlinks mit Angabe des zugehörigen Hyperlinks
             | T_RefLink String -- Referenzlink
             | T_RefText String -- Referenztext für einen Hyperlink
             | T_HLink String   -- Hyperlink
             | T_Image String   -- eingebundenes Bild mit Bildlink

    deriving (Show, Eq)


-- Scanfunktion
scan :: String -> Maybe [MDToken]


-- Rekursionsende
scan "" = Just []


-- eine Überschrift erkennen
scan string@('#':xs) =
        -- String aufteilen in Hashes und Rest
    let (hashes, rest) = span (=='#') string
        -- Anzahl der Hashes ergibt das Level, aber höchstens 6 werden gezählt, der Rest ignoriert
        level = min (length hashes) 6
    in maybe Nothing (\tokens -> Just (T_H level:tokens))      $ scan rest

    
-- String aufteilen in Sternchen und Rest
scan ('*':xs) = maybe Nothing (\tokens -> Just (T_Asterisk:tokens))    $ scan xs


--zwei aufeinanderfolgende Zeilenumbrüche als Leerzeile erkennen
scan('\n':'\n':xs) =  maybe Nothing (\tokens -> Just (T_EmptyLine:tokens))    $ scan xs

-- sonstige Zeilenumbrüche analysieren, ob es ein einfacher Zeilenumbruch ist oder eine Emptyline mit überflüssigen Spaces.
scan string@('\n':xs) = 
    let (spaces, rest) = span (==' ') xs
        -- faengt der String, um Leerzeichen bereinigt, mit einem Zeilenumbruch an, haben wir eine Emptyline
        in case rest of
            '\n':xd -> maybe Nothing (\tokens -> Just (T_EmptyLine:tokens)) $ scan xd
            _ -> maybe Nothing (\tokens -> Just (T_Newline:tokens)) $ scan xs


-- Tabs erkennen (vier Spaces sind ein Tab)
scan string@(' ':' ':' ':' ':xs) = 
    let (tabs, rest) = span (== ' ') string
        levelTabs = (div (length tabs) 4)
        levelSpaces = (length tabs - levelTabs * 4)
        in case levelSpaces of
            0 -> maybe Nothing (\tokens -> Just (T_Tab levelTabs:tokens))    $ scan rest
            _ -> maybe Nothing (\tokens -> Just (T_Tab levelTabs:T_Space levelSpaces:tokens))    $ scan rest


-- Leerzeichen mit Anzahl erkennen
scan string@(' ':xs) =
    let (spaces, rest) = span (==' ') string
        level = (length spaces)
    in maybe Nothing (\tokens -> Just (T_Space level:tokens))      $ scan rest


-- Erkennen der diversen Aufzaehlungszeichen fuer ungeordnete Listen
scan ('-':xs) = maybe Nothing (\tokens -> Just (T_Seperator:tokens))    $ scan xs

scan ('+':xs) = maybe Nothing (\tokens -> Just (T_Plus:tokens))    $ scan xs

scan ('\\':xs) = maybe Nothing (\tokens -> Just (T_Escape:tokens))    $ scan xs

scan (isDigit :'.':xs) = maybe Nothing (\tokens -> Just (T_SLI:tokens))    $ scan xs


-- Erkennen von Underscores, die kursiven Text auszeichnen
scan string@('_':xs) =
    let (italicText, xd) = span (/= '_') xs
        in case xd of
            '_':rest -> maybe Nothing (\tokens -> Just (T_Underscore italicText:tokens)) $ scan rest
            -- kommt hier irgendetwas anderes, wird das Eingescannte als bloßer Text erkannt.


-- Erkennen eckigen Klammern bzw. Klammerpaaren und Differenzierung in RefText, RefLink oder RefLinkDefinition
scan ('[':xs) =
    let (refText, xd) = span (/= ']') xs
        in case xd of
            -- wenn nach dem Referenztext ein weiteres paar eckiger Klammern folgt, muss dieses als Referenzlink erkannt werden.
            ']':'[':rest -> let (refLink, otherStuff) = span (/= ']') rest
                            in maybe Nothing (\tokens -> Just (T_RefText refText:T_RefLink refLink:tokens)) $ scan (tail otherStuff)

            -- folgt danach ein Doppelpunkt, liegt eine Definition eines Referenzlinks vor. Alles bis zum naechsten Leerzeichen wird
            -- dann als zugehoerige URL eingelesen.
            ']':':':rest -> let (url, otherStuff) = span (/=' ') rest
                            in maybe Nothing (\tokens -> Just (T_RefLinkDef refText url:tokens)) $ scan otherStuff

            -- ansonsten gehen wir davon aus, dass irgendetwas anderes nachfolgt - sinnvollerweise ein HLink.
            -- Alles weitere ueberlassen wir der scan-Fkt.
            ']':rest -> maybe Nothing (\tokens -> Just (T_RefText refText:tokens)) $ scan rest

            -- wenn hier irgendetwas anderes kommt, wird es einfach als Text gescannt.

-- Erkennen von Hyperlinks
scan string@('(':xs) =
    let (hlink, xd) = span (/= ')') xs
        in case xd of
            ')':rest -> maybe Nothing (\tokens -> Just (T_HLink hlink:tokens)) $ scan rest
            -- wenn hier irgendetwas anderes kommt, wird es einfach als Text abgefruehstueckt.


--Erkennen von eingebundenen Bildern
scan string@('!':xs) =
    let (exclamMark, rest) = span (=='!') xs
        in case rest of
            -- Erkennen der Kombination '!('... als Anfang eines Bildlinks
            '(':possImgLink -> let (imgLink, rest) = span(/= ')') possImgLink
                               in maybe Nothing (\tokens -> Just (T_Image imgLink:tokens)) $ scan (tail rest)
            -- dient das Ausrufezeichen nicht zum Markieren eines Bildlinks, weil irgendetwas anderes folgt,
            --wird es explizit in ein simples Texttoken umgewandelt.
            _ -> maybe Nothing (\tokens -> Just (T_Text "!":tokens)) $ scan (tail string)


-- alles, was kein Steuersymbol ist, lesen wir einfach bis zum Zeilenende in ein Text-Token ein
scan str =
    let (restOfLine, restOfStr) = span (`notElem` ['\n','*','\\','(',']','[','_','!']) str
    in maybe Nothing (\tokens -> Just (T_Text restOfLine:tokens)) $ scan restOfStr
