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
             | T_Space Int      -- ein Leerzeichen mit zugehoeriger Anzahl
             | T_Tab Int        -- 
             | T_EmptyLine      -- eine leere Zeile, nach der ein neuer P-Absatz beginnt

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

-- Zeilenumbrüche aufheben, um im Parser Leerzeilen zu erkennen
scan string@('\n':xs) = 
    let (spaces, rest) = span (==' ') string
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

-- sonst lesen wir einfach den Rest bis zum Zeilenende in ein Text-Token ein
scan str =
    let (restOfLine, restOfStr) = span (`notElem` ['\n','*','\\']) str
    in maybe Nothing (\tokens -> Just (T_Text restOfLine:tokens)) $ scan restOfStr