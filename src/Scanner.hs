module Scanner where

-- MD: Markdown
data MDToken = T_Newline     -- '\n' 
             | T_H Int       -- ein Header mit der Anzahl der Hashes
             | T_Text String -- Text, aber immer nur bis zum Zeilenende, Text über mehrere Zeilen muss vom Parser zusammengesetzt werden
             | T_Seperator     -- ein ungeordnetes Listenelement-Marker mit der (Einrückungs-)Ebene
             | T_Plus     -- ein ungeordnetes Listenelement-Marker mit der (Einrückungs-)Ebene
             | T_Escape     -- ein ungeordnetes Listenelement-Marker mit der (Einrückungs-)Ebene
             | T_ULI Int
             | T_Asterisk Int     -- ein ungeordnetes Listenelement-Marker mit der (Einrückungs-)Ebene
             | T_Space Int     -- ein ungeordnetes Listenelement-Marker mit der (Einrückungs-)Ebene
    deriving (Show, Eq)

scan :: String -> Maybe [MDToken]

-- Rekursionsende
scan ""           = Just []
-- eine Überschrift
scan str@('#':xs) =
        -- String aufteilen in Hashes und Rest
    let (hashes, rest) = span (=='#') str
        -- Anzahl der Hashes ergibt das Level, aber höchstens 6 werden gezählt, der Rest ignoriert
        level = min (length hashes) 6
    in maybe Nothing (\tokens -> Just (T_H level:tokens))      $ scan rest
-- eine Überschrift
scan str@('*':xs) =
    -- String aufteilen in Sternchen und Rest
    let (stars, rest) = span (=='*') str
        level = (length stars)
    in maybe Nothing (\tokens -> Just (T_Asterisk level:tokens))      $ scan rest
scan str@(' ':xs) =
    -- String aufteilen in Sternchen und Rest
    let (stars, rest) = span (==' ') str
        level = (length stars)
    in maybe Nothing (\tokens -> Just (T_Space level:tokens))      $ scan rest
-- Zeilenumbrüche aufheben um im Parser Leerzeilen zu erkennen
scan ('\n':xs)    = maybe Nothing (\tokens -> Just (T_Newline:tokens)) $ scan xs
-- wenn das '-' am Zeilenanfang gelesen wird, ist es Level 0
-- TODO: noch sind wir sicher am Zeilenanfang, aber nicht mehr unbedingt, wenn wir weitere Fälle einbauen (Links etc.)
scan ('-':xs)     = maybe Nothing (\tokens -> Just (T_Seperator:tokens))    $ scan xs
scan ('+':xs)     = maybe Nothing (\tokens -> Just (T_Plus:tokens))    $ scan xs
scan ('\\':xs)     = maybe Nothing (\tokens -> Just (T_Escape:tokens))    $ scan xs
-- sonst lesen wir einfach den Rest bis zum Zeilenende in ein Text-Token ein
scan str          =
    let (restOfLine, restOfStr) = span (`notElem` ['\n','*','#',' ','\\']) str
    in maybe Nothing (\tokens -> Just (T_Text restOfLine:tokens)) $ scan restOfStr