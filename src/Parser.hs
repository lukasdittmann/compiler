module Parser ( parse {- nur parse exportieren -} )
    where

import           IR
import           Scanner

-- Definiton der verfügbaren Funktione
parse :: [MDToken] -> [LINK] -> Maybe AST
parseInAbs :: [MDToken] -> [LINK] -> String
parseToText :: [MDToken] -> String
streamParseInAbs :: [MDToken] -> [AST]
parsePara :: [MDToken] -> [LINK] -> Maybe AST
parseInList :: [MDToken] -> [LINK] -> Maybe AST
findContents :: [MDToken] -> ([MDToken],[MDToken])
parseCodeblock :: [MDToken] -> Maybe AST

-- [Root] [Absatz] :: Hilfsfunktion für das Parsen von Codeblöcken
parseCodeblock (T_Newline:xs) = maybe Nothing (\(Sequence ast) -> Just $ Sequence (Break:ast)) $ parseCodeblock xs
parseCodeblock (xs) | length xs > 0 = let(elemente,allesDanach) = span (`notElem` [T_Newline]) xs
    in maybe Nothing (\(Sequence ast) -> Just $ Sequence (Text (parseToText elemente):ast)) $ parseCodeblock allesDanach
parseCodeblock _ = Just $ Sequence []


-- [Root] :: Erst die Ebene festlegen:
parse(T_Space i:xs) linkList=  let(elemente,allesDanach) = span (`notElem` [T_EmptyLine]) xs
    in case parsePara elemente linkList of
        Nothing -> Nothing
        Just elems -> maybe Nothing (\(Sequence ast) -> Just $ Sequence (Absatz i elems:ast)) $ parsePara allesDanach linkList
-- [Root] :: Codeblock erkennen
parse(T_Tab i:xs) linkList=  let(elemente,allesDanach) = span (`notElem` [T_EmptyLine]) xs
    in case parseCodeblock elemente of
        Nothing -> Nothing
        Just elems -> maybe Nothing (\(Sequence ast) -> Just $ Sequence (Codeblock (i-1) elems:ast)) $ parsePara allesDanach linkList

-- [Root] :: Falls keine Ebene angegeben wird alles in Ebene 0 packen
parse(xs) linkList=  let(elemente,allesDanach) = span (`notElem` [T_EmptyLine]) xs
    in case parsePara elemente linkList of
        Nothing -> Nothing
        Just elems -> maybe Nothing (\(Sequence ast) -> Just $ Sequence (Absatz 0 elems:ast)) $ parsePara allesDanach linkList

-- [Root] [Absatz] [Liste] :: Hilfsfunktion für Listen
findContents xs@(T_Newline:T_Asterisk:_)=([],xs)
findContents xs@(T_Newline:T_SLI:_)=([],xs)
findContents xs@(T_Newline:T_Plus:_)=([],xs)
findContents xs@(T_Newline:T_Seperator:_)=([],xs)
findContents xs@(T_EmptyLine:_)=([],xs)
findContents [] = ([],[])
findContents (t:ts) = let(tl1,tl2) = findContents ts
    in(t:tl1,tl2)

-- [Root] [Absatz] [Liste] :: Unterfunktion für Listen
parseInList (T_Newline:a: xs) linkList | a `elem` [T_Seperator,T_Plus,T_Asterisk,T_SLI] =  let(elemente,allesDanach) = findContents xs
    in case parsePara elemente linkList of
        Nothing -> Nothing
        Just elemC-> maybe Nothing (\(Sequence ast) -> Just $ Sequence (LiE [elemC]:ast)) $ parseInList allesDanach linkList
parseInList xs linkList= parsePara xs linkList

-- [Root] [Absatz] :: Listenelemente finden
parsePara (a:T_Space int: xs) linkList | a `elem` [T_Seperator,T_Plus]=  let(elemente,allesDanach) = findContents xs
    in case parsePara (T_Space int:elemente) linkList of
        Nothing -> Nothing
        Just elemC-> maybe Nothing (\(Sequence ast) -> Just $ Sequence [UL (LiE [elemC]:ast)]) $ parseInList allesDanach linkList

-- [Root] [Absatz] :: Listenelemente finden
parsePara (T_Asterisk:T_Space int: xs)  linkList=  let(elemente,allesDanach) = findContents xs
    in case parsePara (T_Space int:elemente) linkList of
        Nothing -> Nothing
        Just elemC-> maybe Nothing (\(Sequence ast) -> Just $ Sequence [UL (LiE [elemC]:ast)]) $ parseInList allesDanach linkList

-- [Root] [Absatz] :: Aufzählung finden
parsePara (T_SLI:T_Space int: xs) linkList =  let(elemente,allesDanach) = findContents xs
    in case parsePara (T_Space int:elemente) linkList of
        Nothing -> Nothing
        Just elemC-> maybe Nothing (\(Sequence ast) -> Just $ Sequence [OL (LiE [elemC]:ast)]) $ parseInList allesDanach linkList

-- [Root] [Absatz] :: Exit-Strategy
parsePara (T_EmptyLine:xs) linkList= parse xs linkList

-- [Root] [Absatz] :: Escaped Symbols
parsePara (T_Escape:T_Asterisk:xs) linkList= maybe Nothing (\ast -> Just $ addP (Text "*") ast) $ parsePara xs linkList

-- [Root] [Absatz] :: Kursiv
parsePara (T_Asterisk: xs) linkList | (head xs) /= T_Asterisk =  let(elemente,allesDanach) = span (`notElem` [T_Asterisk]) xs
    in maybe Nothing (\(Sequence ast) -> Just $ Sequence (Kursiv (streamParseInAbs elemente):ast)) $ parsePara (tail allesDanach) linkList

-- [Root] [Absatz] :: Bold
parsePara (T_Asterisk: T_Asterisk: xs) linkList=  let(elemente,allesDanach) = span (`notElem` [T_Asterisk]) xs
    in maybe Nothing (\(Sequence ast) -> Just $ Sequence (Bold (streamParseInAbs elemente):ast)) $ parsePara (tail (tail allesDanach)) linkList

-- [Root] [Absatz] :: Text
parsePara (T_Text str:xs) linkList= maybe Nothing (\ast -> Just $ addP (Text str) (addP (Text " ") ast)) $ parsePara xs linkList
parsePara (T_Space int:xs) linkList= maybe Nothing (\ast -> Just $ addP (Text " ") (addP (Text " ") ast)) $ parsePara xs linkList

-- [Root] [Absatz] :: Neue Zeile
parsePara (T_Newline:xs) linkList= maybe Nothing (\(Sequence ast) -> Just $ Sequence (Break:ast)) $ parsePara xs linkList

-- [Root] [Absatz] :: Bild erkennen
parsePara (T_Image str:xs) linkList= maybe Nothing (\(Sequence ast) -> Just $ Sequence (Image str:ast)) $ parsePara xs linkList

-- [Root] [Absatz] :: Header erkennen
parsePara (T_H i : T_Space 1:T_Text str: xs) linkList= maybe Nothing (\(Sequence ast) -> Just $ Sequence (H i str:ast)) $ parsePara xs linkList
parsePara (T_H i : T_Text str: xs) linkList= maybe Nothing (\(Sequence ast) -> Just $ Sequence (H i str:ast)) $ parsePara xs linkList

-- Falls nichts erkannt wird
parsePara _  _= Just $ Sequence []

-- [Root] [Absatz] [Element] :: Elemente in Text umwandeln
parseInAbs (T_Space int:xs) linkList= (\str -> " " ++ str) $ parseInAbs xs linkList
parseInAbs (T_Text istr:xs) linkList= (\str -> istr ++ str) $ parseInAbs xs linkList
parseInAbs (T_Asterisk:xs) linkList= (\str -> "*" ++ str) $ parseInAbs xs linkList
parseInAbs (T_Newline:xs) linkList= case parseInAbs xs linkList of
                                " " -> ""
                                stri ->  "#TODO#" ++ stri
parseInAbs _ _= " "

-- [Root] [Absatz] [Bold/Kursiv] :: Elemente zusammenfügen auch über Zeilenumbruch hinweg
streamParseInAbs (T_Newline:xs) = (\ast -> Break:ast) $ streamParseInAbs xs

streamParseInAbs (T_Text str:xs) = let (bisBr,nachBr) = span (`notElem` [T_Newline]) xs
                                        in (\ast -> (Text (str ++ (parseToText bisBr))):ast) $ streamParseInAbs nachBr
streamParseInAbs (T_Space int:xs) = let (bisBr,nachBr) = span (`notElem` [T_Newline]) xs
                                        in (\ast -> (Text (" " ++ (parseToText bisBr))):ast) $ streamParseInAbs nachBr
streamParseInAbs _ = []

-- [Root] [Codeblock] :: Alle Tokens als Text ausgeben
parseToText (T_Space int:xs) = (\str -> " " ++ str) $ parseToText xs
parseToText (T_Newline:xs) = (\str -> " " ++ str) $ parseToText xs
parseToText (T_Tab int:xs) = (\str -> " " ++ str) $ parseToText xs
parseToText (T_Text istr:xs) = (\str -> istr ++ str) $ parseToText xs
parseToText (T_Image istr:xs) = (\str -> istr ++ str) $ parseToText xs
parseToText (T_Asterisk:xs) = (\str -> "*" ++ str) $ parseToText xs
parseToText _ = ""

-- [Root] [Absatz] :: Hilfsfunktionen für den Parser

-- Mehrere aufeinander folgende Texte werden zu einem Absatz zusammengefügt.
addP :: AST -> AST -> AST
-- Wenn wir zwei Absätze hintereinander finden, fassen wir diese zusammen 
addP (Text str1) (Sequence (Text str2 : ast)) = Sequence (Text (str1 ++ str2) : ast)
-- Andernfalls bleibt der Absatz alleine
addP p (Sequence ast) = Sequence (p : ast)

