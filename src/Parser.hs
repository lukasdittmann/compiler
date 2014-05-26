module Parser ( parse ) -- nur parse exportieren
    where

import           IR
import           Scanner

-- Der Parser versucht aus einer Liste von MDToken einen AST zu erzeugen 
parse :: [MDToken] -> Maybe AST
-- Gesondertes Parsen des Texts innerhalb eines LI-Elements
parseInAbs :: [MDToken] -> [AST]

-- Die leere Liste ergibt eine leere Sequenz
parse [] = Just $ Sequence []

-- LI-Element erkennen und Inhalt gesondert parsen, um bspw. kursiven Text erkennen zu koennen
parse (T_Seperator: xs) =  let(elemente,allesDanach) = span (`notElem` [T_Newline]) xs
    in maybe Nothing (\(Sequence ast) -> Just $ Sequence (Li (parseInAbs elemente):ast)) $ parse allesDanach

-- Leerzeichen, gefolgt von Text, wird in Absatz eingefuegt
parse (T_Space 1:T_Text str:xs) = maybe Nothing (\ast -> Just $ addP (P str) (addP (P " ") ast)) $ parse xs

-- Zwei Zeilenumbrüche hintereinander sind eine leere Zeile, die in eine Sequenz eingeführt wird (wirklich immer?)
parse (T_Newline:T_Newline:xs) = maybe Nothing (\(Sequence ast) -> Just $ Sequence (EmptyLine : ast)) $ parse xs

-- eine einzelne Leerzeile ignorieren wir (für den Moment?)
parse (T_Newline:xs) = parse xs

-- einem Header muss ein Text folgen. Das ergibt zusammen einen Header im AST, er wird einer Sequenz hinzugefügt
parse (T_H i : T_Space 1:T_Text str: xs) = maybe Nothing (\(Sequence ast) -> Just $ Sequence (H i str:ast)) $ parse xs

-- Ueberschrift mit zugehoerigem Level erkennen
parse (T_H i : T_Text str: xs) = maybe Nothing (\(Sequence ast) -> Just $ Sequence (H i str:ast)) $ parse xs

-- Der gesamte Rest wird für den Moment ignoriert. Achtung: Der Parser schlägt, in der momentanen Implementierung, nie fehl.
-- Das kann in der Endfassung natürlich nicht so bleiben!
parse _ = Just $ Sequence []

--parseInAbs (T_Asterisk: xs) =  let(elemente,allesDanach) = span (`notElem` [T_Asterisk]) xs
--    in (\(Sequence ast) -> Just $ Sequence (Kur (parseInAbs elemente):ast)) $ parseInAbs allesDanach

--parseInAbs (T_Text str: xs) = maybe Nothing (\(Sequence ast) -> Just $ Sequence (P str: ast)) $ parse xs

parseInAbs list@_ = [Bold (show list)]


------------------------------------
{- Hilfsfunktionen für den Parser -}

-- Mehrere aufeinander folgende Texte werden zu einem Absatz zusammengefügt.
addP :: AST -> AST -> AST
-- Wenn wir zwei Absätze hintereinander finden, fassen wir diese zusammen 
addP (P str1) (Sequence (P str2 : ast)) = Sequence (P (str1 ++ "\n" ++ str2) : ast)
-- Andernfalls bleibt der Absatz alleine
addP p (Sequence ast) = Sequence (p : ast)
