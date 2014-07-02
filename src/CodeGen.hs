-- Modul zum Generieren von HTML-Code der als String repräsentiert wird aus einem AST.
module CodeGen where

import IR

-- HTML generieren
-- zuerst das äußere Gerüst
generateHTML :: AST -> String
generateHTML ast = "<html>\n<head></head>\n<body>\n" ++ generateHTML' ast ++ "</body>\n</html>"

-- dann Elemente für jeden AST-Knoten
generateHTML' :: AST -> String

-- eine Sequenz/Absatz
generateHTML' (Sequence (a:as)) = generateHTML' a ++ "" ++ generateHTML' (Sequence as)
generateHTML' (Absatz int ast)  = case int of
    0 -> "" ++ (generateHTML' ast) ++ "\n"
    _ -> "<p>\n" ++ (generateHTML' ast) ++ "\n</p>\n"

-- ein Codeblock
generateHTML' (Codeblock int ast)  = "<p style=\"border: "++ show (int+1) ++"px #000000 solid;\">\n" ++ (generateHTML' ast) ++ "\n</p>\n"

-- eine Überschrift
generateHTML' (H i str) = "<h" ++ show i ++ ">" ++ str ++ "</h" ++ show i ++ ">"

-- Bold und Kursiv
generateHTML' (Bold ast) = "<b>" ++ concat (map generateHTML' ast) ++ "</b>"
generateHTML' (Kursiv ast) = "<i>" ++ concat (map generateHTML' ast) ++ "</i>"

-- Listen
generateHTML' (UL lis) = "<ul>\n" ++ concat (map generateHTML' lis) ++ "</ul>\n"
generateHTML' (OL lis) = "<ol>\n" ++ concat (map generateHTML' lis) ++ "</ol>\n"

-- Listenelemente
generateHTML' (LiE ast) = "<li>" ++ concat (map generateHTML' ast) ++ "</li>\n"

-- Bilder
generateHTML' (Image str) = "<img src=\""++ str ++"\"/>\n"

-- Zeilenumbruch
generateHTML' (Break) = "<br>\n"

-- ein Textblock
generateHTML' (Text str)  = "" ++ str ++ ""

-- alles andere wird für den ignoriert
generateHTML' _ = ""
