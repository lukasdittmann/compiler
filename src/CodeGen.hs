-- Modul zum Generieren von HTML-Code der als String repräsentiert wird aus einem AST.
module CodeGen where

import IR

import Data.Map as M

-- HTML generieren
-- zuerst das äußere Gerüst
generateHTML :: AST-> References -> String
generateHTML ast linkList= "<html>\n<head></head>\n<body>\n" ++ generateHTML' (ast,linkList)++ "</body>\n</html>"

-- dann Elemente für jeden AST-Knoten
generateHTML' :: (AST,References) -> String

-- eine Sequenz/Absatz
generateHTML' ((Sequence (a:as)),linkList)= generateHTML' (a, linkList)++ "" ++ generateHTML' ((Sequence as), linkList)
generateHTML' ((Absatz int ast),  linkList)= case int of
    0 -> "" ++ (generateHTML' (ast, linkList)) ++ "\n"
    _ -> "<p>\n" ++ (generateHTML' (ast, linkList)) ++ "\n</p>\n"

-- ein Codeblock
generateHTML' ((Codeblock int ast), linkList)= "<p style=\"border: "++ show (int+1) ++"px #000000 solid;\">\n" ++ (generateHTML' (ast, linkList)) ++ "\n</p>\n"

-- eine Überschrift
generateHTML' ((H i str),linkList)= "<h" ++ show i ++ ">" ++ str ++ "</h" ++ show i ++ ">"

-- Bold und Kursiv
generateHTML' ((Bold ast),linkList) = "<b>" ++ generateHTML' (Sequence ast,linkList) ++ "</b>"
generateHTML' ((Kursiv ast),linkList)= "<i>" ++ generateHTML' (Sequence ast,linkList) ++ "</i>"

-- Listen
generateHTML' ((UL lis),linkList)= "<ul>\n" ++ generateHTML' (Sequence lis,linkList) ++ "</ul>\n"
generateHTML' ((OL lis),linkList)= "<ol>\n" ++ generateHTML' (Sequence lis,linkList) ++ "</ol>\n"

-- Listenelemente
generateHTML' ((LiE ast),linkList)= "<li>" ++ generateHTML' (Sequence ast,linkList) ++ "</li>\n"

-- Bilder
generateHTML' ((Image str),linkList)= "<img src=\""++ str ++"\"/>\n"

-- Zeilenumbruch
generateHTML' ((Break),linkList)= "<br>\n"

-- ein Textblock
generateHTML' ((Text str),linkList) = "" ++ str ++ ""

-- Referenz - Links umsetzen
generateHTML' ((Link id text),linkList) = 
    let ref = M.lookup id linkList
    in (case ref of
        Nothing  -> "<a href=\"#\">"++ text ++"</a>"
        Just url -> "<a href=\"" ++ url
                    ++ "\">"++ text ++"</a>"
     )

-- Direkte Links übersetzen
generateHTML' ((DLink url text),linkList) = "<a href=\""++ url ++"\">"++ text ++"</a>"
     
-- alles andere wird für den ignoriert
generateHTML' _ = ""
