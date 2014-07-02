module IR where

import Data.Map as M

-- Abstract Syntax Tree für HTML-Generierung. Daher schon nahe an HTML angelehnt.
data AST = Sequence [AST] -- eine Sequenz von HTML-Elementen
         | H Int String   -- eine Überschrift, de Int ist das Level (6 für H6) und der String der Text
         | UL [AST]       -- eine ungeordnete Liste, in der Liste müssen dann die Listenelemente stehen
         | OL [AST]       -- eine geordnete Liste, in der Liste müssen dann die Listenelemente stehen
         | LiE [AST]      -- ein Listenelement mit dem Inhalt
         | Text String    -- reiner Textblock
         | EmptyLine      -- eine leere Zeile
         | Bold [AST]     -- ein fettgedruckter Text
         | Kursiv [AST]   -- eine kursiv gedruckter Text
         | Absatz Int AST -- ein Absatz mit Einrueckungsebene
         | Codeblock Int AST  -- ein Codeblock
         | Break          -- ein Zeilenumbruch
         | Image String   -- ein Bild
         | Link String String -- ein Link
         | DLink String String -- ein direkter Link
    deriving (Show, Eq)

type Id         = String
type Url        = String
type References = M.Map Id Url
