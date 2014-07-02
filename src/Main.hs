module Main where

import           CodeGen
import           IR
import           Parser
import           Scanner

main :: IO ()
main = do
    -- lese den Inhalt der Datei "test.md" als einen kompletten String ein
    input <- readFile "newMDFile.md"
    -- Liste erstellen (Leere Listen führen zu Fehlern in Haskell)
    let linkList = [Link "" ""]

    -- Scaner-Tokens lesen
    let maybeTokens = scan input
   
    putStrLn "Scanner output\n=============="
    --print maybeTokens
    
    -- Verarbeitung der Scanner-Tokens (Maybe Nothing)
    case maybeTokens of
        Nothing -> putStrLn "scanner failed"
        Just tokens -> do -- der Scanner war erfolgreich
            -- versuche die Tokens zu parsen
            let maybeAst = parse tokens linkList
                
            putStrLn "\nParser output\n============="
            print maybeAst
            
            putStrLn "Scanner output\n=============="
            print linkList

            -- Verarbeitung der Parse-Tokens (Maybe Nothing) 
            case maybeAst of
                Nothing -> putStrLn "parser failed"
                Just ast -> do -- der Parser war erfolgreich
                               
                    putStrLn "\nGenerated HTML\n=============="
                    putStrLn $ generateHTML ast
