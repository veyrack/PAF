-- Option de compilation pour que les chaînes litérales
-- soient surchargées plutôt que fixées à `[Char]`
{-# LANGUAGE OverloadedStrings #-}
module Main where
-- Le type `Text` est disponible
import Data.Text (Text)

-- Les fonctions de manipulation de textes
-- sont préfixées par `T` plutôt que `Data.Text`
import qualified Data.Text as T
-- Les fonctions d'entrées sorties pour les textes
import qualified Data.Text.IO as TIO
import System.Environment

import Stat

analyse :: Text -> String -> IO ()
analyse texte titre = (putStrLn $ "Analyse du texte de " ++ titre ++ "\n")
    >>= (\_ -> putStrLn ("Nombre de caractères " ++ (show $ compte texte) ++ "\n"))
    >>= (\_ -> putStrLn ("Nombre de mots " ++ (show $ compteMotF texte) ++ "\n"))
    >>= let dict = consDict texte in
        (\_ -> putStrLn ("*** Caracteres de " ++ titre ++ "***\n\n" ++ imprimeDict $ triDict dict))

mainEtParse :: [String] -> IO ()
mainEtParse [] = putStrLn "Il manque l'argument.\n"
mainEtParse (titre:_) = (TIO.readFile $ "./txt/" ++ titre ++ ".txt") >>= (\t -> analyse t titre)


main :: IO ()
main = do
    mainEtParse ["test"]
    --getArgs >>= mainEtParse