module Main where

import System.Console.Haskeline
import System.Console.Haskeline.Completion
import System.Environment
import Control.Exception (AsyncException(..))

{-
Testing case-insensitive autocompletion.

Usage:
(from the project folder)
    cabal new-repl
(an then from inside ghci)
    :load examples/Case.hs
    main
-}

{- Expected behaviour:
        r<tab> does not change cap of the letter
        R<tab> does not change cap of the letter
        Ca<tab> suggestions are "Cane callo"
        ca<tab> suggestions are "callo Cane"
        ma<tab> gets completed to "maybe"
        Ma<tab> gets completed to "Maybe"
        cane<tab> gets completed to "Cane"
-}

mySettings :: Settings IO
mySettings = setComplete cfCI defaultSettings
    where
          cfCI :: CompletionFunc IO
          cfCI = completeList Nothing " "
                     ["Rana", "rosa", "Cane", "callo", "maybe", "Maybe"]
                     IgnoreCase

main :: IO ()
main = runInputT mySettings $ withInterrupt $ loop getInputLine 0
    where
        loop inputFunc n = do
            minput <- handle (\Interrupt -> return (Just "Caught interrupted"))
                        $ inputFunc (show n ++ ":")
            case minput of
                Nothing -> return ()
                Just "quit" -> return ()
                Just "q" -> return ()
                Just s -> do
                            outputStrLn ("line " ++ show n ++ ":" ++ s)
                            loop inputFunc (n+1)

