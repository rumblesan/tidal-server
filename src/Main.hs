module Main where

import           Data.List

import           Control.Monad
import           Language.Haskell.Interpreter

main :: IO ()
main = do
  r <- runInterpreter testHint
  case r of
    Left err -> putStrLn $ errorString err
    Right () -> return ()

errorString :: InterpreterError -> String
errorString (WontCompile es) = intercalate "\n" (header : map unbox es)
  where
    header = "ERROR: Won't compile:"
    unbox (GhcError e) = e
errorString e = show e

say :: String -> Interpreter ()
say = liftIO . putStrLn

emptyLine :: Interpreter ()
emptyLine = say ""

setupImports :: [(String, Maybe String)]
setupImports =
  [ ("Prelude", Nothing)
  , ("Sound.Tidal.Context", Nothing)
  , ("Sound.Tidal.Scales", Just "Scales")
  , ("Sound.Tidal.Chords", Just "Chords")
  ]

setupExpressions :: [String]
setupExpressions =
  [ "(cps, nudger, getNow) <- cpsUtils'"
  , "(d1,t1) <- superDirtSetters getNow"
  , "(d2,t2) <- superDirtSetters getNow"
  , "(d3,t3) <- superDirtSetters getNow"
  , "(d4,t4) <- superDirtSetters getNow"
  , "(d5,t5) <- superDirtSetters getNow"
  , "(d6,t6) <- superDirtSetters getNow"
  , "(d7,t7) <- superDirtSetters getNow"
  , "(d8,t8) <- superDirtSetters getNow"
  , "(d9,t9) <- superDirtSetters getNow"
  , "(d10,t10) <- superDirtSetters getNow"
  , "(c1,ct1) <- dirtSetters getNow"
  , "(c2,ct2) <- dirtSetters getNow"
  , "(c3,ct3) <- dirtSetters getNow"
  , "(c4,ct4) <- dirtSetters getNow"
  , "(c5,ct5) <- dirtSetters getNow"
  , "(c6,ct6) <- dirtSetters getNow"
  , "(c7,ct7) <- dirtSetters getNow"
  , "(c8,ct8) <- dirtSetters getNow"
  , "(c9,ct9) <- dirtSetters getNow"
  , "(c10,ct10) <- dirtSetters getNow"
  , "let bps x = cps (x/2)"
  , "let hush = mapM_ ($ silence) [c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,d1,d2,d3,d4,d5,d6,d7,d8,d9,d10]"
  , "let solo = (>>) hush"
  ]

-- observe that Interpreter () is an alias for InterpreterT IO ()
testHint :: Interpreter ()
testHint = do
  say "Starting Tidal Server"
  setImportsQ setupImports
  set [languageExtensions := [OverloadedStrings]]
  emptyLine
  forM_ setupExpressions runStmt
  emptyLine
