{-# LANGUAGE LambdaCase #-}
module Main where

-- import qualified MyLib (someFunc)
import Data.Aeson.FlatParse (json)
import Data.ByteString qualified as BS
import FlatParse.Basic qualified as F
import Data.Aeson qualified as A
import Data.Aeson.Types (Value)

import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= \case
  ["flatparse",fname] -> do
    putStrLn "Parsing with flatparse"
    f <- BS.readFile fname
    case F.runParser json f of
      F.OK !res other -> putStrLn $ take 100 $ show (res :: Value)
      e -> print e
  ["aeson",fname] -> do
    putStrLn "Parsing with aeson"
    f <- BS.readFile fname
    case A.eitherDecodeStrict f of
      Right !res -> putStrLn $ take 100 $ show (res :: Value)
      Left err -> print err
  ["compare",fname] -> do
    putStrLn "Comparing aeson and flatparse"
    f <- BS.readFile fname
    case A.eitherDecodeStrict f of
      Right !ares -> case F.runParser json f of
        F.OK !fres other -> print (ares == fres)
        e -> print e
      Left err -> print err
