module Main where

import Text.ParserCombinators.Parsec
import Data.Monoid
import Data.Functor
import Data.Char (digitToInt)
import Data.List
import System.IO
import System.Environment

import Parser
import Printer


main :: IO()
main = do
  args <- getArgs
  case args of
    [inFile,outFile] -> doParse inFile outFile
    _                -> error "Syntax: ffiparser <infile> <outfile>"
    
doParse :: String -> String -> IO ()
doParse inFile outFile = do
  contents <- readFile inFile
  let lines = parse parseFFIFile inFile contents
  case lines of
    Right l  -> writeFilesOut outFile l
    Left p -> putStrLn $ "Error: " ++ (show p)
    
writeFilesOut :: String -> [FFILine] -> IO ()
writeFilesOut baseName lines = do
  putStrLn $ show lines
  let hsFile = haskellFile lines
      jsFile = javascriptFile lines
  writeFile (baseName ++ ".hs") hsFile
  writeFile (baseName ++ ".js") jsFile