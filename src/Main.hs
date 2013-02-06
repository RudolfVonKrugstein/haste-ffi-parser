module Main where

import Text.ParserCombinators.Parsec
import Data.Monoid
import Data.Functor
import Data.Char (digitToInt)
import Data.List
import System.IO
import System.Environment
import System.Console.GetOpt

import Parser
import Printer

-- usage info
usageHeader = "Usage: haste-ffi-parser"

-- Command line options
data Options = Options {
  convertFile :: IO String,
  inFileName  :: Maybe String,
  hsFileName  :: Maybe String,
  jsFileName  :: Maybe String
}

defaultOptions = Options (return "[]") Nothing Nothing Nothing

options :: [OptDescr (Options -> Options)]
options = [
  Option ['c'] ["convert"] (ReqArg (\s o -> o {convertFile = readFile s}) "FILE") "file with type definitinitions to convert",
  Option ['i'] ["in"]      (ReqArg (\s o -> o {inFileName = Just s})      "FILE") "input file with ffi definition",
  Option ['o'] ["out"]     (ReqArg (\s o -> o {hsFileName = Just s})      "FILE") "output haskell file",
  Option ['j'] ["js-file"] (ReqArg (\s o -> o {jsFileName = Just s})      "FILE") "javascript file to output"
  ]

main :: IO()
main = do
  args <- getArgs
  -- parse the arguments
  let (actions, nonOpt, msgs) = getOpt RequireOrder options args
      opt = case getOpt RequireOrder options args of
        (actions,[],[]) -> foldl' (flip ($)) defaultOptions actions
        (_,nonOpts ,[]) -> error $ "unrecognized arguments: " ++ unwords nonOpts
        (_,_,     msgs) -> error $ concat msgs ++ usageInfo usageHeader options
  -- check the options
  case opt of
    Options cFile (Just inFile) (Just hsFile) (Just jsFile)  -> do
      cString <- cFile
      let convertTuples = read cString :: [(String,String,String)]
          convertData   = map (\(s1,s2,s3) -> ConvertData s1 s2 s3) convertTuples
      doParse convertData inFile hsFile jsFile
    _ -> error $ usageInfo usageHeader options
    
doParse :: ConvertMap -> String -> String ->String -> IO ()
doParse cData inFile hsFile jsFile = do
  contents <- readFile inFile
  let lines = parse (parseFFIFile cData) inFile contents
  case lines of
    Right l  -> writeFilesOut hsFile jsFile l
    Left p -> putStrLn $ "Error: " ++ (show p)
    
writeFilesOut :: String -> String -> [FFILine] -> IO ()
writeFilesOut hsFile jsFile lines = do
  putStrLn $ show lines
  let hsData = haskellFile lines
      jsData = javascriptFile lines
  writeFile hsFile hsData
  writeFile jsFile jsData
