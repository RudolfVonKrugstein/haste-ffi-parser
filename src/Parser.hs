
module Parser where

import Text.ParserCombinators.Parsec
import Data.Functor
import Data.Char (digitToInt)
import Data.List

-- Output data structure
data FFILine = PlainLine String
          | FFILine {
            jsExp :: JSExpr,
            hsName :: String,
            classConstraints :: String,
            hsType :: Type
            } deriving (Eq,Show)

-- Type distinguishes between those types, that are important to us
data Type = StringType | IOVoid | IOType Type | PlainType String | FunctionType Type Type deriving (Eq,Show)

type JSExpr = [JSExprPart]
data JSExprPart = StringPart String | ArgumentPart Int | RestArgPart deriving (Eq,Show)

parseFFIFile :: GenParser Char st [FFILine]
parseFFIFile = endBy line eol
 
line :: GenParser Char st FFILine
line = ffiLine <|> plainLine

plainLine :: GenParser Char st FFILine
plainLine = PlainLine <$> many (noneOf "\n")

whiteSpaces :: GenParser Char st String
whiteSpaces = many $ (char ' ' <|> char '\t')
whiteSpaces1 :: GenParser Char st String
whiteSpaces1 = many1 $ (char ' ' <|> char '\t')

ffiLine :: GenParser Char st FFILine
ffiLine = do
  try $ do
    string "foreign"
    whiteSpaces1
    string "import"
    whiteSpaces1
    string "jscall"
  whiteSpaces1
  char '\"'
  jsName <- jsExpr
  char '\"'
  whiteSpaces
  hsName <- many1 alphaNum
  whiteSpaces
  string "::"
  whiteSpaces
  constraints <- try classConstr <|> return ""
  whiteSpaces
  signature <- typeSignature 
  return $ FFILine jsName hsName constraints signature

jsExpr :: GenParser Char st JSExpr
jsExpr = many1 jsExprPart

jsExprPart :: GenParser Char st JSExprPart
jsExprPart = try jsExprArgPart <|> try jsExprRestArgPart <|> jsExprStringPart

positiveNatural :: GenParser Char st Int
positiveNatural = 
    foldl' (\a i -> a * 10 + digitToInt i) 0 <$> many1 digit

jsExprArgPart :: GenParser Char st JSExprPart
jsExprArgPart = do
  char '%'
  n <- positiveNatural
  return $ ArgumentPart n
  
jsExprRestArgPart :: GenParser Char st JSExprPart
jsExprRestArgPart = string "%*" >> return RestArgPart

jsExprStringPart :: GenParser Char st JSExprPart
jsExprStringPart = StringPart <$> many1 (noneOf "\"%")

typeSignature :: GenParser Char st Type
typeSignature = try functionType <|> oneArgumentType

oneArgumentType :: GenParser Char st Type
oneArgumentType = try $ do char '('
                           res <- typeSignature
                           char ')'
                           return res
                  <|> try stringType
                  <|> try ioVoidType
                  <|> try ioType
                  <|> plainType
                  
stringType :: GenParser Char st Type
stringType = do
  whiteSpaces
  string "String"
  whiteSpaces
  return StringType
  
ioVoidType :: GenParser Char st Type
ioVoidType = do
  whiteSpaces
  string "IO"
  whiteSpaces
  string "()"
  return IOVoid
  
ioType :: GenParser Char st Type
ioType = do
  whiteSpaces
  string "IO"
  whiteSpaces
  r <- oneArgumentType
  whiteSpaces
  return $ IOType r
  
plainType :: GenParser Char st Type
plainType = do
  whiteSpaces
  r <- many1 alphaNum
  whiteSpaces
  return $ PlainType r
                  

functionType :: GenParser Char st Type
functionType = do
  whiteSpaces
  t1 <- oneArgumentType
  whiteSpaces
  string "->"
  whiteSpaces
  t2 <- typeSignature
  whiteSpaces
  return $ FunctionType t1 t2
  
classConstr :: GenParser Char st String
classConstr = do
  res <- many1 (noneOf "=\n:")
  string "=>"
  return (res ++ "=>")

eol = char '\n'