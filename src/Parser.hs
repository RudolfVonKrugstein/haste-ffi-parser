
module Parser where

import Text.ParserCombinators.Parsec
import Data.Functor
import Data.Char (digitToInt)
import Data.List (foldl', find)
import Data.Maybe (catMaybes)
import Data.Foldable (asum)

-- Output data structure
data FFILine = PlainLine String
          | FFILine {
            jsExp :: JSExpr,
            hsName :: String,
            cConstraints :: [ClassConstraint],
            hsType :: Type
            } deriving (Eq,Show)

-- Type distinguishes between those types, that are important to us
data Type = IOVoid
          | IOType Type
          | PlainType String
          | ConvertType ConvertData -- a type that must be converted
          | FunctionType Type Type
          deriving (Eq,Show)
-- class constraint in the type signature
data ClassConstraint = ClassConstraint {
     className  :: String,
     parameters :: [String]
  } deriving (Eq,Show)

data ConvertData = ConvertData {
                     typeName    :: String,
                     toConvert   :: String,
                     fromConvert :: String}
                   deriving(Eq,Show)
type ConvertMap = [ConvertData]

type JSExpr = [JSExprPart]
data JSExprPart = StringPart String | ArgumentPart Int | RestArgPart deriving (Eq,Show)

parseFFIFile :: ConvertMap -> GenParser Char st [FFILine]
parseFFIFile cm = endBy (line cm) eol
 
line :: ConvertMap -> GenParser Char st FFILine
line cm = ffiLine cm <|> plainLine

plainLine :: GenParser Char st FFILine
plainLine = PlainLine <$> many (noneOf "\n")

whiteSpaces :: GenParser Char st String
whiteSpaces = many $ (char ' ' <|> char '\t' <?> "Whitespace")
whiteSpaces1 :: GenParser Char st String
whiteSpaces1 = many1 $ (char ' ' <|> char '\t' <?> "Whitespace")

ffiLine :: ConvertMap -> GenParser Char st FFILine
ffiLine cm = do
  try $ do
    string "foreign"
    whiteSpaces1
    string "import"
    whiteSpaces1
    string "cpattern"
  whiteSpaces1
  char '\"'
  jsName <- jsExpr
  char '\"'
  whiteSpaces
  hsName <- many1 (alphaNum <|> char '_')
  whiteSpaces
  string "::"
  whiteSpaces
  (constraints,newCm) <- try (classConstraints cm) <|> return ([],cm)
  whiteSpaces
  signature <- typeSignature newCm
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

typeSignature :: ConvertMap -> GenParser Char st Type
typeSignature cm = try (functionType cm) <|> try (oneArgumentType cm)

oneArgumentType :: ConvertMap -> GenParser Char st Type
oneArgumentType cm =
  try (do char '('
          res <- typeSignature cm
          char ')'
          return res)
  <|> try (convertType cm)
  <|> try ioVoidType
  <|> try (ioType cm)
  <|> try plainType
  <?> "Some haskell type"
                  
convertType :: ConvertMap -> GenParser Char st Type
convertType cm = do
  whiteSpaces
  -- test if any of the types match (they are in fst cm)
  r <- asum . map (\c -> do {string $ typeName c; return $ ConvertType c}) $ cm
  whiteSpaces
  return r
  
ioVoidType :: GenParser Char st Type
ioVoidType = do
  whiteSpaces
  string "IO"
  whiteSpaces
  string "()"
  return IOVoid
  
ioType :: ConvertMap -> GenParser Char st Type
ioType cm = do
  whiteSpaces
  string "IO"
  whiteSpaces
  r <- oneArgumentType cm
  whiteSpaces
  return $ IOType r

plainType :: GenParser Char st Type
plainType = do
  whiteSpaces
  parts <- many1 plainTypePart
  whiteSpaces
  return $ PlainType $ concat parts

plainTypePart :: GenParser Char st String
plainTypePart = do
  try $ do char '('
           parts <- many plainTypePart
           char ')'
           return $ "(" ++ (concat parts) ++ ")"
  <|> many1 (alphaNum <|> char ' ')

functionType :: ConvertMap -> GenParser Char st Type
functionType cm = do
  whiteSpaces
  t1 <- oneArgumentType cm
  whiteSpaces
  string "->"
  whiteSpaces
  t2 <- typeSignature cm
  whiteSpaces
  return $ FunctionType t1 t2
  
classConstraints :: ConvertMap -> GenParser Char st ([ClassConstraint],ConvertMap)
classConstraints cm = do
  cc <- try $ do c <- singleClassConstraint
                 return [c]
        <|> manyClassConstraints
  whiteSpaces
  string "=>"
  -- find the class names in the convert map, and create appropriae new convert entries
  let singleParamConstraints = filter (\(ClassConstraint _ parameters) -> length parameters == 1) cc
      constrAppliesToConvertData constr convDat = typeName convDat == className constr
      makeConvDataWithClassConstr constr convData = convData {typeName = (head . parameters) constr}
      newCM = catMaybes $ map (\constr ->(makeConvDataWithClassConstr constr) <$> (find (constrAppliesToConvertData constr) cm)) singleParamConstraints
  return (cc,cm ++ newCM)

singleClassConstraint :: GenParser Char st ClassConstraint
singleClassConstraint = do
  whiteSpaces
  first <- upper
  rest  <- many alphaNum
  let className = first:rest
  whiteSpaces1
  names <- many1 nameInClassConstraint
  return $ ClassConstraint className names

nameInClassConstraint :: GenParser Char st String
nameInClassConstraint = do
  first <- lower
  rest  <- many alphaNum
  whiteSpaces
  return (first:rest)

manyClassConstraints :: GenParser Char st [ClassConstraint]
manyClassConstraints = do
  char '('
  whiteSpaces
  res <- sepBy1 singleClassConstraint (char ',')
  char ')'
  return res

  

eol = char '\n' <?> "end of line"
