
module Printer where

import Parser

haskellFile :: [FFILine] -> String
haskellFile = concat . map haskellLine

haskellLine :: FFILine -> String
haskellLine (PlainLine s) = s ++ "\n"

haskellLine (FFILine jsExp hsName args retVal) =
  if or . map (==StringArgument) $ args then
    --expression with strings
    "foreign import ccall \"" ++ hsName ++ "JSImpl\" " ++ hsName ++ "JSStr :: " ++ signature
    ++ "\n" ++ hsName ++ (argumentList (length args)) ++ " = " ++ hsName ++ "JSImpl " ++ (concat . map showArg $ zip args [1..] ) ++ "\n"
  else
    --expression without strings
    "foreign import ccall \"" ++ hsName ++ "JSImpl\" " ++ hsName ++ " :: " ++ signature ++ "\n"
  where
    argTypeList = concat . map (\a-> showArgType a ++ " -> ") $ args
    signature = argTypeList ++ (showRetType retVal)
    showArgType :: Argument -> String
    showArgType StringArgument = "JSString"
    showArgType (OtherArgument s) = s
    showRetType :: ReturnValue -> String
    showRetType (IOVoid) = "IO ()"
    showRetType (IOReturn s) = "IO " ++ s
    showRetType (PureReturn s) = s
    argumentList :: Int -> String
    argumentList max = concat . map (\i -> " a" ++ (show i)) $ [1..max]
    showArg :: (Argument,Int) -> String
    showArg (a,i) = if a == StringArgument then "(toJS a" ++ (show i) ++ ") " else "a" ++ (show i) ++ " "


javascriptFile :: [FFILine] -> String
javascriptFile = concat . map javascriptLine

javaScriptLine (PlainLine _) = ""
javascriptLine (FFILine jsExp hsName args retVal) =
  "function " ++ hsName ++ "JSImpl(" ++ (argumentList $ length args) ++ ioArg ++ ") {\n  "
  ++ if retVal == IOVoid then jsCommand ++ ";\n  return [1,0];\n}" else "  return [1,0," ++ jsCommand ++ "];\n}\n"
  where
    argumentList :: Int -> String
    argumentList max = concatWith "," . map (\i -> "a" ++ (show i)) $ [1..max]
    concatWith :: String -> [String] -> String
    concatWith sep (x:y:xs) = x ++ sep ++ (concatWith sep (y:xs))
    concatWith _ (x:[])     = x
    concatWith _  _ = ""
    ioArg = if isIORetVal retVal then ",_" else ""
    jsCommand = concat . map showExprPart $ jsExp
    showExprPart :: JSExprPart -> String
    showExprPart (StringPart s) = s
    showExprPart (ArgumentPart i) = "a" ++ (show i)
    showExprPart (RestArgPart) = concatWith "," . map showExprPart $ restArguments
    restArguments :: [JSExprPart]
    restArguments = let argId (ArgumentPart i) = i
                        argId _ = 0
                        highestArgument = maximum . map (argId) $ jsExp
                        numArguments = length args
                        missingArgs = if highestArgument >= numArguments then [] else [(highestArgument+1) .. numArguments]
                    in map (\i -> ArgumentPart i) missingArgs

