
module Printer where

import Parser

haskellFile :: [FFILine] -> String
haskellFile = concat . map haskellLine

haskellLine :: FFILine -> String
haskellLine (PlainLine s) = s ++ "\n"

haskellLine (FFILine _ hsName cConstr hsType) =
  if needsConversion hsType then
    --expression with something that needs conversion
    -- so output to lines, the foreign call with converted types, and the conversion function
    "foreign import ccall \"" ++ hsName ++ "_CImpl\" " ++ hsName ++ "_With_CTypes :: " ++ (signature hsType) ++ "\n"
    ++ hsName ++ argumentList ++ "= " ++ resultConversion ++ hsName ++ "_With_CTypes " ++ argumentListWithConversion ++ "\n"
  else
    --expression without strings
    "foreign import ccall \"" ++ hsName ++ "_CImpl\" " ++ hsName ++ " :: " ++ (signature hsType) ++ "\n"
  where
    -- return true if there is a IO or function type in the argument list
    -- return true if there is coversion type with to conversion in argument list
    -- return true of there is converion type with from conversion as result
    needsConversion t = case t of
      (IOType t')        -> needsConversion t'
      (ConvertType dat)  -> if null (fromConvert dat) then False else True
      (FunctionType a r) -> argNeedsConversion a || needsConversion r
      _                  -> False
    argNeedsConversion t = case t of
      (ConvertType dat)  -> if null (toConvert dat) then False else True
      IOVoid             -> True
      (IOType _)         -> True
      (FunctionType _ _) -> True
      _                  -> False
    numArgs = numArgs' hsType
      where
      numArgs' (FunctionType _ r) = 1 + (numArgs' r)
      numArgs' _                  = 0
    argumentList :: String
    argumentList = concat . map (\i -> " a" ++ (show i)) $ ([1..numArgs] :: [Int])
    argumentListWithConversion :: String
    argumentListWithConversion = concat $ zipWith argumentConversion ([1..] :: [Int]) (argTypes hsType)
      where
      argumentConversion id t = case t of
        (ConvertType dat)  -> if null (toConvert dat) then
                                 "a" ++ (show id)
                              else
                                 "(" ++ (toConvert dat) ++ " a" ++ (show id) ++")"
        (FunctionType _ _) -> "(mkCallback $! a" ++ (show id) ++ ")"
        IOVoid             -> "(mkCallback $! a" ++ (show id) ++ ")"
        IOType _           -> "(mkCallback $! a" ++ (show id) ++ ")"
        _                  -> "a" ++ (show id)
    resultConversion = resultConversion' (resultType hsType)
      where
      resultConversion' t = case t of
        ConvertType dat -> if null (fromConvert dat) then "" else (fromConvert dat) ++ " $ "
        _               -> ""
      
  
    argTypeList :: Type -> String
    argTypeList t = concat . map (\a-> showArgType a ++ " -> ") $ argTypes t
    signature :: Type -> String
    signature t = cConstr ++ (argTypeList t) ++ (showResType . resultType $ t)
    showArgType :: Type -> String
    showArgType (ConvertType dat)   = typeName dat
    showArgType IOVoid              = "JSFun (IO ())"
    showArgType (IOType t)          = "JSFun (IO (" ++ showArgType t ++ "))"
    showArgType (PlainType s)       = s
    showArgType (FunctionType f r)  = "JSFun (" ++ signature (FunctionType f r) ++ ")"
    showResType :: Type -> String
    showResType (ConvertType dat)   = typeName dat
    showResType IOVoid              = "IO ()"
    showResType (IOType t)          = "IO (" ++ showResType t ++ ")"
    showResType (PlainType s)       = s


javascriptFile :: [FFILine] -> String
javascriptFile = concat . map javascriptLine

javascriptLine (PlainLine _) = ""
javascriptLine (FFILine jsExp hsName cConstr hsType) =
  "function " ++ hsName ++ "JSImpl(" ++ (argumentList $ length (argTypes hsType)) ++ ioArg ++ ") {\n  "
  ++ if (resultType hsType) == IOVoid then jsCommand ++ ";\n  return [1,0];\n}\n" else "  return [1,0," ++ jsCommand ++ "];\n}\n"
  where
    argumentList :: Int -> String
    argumentList max = concatWith "," . map (\i -> "a" ++ (show i)) $ [1..max]
    concatWith :: String -> [String] -> String
    concatWith sep (x:y:xs) = x ++ sep ++ (concatWith sep (y:xs))
    concatWith _ (x:[])     = x
    concatWith _  _ = ""
    ioArg = if null (argTypes hsType) then "_" else  ",_"
    jsCommand = concat . map showExprPart $ jsExp
    showExprPart :: JSExprPart -> String
    showExprPart (StringPart s) = s
    showExprPart (ArgumentPart i) = "a" ++ (show i)
    showExprPart (RestArgPart) = concatWith "," . map showExprPart $ restArguments
    restArguments :: [JSExprPart]
    restArguments = let argId (ArgumentPart i) = i
                        argId _ = 0
                        highestArgument = maximum . map (argId) $ jsExp
                        numArguments = length (argTypes hsType)
                        missingArgs = if highestArgument >= numArguments then [] else [(highestArgument+1) .. numArguments]
                    in map (\i -> ArgumentPart i) missingArgs

-- helper functions
argTypes :: Type -> [Type]
argTypes t = case t of 
  FunctionType a r -> a:(argTypes r)
  _                -> []
  
resultType :: Type -> Type
resultType t = case t of
  FunctionType _ r -> resultType r
  r                -> r
