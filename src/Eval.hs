module Eval where

import Data.Complex (realPart)

import LispVal

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool   _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+))
             ,("-", numericBinop (-))
             ,("*", numericBinop (*))
             ,("/", numericBinop div)
             ,("mod", numericBinop mod)
             ,("quotient", numericBinop quot)
             ,("remainder", numericBinop rem)
             ,("symbol?", isSymbol)
             ,("string?", isString)
             ,("number?", isNumber)
             ,("list?", isList)
             ,("boolean?", isBoolean)
             ,("null?", isNull)]

isSymbol, isNumber, isBoolean, isList, isNull :: [LispVal] -> LispVal

isSymbol [(Atom _)] = Bool True
isSymbol [(List (Atom _:_))] = Bool True
isSymbol _        = Bool False

isString [(String _)] = Bool True
isString _          = Bool False

isNumber [(Number _)] = Bool True
isNumber _          = Bool False

isBoolean [(Bool _)] = Bool True
isBoolean _        = Bool False

isList [(List _)]  = Bool True
isList _         = Bool False

isNull [(List [])] = Bool True
isNull _         = Bool False



numericBinop :: (Integer -> Integer -> Integer) -> ([LispVal] -> LispVal)
numericBinop op params = Number . LInt $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) =
    case n of
        LRea d -> truncate d
        LInt i -> i
        LRat r -> truncate . fromRational $ r
        LCom c -> truncate . realPart $ c

unpackNum (String n) = let parsed = reads n :: [(Integer, String)]
                       in  if null parsed
                              then 0
                              else fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

