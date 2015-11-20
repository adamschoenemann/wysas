module Eval where

import Data.Complex (realPart)
import Control.Monad.Error

import LispVal
import LispError

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool   _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badform = throwError $ BadSpecialForm "Unrecognized special form" badform

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe
    (throwError $ NotFunction "Unrecognized primitive function args" func)
    ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
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
             ,("null?", isNull)
             ,("symbol->string", symbolToString)
             ,("string->symbol", stringToSymbol)]

isSymbol, isNumber, isBoolean, isList, isNull :: [LispVal] -> ThrowsError LispVal

isSymbol [(Atom _)]          = return $ Bool True
isSymbol [(List (Atom _:_))] = return $ Bool True
isSymbol _                   = return $ Bool False

isString [(String _)]        = return $ Bool True
isString _                   = return $ Bool False

isNumber [(Number _)]        = return $ Bool True
isNumber _                   = return $ Bool False

isBoolean [(Bool _)]         = return $ Bool True
isBoolean _                  = return $ Bool False

isList [(List _)]            = return $ Bool True
isList _                     = return $ Bool False

isNull [(List [])]           = return $ Bool True
isNull _                     = return $ Bool False

symbolToString, stringToSymbol :: [LispVal] -> ThrowsError LispVal

symbolToString [(Atom name)] = return $ String name
symbolToString (bad:xs) = throwError $ TypeMismatch "symbol->string must be called on a symbol" bad

stringToSymbol [(String name)] = return $ Atom name
stringToSymbol (bad:xs) = throwError $ TypeMismatch "string->symbol must be called on a string" bad



numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op []     = throwError $ NumArgs 2 []
numericBinop op single@[_] = throwError $ NumArgs 2 single
numericBinop op params = mapM unpackNum params >>= (return . (Number . LInt) . foldl1 op)

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return $
    case n of
        LRea d -> truncate d
        LInt i -> i
        LRat r -> truncate . fromRational $ r
        LCom c -> truncate . realPart $ c

--unpackNum (String n) = let parsed = reads n :: [(Integer, String)]
--                       in  if null parsed
--                              then 0
--                              else fst $ head parsed
--unpackNum (List [n]) = unpackNum n
unpackNum bad = throwError $ TypeMismatch "Expected a number" bad

