module LispVal where

import Data.Complex
import Data.Array

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number LispNum
             | String String
             | Character Char
             | Bool Bool
             | Vector (Array Integer LispVal)

data LispNum = LRea Double
             | LInt Integer
             | LRat Rational
             | LCom (Complex Double)

showVal :: LispVal -> String
showVal (String v)   = "\"" ++ v ++ "\""
showVal (Atom   v)   = v
showVal (Number v)   = show v
showVal (Bool True)  = "#t"
showVal (Bool False) = "#f"
showVal (List v) = "(" ++ unwordsList v ++ ")"
showVal (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ showVal t ++ ")"
showVal (Vector a) = "(" ++ (unwordsList . elems $ a) ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showNum :: LispNum -> String
showNum (LRea x) = show x
showNum (LInt x) = show x
showNum (LRat x) = show x
showNum (LCom x) = show x

instance Show LispVal where
    show = showVal

instance Show LispNum where
    show = showNum