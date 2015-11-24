module LispData where

import Data.Complex
import Data.Array
import Control.Monad.Error
import Text.ParserCombinators.Parsec.Error

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number LispNum
             | String String
             | Character Char
             | Bool Bool
             | Vector (Array Integer LispVal)
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)


data LispNum = LRea Double
             | LInt Integer
             | LRat Rational
             | LCom (Complex Double)
             deriving (Eq)

showVal :: LispVal -> String
showVal (String v)   = "\"" ++ v ++ "\""
showVal (Atom   v)   = v
showVal (Number v)   = show v
showVal (Character c) = "#\\" ++ [c]
showVal (Bool True)  = "#t"
showVal (Bool False) = "#f"
showVal (List v) = "(" ++ unwordsList v ++ ")"
showVal (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ showVal t ++ ")"
showVal (Vector a) = "#(" ++ (unwordsList . elems $ a) ++ ")"

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


data LispError
    = NumArgs Integer [LispVal]
    | TypeMismatch String LispVal
    | Parser ParseError
    | BadSpecialForm String LispVal
    | NotFunction String String
    | UnboundVar String String
    | Default String

showError :: LispError -> String
showError (UnboundVar msg vname) = msg ++ ": " ++ vname
showError (BadSpecialForm msg form) = msg ++ ": " ++ show form
showError (NotFunction msg func) = msg ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected
                        ++ " arguments, but found " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser err) = "Parse error at " ++ show err
showError (Default s)  = "Error: " ++ s

instance Show LispError where
    show = showError

instance Error LispError where
    noMsg = Default "An error has occured"
    strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val