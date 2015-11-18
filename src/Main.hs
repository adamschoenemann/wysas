module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Data.Char (digitToInt)
import Numeric

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Character Char
             | Float Float
             | Bool Bool
             deriving (Show)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseCharacter :: Parser LispVal
parseCharacter = do
    _ <- char '#' >> char '\\'
    cs <- (string "space" <|> string "newline") <|> (anyChar >>= return . (:[]))
    return $ case cs of
                "space" -> Character ' '
                "newline" -> Character '\n'
                [cs'] -> Character cs'


parseFloat :: Parser LispVal
parseFloat = do
    digits <- many digit >> (char '.')
    return Float (readFloat digits)

parseString :: Parser LispVal
parseString = do
    let escaped = char '\\' >> oneOf "nrt\"\\"
    char '"'
    x <- many $ (escaped <|> noneOf "\"")
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest  <- many (letter <|> symbol <|> digit)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do
    f <- char '#' <|> digit
    case f of
        '#' -> do
            r <- oneOf "bodx"
            let reader = getReader r
            ds <- many anyChar
            return $ (Number . reader) ds
        d   -> liftM (Number . read . (d:)) (many digit)
    where
        getReader :: Char -> (String -> Integer)
        getReader 'd' = read
        getReader 'x' = fst . head . readHex
        getReader 'o' = fst . head . readOct
        getReader 'b' = readBin
        readBin = toInteger . foldl (\acc x -> acc * 2 + digitToInt x) 0

parseExpr :: Parser LispVal
parseExpr =  parseAtom
         <|> parseString
         <|> parseCharacter
         <|> parseNumber


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match " ++ show err
    Right val -> "Found Value: " ++ show val

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn $ readExpr "hell\"o"
    return ()