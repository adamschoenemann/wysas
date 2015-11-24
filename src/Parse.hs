module Parse where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Error
import Control.Monad
import Control.Applicative ((<*), (*>), (<*>))
import Data.Char (digitToInt)
import Data.Complex
import Data.Array
import Numeric

import Debug.Trace

import LispData

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseNumber :: Parser LispVal
parseNumber = do
    num <- lookAhead $ many (noneOf " ")
    case num of
        ('#':num') -> parseInteger
        num' -> case () of
            _ | '.' `elem` num' -> parseReal
              | '+' `elem` num' && last num' == 'i' -> parseComplex
              | '/' `elem` num' -> parseRational
              | otherwise -> parseInteger

parseReal :: Parser LispVal
parseReal = do
    digits <- many (digit <|> char '.')
    return $ Number $ LRea ((fst . head . readFloat) digits)

parseComplex :: Parser LispVal
parseComplex = do
    real <- (many1 digit) <* (char '+')
    img  <- (many1 digit) <* (char 'i')
    let reader = (fst . head . readFloat)
    return $ Number $ LCom $ (reader real) :+ (reader img)


parseRational :: Parser LispVal
parseRational = do
    ops <- sepBy1 (many digit) (char '/')
    let [num, denom] = map (read) ops :: [Integer]
    return $ Number $ LRat $ (toRational num) / (toRational denom)

parseInteger :: Parser LispVal
parseInteger = (parseDecimal) <|> (parseFormat) where
    parseFormat = do
        r <- try $ char '#' >> oneOf "bodx"
        let (reader, permitted) = getFormat r
        ds <- many $ permitted
        return $ (Number . LInt . reader) ds
        where
            getFormat 'd' = (read, digit)
            getFormat 'x' = (fst . head . readHex, digit <|> oneOf (['a'..'f'] ++ ['A'..'F']))
            getFormat 'o' = (fst . head . readOct, oneOf ['0'..'7'])
            getFormat 'b' = (readBin, oneOf "01")
            readBin = toInteger . foldl (\acc x -> acc * 2 + digitToInt x) 0

    parseDecimal = do
        ds <- try (many1 digit)
        (return . Number . LInt . read) ds

parseCharacter :: Parser LispVal
parseCharacter = do
    try $ char '#' >> char '\\'
    cs <- (string "space" <|> string "newline") <|> (anyChar >>= return . (:[]))
    return $ case cs of
                "space" -> Character ' '
                "newline" -> Character '\n'
                [cs'] -> Character cs'

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


parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseBackquote :: Parser LispVal
parseBackquote = do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]

parseVector :: Parser LispVal
parseVector = do
    try (char '#' >> char '(')
    exprs <- sepBy parseExpr spaces
    char ')'
    let arr = listArray (0, toInteger $ length exprs - 1) exprs
    return $ Vector arr

parseListOrDottedList :: Parser LispVal
parseListOrDottedList = do
    char '('
    x <- try parseList <|> parseDottedList -- try --> backtrack on fail, dont consume
    char ')'
    return x


parseExpr :: Parser LispVal
parseExpr =  parseString
         <|> parseVector
         <|> parseCharacter
         <|> parseNumber
         <|> parseAtom
         <|> parseListOrDottedList
         <|> parseQuoted


readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

