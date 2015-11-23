{-# LANGUAGE ExistentialQuantification #-}

module Eval where

import Data.Complex (realPart)
import Control.Monad.Error
import Debug.Trace

import LispVal
import LispError

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool   _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = do
    result <- eval pred
    case result of
        Bool False -> eval alt
        Bool True  -> eval conseq
        otherwise  -> throwError $ TypeMismatch "Predicate should return Bool" result
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badform = throwError $ BadSpecialForm "Unrecognized special form" badform

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe
    (throwError $ NotFunction "Unrecognized primitive function" func)
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
             ,("string->symbol", stringToSymbol)
             ,("=", numBoolBinop (==))
             ,("<", numBoolBinop (<))
             ,(">", numBoolBinop (>))
             ,("/=", numBoolBinop (/=))
             ,(">=", numBoolBinop (>=))
             ,("<=", numBoolBinop (<=))
             ,("&&", boolBoolBinop (&&))
             ,("||", boolBoolBinop (||))
             ,("string=?", strBoolBinop (==))
             ,("string<?", strBoolBinop (<))
             ,("string>?", strBoolBinop (>))
             ,("string<=?", strBoolBinop (<=))
             ,("string>=?", strBoolBinop (>=))
             ,("car", car)
             ,("cdr", cdr)
             ,("cons", cons)
             ,("eqv?", eqv)
             ,("eq?", eqv)
             ,("equal?", equal)
             ,("cond", cond)
             ,("case", caseFun)
             ]



cond :: [LispVal] -> ThrowsError LispVal
cond ((List [Atom "else", expr]):[]) = eval expr
cond ((List [test, expr]):xs) = do
    r' <- eval test
    case r' of
        (Bool True)  -> eval expr
        (Bool False) -> cond xs
        _        -> throwError $ TypeMismatch "a boolean expression" r'
cond [] = throwError $ NumArgs 1 []
cond (x:xs) = throwError $ TypeMismatch "(test expr)" x

caseFun :: [LispVal] -> ThrowsError LispVal
caseFun [_, List [Atom "else", expr]] = eval expr
caseFun (key:(List [List clauses,expr]):xs) = do
    comparisons <- mapM (\c -> eqv [key, c]) clauses
    let result = (or . map extractBool) comparisons
    case result of
        True  -> eval expr
        False -> caseFun (key:xs)
    where
        extractBool (Bool True) = True
        extractBool (Bool False) = False
caseFun [key] = return $ Atom "undefined"
caseFun [] = throwError $ NumArgs 2 []
caseFun (x:xs) = throwError $ TypeMismatch "((c1, c2 ...) expr)" x


car :: [LispVal] -> ThrowsError LispVal
car [(DottedList (x:xs) _)] = return x
car [(List (x:xs))] = return x
car [x]               = throwError $ TypeMismatch "Expected a list" x
car badArgs     = throwError $ NumArgs 1 badArgs

cdr :: [LispVal] -> ThrowsError LispVal
cdr [(List (x:xs))] = return $ List xs
cdr [(DottedList [_] tail)] = return tail
cdr [(DottedList (_:xs) x)] = return $ DottedList xs x
cdr [bad] = throwError $ TypeMismatch "Expected a list" bad
cdr badArgs = throwError $ NumArgs 1 badArgs

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List xs] = return $ List (x:xs)
cons [x, DottedList h t] = return $ DottedList (x:h) t
cons [x, y] = return $ DottedList [x] y
cons badArgList = throwError $ NumArgs 2 badArgList


areListsEqual :: [LispVal] -> [LispVal] -> ([LispVal] -> ThrowsError LispVal)
              -> ThrowsError Bool
areListsEqual l1 l2 cmp = do
    return $ (length l1 == length l2) &&
                    (all cmp' $ zipWith (\x y -> [x, y]) l1 l2)
    where cmp' x = case cmp x of
                     Left err -> False
                     Right (Bool val) -> val


eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = liftM Bool $ areListsEqual arg1 arg2 eqv
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList


equal :: [LispVal] -> ThrowsError LispVal
equal [(List xs), (List ys)] = liftM Bool $ areListsEqual xs ys equal
equal [(DottedList xs x), (DottedList ys y)] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [x, y] = do
    eqs <- mapM (unpackEquals x y) unpackers >>= (return . or)
    return . Bool $ eqs
    where
        unpackers = [ AnyUnpacker unpackNum
                    , AnyUnpacker unpackStr
                    , AnyUnpacker unpackBool
                    ]
equal bad = throwError $ NumArgs 2 bad

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

boolBinop :: (LispVal -> ThrowsError a)
          -> (a -> a -> Bool)
          -> [LispVal]
          -> ThrowsError LispVal
boolBinop unpacker op params
    | length params /= 2 = throwError $ NumArgs 2 params
    | otherwise = do
        left <- unpacker $ params !! 0
        right <- unpacker $ params !! 1
        return . Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op []     = throwError $ NumArgs 2 []
numericBinop op single@[_] = throwError $ NumArgs 2 single
numericBinop op params = mapM unpackNum params >>= (return . (Number . LInt) . foldl1 op)


data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals x y (AnyUnpacker unpack) =
    do
        x' <- unpack x
        y' <- unpack y
        return $ x' == y'
    `catchError` (const $ return False)


unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool b = throwError $ TypeMismatch "Expected a boolean" b

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Bool b)   = return $ show b
unpackStr (Number n) = do
    n' <- unpackNum (Number n)
    return $ show n'
unpackStr s = throwError $ TypeMismatch  "Expected a string" s

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return $
    case n of
        LRea d -> truncate d
        LInt i -> i
        LRat r -> truncate . fromRational $ r
        LCom c -> truncate . realPart $ c

unpackNum (String n) = let parsed = reads n :: [(Integer, String)]
                      in  if null parsed
                             then throwError . Default $ "String " ++ n ++ " is not numeric"
                             else (return . fst . head) parsed
unpackNum (List [n]) = unpackNum n
unpackNum bad = throwError $ TypeMismatch "Expected a number" bad

