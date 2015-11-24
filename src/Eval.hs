{-# LANGUAGE ExistentialQuantification #-}

module Eval where

import Data.Complex (realPart)
import Control.Monad.Error
import Debug.Trace
import Data.Char (toUpper)

import LispData
import LispEnv

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool   _) = return val
eval env val@(Character _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = do
    result <- eval env pred
    case result of
        Bool False -> eval env alt
        Bool True  -> eval env conseq
        otherwise  -> throwError $ TypeMismatch "Predicate should return Bool" result
eval env (List (Atom "cond":xs)) = cond env xs
eval env (List (Atom "case":key:xs)) = do
    evaled <- eval env key
    caseFun env (evaled:xs)
eval env (List [Atom "set!", Atom var, form]) =
                eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
                eval env form >>= defineVar env var
eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func
eval env badform = throwError $ BadSpecialForm "Unrecognized special form" badform


cond :: Env -> [LispVal] -> IOThrowsError LispVal
cond env ((List [Atom "else", expr]):[]) = eval env expr
cond env ((List [test, expr]):xs) = do
    r' <- eval env test
    case r' of
        (Bool True)  -> eval env expr
        (Bool False) -> cond env xs
        _        -> throwError $ TypeMismatch "a boolean expression" r'
cond env [] = throwError $ NumArgs 1 []
cond env (x:xs) = throwError $ TypeMismatch "(test expr)" x

caseFun :: Env -> [LispVal] -> IOThrowsError LispVal
caseFun env [_, List [Atom "else", expr]] = eval env expr
caseFun env (key:(List [List clauses,expr]):xs) = do
   comparisons <- liftThrows $ mapM (\c -> eqv [key, c]) clauses
   let result = (or . map extractBool) comparisons
   case result of
       True  -> eval env expr
       False -> caseFun env (key:xs)
   where
       extractBool (Bool True) = True
       extractBool (Bool False) = False
caseFun env [key] = return $ Atom "undefined"
caseFun env [] = throwError $ NumArgs 2 []
caseFun env (x:xs) = throwError $ TypeMismatch "((c1, c2 ...) expr)" x


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
             ,("string-ci=?", strBoolBinop $ stringCIOp (==))
             ,("string-ci<?", strBoolBinop $ stringCIOp (<))
             ,("string-ci>?", strBoolBinop $ stringCIOp (>))
             ,("string-ci<=?", strBoolBinop $ stringCIOp (<=))
             ,("string-ci>=?", strBoolBinop $ stringCIOp (>=))
             ,("substring", substring)
             ,("car", car)
             ,("cdr", cdr)
             ,("cons", cons)
             ,("eqv?", eqv)
             ,("eq?", eqv)
             ,("equal?", equal)
             ,("make-string", makeString)
             ,("string-length", stringLength)
             ,("string-ref", stringRef)
             ,("string-append", stringAppend)
             ,("string->list", stringToList)
             ,("list->string", listToString)
             ]

listToString :: [LispVal] -> ThrowsError LispVal
listToString [List xs] =
    if all isChar xs
        then return $ String $ map unwrap xs
        else throwError $ Default $ "Cannot convert list with a non-char" where
            unwrap (Character c) = c
            isChar (Character c) = True
            isChar _             = False
listToString [x] = throwError $ TypeMismatch "List" x
listToString xs = throwError $ NumArgs 1 xs

stringToList :: [LispVal] -> ThrowsError LispVal
stringToList [String s] = return . List $ map Character s
stringToList [x] = throwError $ TypeMismatch "String" x
stringToList xs = throwError $ NumArgs 1 xs

stringAppend :: [LispVal] -> ThrowsError LispVal
stringAppend [String x, String y] = return $ String $ x ++ y
stringAppend [String _,x] = throwError $ TypeMismatch "String" x
stringAppend [x,_] = throwError $ TypeMismatch "String" x
stringAppend xs = throwError $ NumArgs 2 xs

substring :: [LispVal] -> ThrowsError LispVal
substring [String s, Number (LInt f), Number (LInt t)]
    | f' <= t' && t' <= length s = return . String . drop f' . take t' $ s
    | otherwise = throwError $ Default $
        "invalid indices specified (" ++ show f ++ ", " ++ show t ++ ")"
    where   f' = fromInteger f
            t' = fromInteger t
substring xs = throwError $ NumArgs 3 xs

stringCIOp :: (String -> String -> Bool) -> (String -> String -> Bool)
stringCIOp op x y = map toUpper x `op` map toUpper y

stringRef :: [LispVal] -> ThrowsError LispVal
stringRef [String s, Number (LInt i)]
    | i' >= 0 && i' < length s = return . Character $ s !! i'
    | otherwise = throwError . Default $ "invalid index " ++ show i
    where i' = fromInteger i

stringRef [s] = throwError $ NumArgs 2 [s]
stringRef [] = throwError $ NumArgs 2 []
stringRef (x:xs) = throwError $ TypeMismatch "String" x

stringLength :: [LispVal] -> ThrowsError LispVal
stringLength [String s] = return . Number . LInt . toInteger $ length s
stringLength [] = throwError $ NumArgs 1 []
stringLength (x:xs) = throwError $ TypeMismatch "String" x

makeString :: [LispVal] -> ThrowsError LispVal
makeString [Number (LInt k), Character c] =
    return $ String $ replicate (fromInteger k) c
makeString [k'@(Number (LInt k))] = makeString [k', Character ' ']
makeString [] = throwError $ NumArgs 1 []
makeString (x:xs) = throwError $ TypeMismatch "Integer" x





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

