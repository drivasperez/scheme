{-# LANGUAGE ExistentialQuantification #-}

module Evaluator where

import Types
import Control.Monad.Except (throwError, catchError, liftM)

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _)   = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = 
                     do result <- eval pred
                        case result of
                          Bool False -> eval alt
                          _          -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognised special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognised primitive function args" func) 
                        ($ args) 
                        (lookup func primitives)

primitives :: [(String, LispFunc)]
primitives = [
              ("+", numericBinop (+))
             ,("-", numericBinop (-))
             ,("*", numericBinop (*))
             ,("/", numericBinop div)
             ,("mod", numericBinop mod)
             ,("quotient", numericBinop quot)
             ,("remainder", numericBinop rem)
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
             ,("eq?", eqv)
             ,("eqv?", eqv)
             ,("equal?", equal)
             ,("list?", testType $ List [])
             ,("dottedlist?", testType $ DottedList [] $ Bool True)
             ,("number?", testType $ Number 0)
             ,("string?", testType $ String "")
             ,("bool?", testType $ Bool True)
             ]

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackers :: [Unpacker]
unpackers = [ AnyUnpacker unpackNum
            , AnyUnpacker unpackStr
            , AnyUnpacker unpackBool
            ] 

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> LispFunc
boolBinop unpacker op args = if length args /= 3
                                then throwError $ NumArgs 2 args
                                else do left <- unpacker $ args !! 0
                                        right <- unpacker $ args !! 1
                                        return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool


testType :: LispVal -> LispFunc
testType lv [] = throwError $ NumArgs 1 []
testType lv [t] = return x 
  where x = case (lv, t) of
              (Atom _, Atom _)                 -> Bool True
              (List _, List _)                 -> Bool True
              (DottedList _ _, DottedList _ _) -> Bool True
              (Number _, Number _)             -> Bool True
              (String _, String _)             -> Bool True
              (Bool _, Bool _)                 -> Bool True
              (_, _)                           -> Bool False
testType lv multiVal = throwError $ NumArgs 1 multiVal

car :: LispFunc
car [List (x:xs)]         = return x
car [DottedList (x:xs) _] = return x
car [badArg]              = throwError $ TypeMismatch "pair" badArg
car badArgList            = throwError $ NumArgs 1 badArgList

cdr :: LispFunc
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: LispFunc
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List (x : xs)
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2 
     return $ unpacked1 == unpacked2
     `catchError` (const $ return False)


eqv :: LispFunc
eqv [(Bool arg1), (Bool arg2)]     = return $ Bool (arg1 == arg2)
eqv [(Number arg1), (Number arg2)] = return $ Bool (arg1 == arg2)
eqv [(String arg1), (String arg2)] = return $ Bool (arg1 == arg2)
eqv [(Atom arg1), (Atom arg2)]     = return $ Bool (arg1 == arg2)
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]     = return $ Bool $ (length arg1 == length arg2) &&
                                                 (all eqvPair $ zip arg1 arg2)
     where eqvPair (x1, x2) = case eqv [x1, x2] of
                                Left err -> False
                                Right (Bool val) -> val
eqv [_, _]                        = return $ Bool False
eqv badArgList                    = throwError $ NumArgs 2 badArgList

equal :: LispFunc
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) unpackers 
  eqvEquals       <- eqv [arg1, arg2] 
  return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList



numericBinop :: (Integer -> Integer -> Integer) -> LispFunc
numericBinop op []            = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                           if null parsed
                              then throwError $ TypeMismatch "number" $ String n
                              else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $  show s
unpackStr notString  = throwError $ TypeMismatch "string" notString 

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch  "boolean" notBool
