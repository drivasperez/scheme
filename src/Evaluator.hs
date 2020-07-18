module Evaluator where

import Types
import Control.Monad.Except (throwError)

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _)   = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognised special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognised primitive function args" func) 
                        ($ args) 
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
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
             ,("list?", testType $ List [])
             ,("dottedlist?", testType $ DottedList [] $ Bool True)
             ,("number?", testType $ Number 0)
             ,("string?", testType $ String "")
             ,("bool?", testType $ Bool True)
             ]

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                                then throwError $ NumArgs 2 args
                                else do left <- unpacker $ args !! 0
                                        right <- unpacker $ args !! 1
                                        return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

testType :: LispVal -> [LispVal] -> ThrowsError LispVal
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
testType lv multiVal@_ = throwError $ NumArgs 1 multiVal

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
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
