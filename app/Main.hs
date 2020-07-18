module Main where

import Parser
import Evaluator
import Types
import Text.ParserCombinators.Parsec (ParseError)
import Control.Monad.Except

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled
