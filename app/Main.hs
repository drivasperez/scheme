module Main where

import Parser
import Evaluator

import System.Environment

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
