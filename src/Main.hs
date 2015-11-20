module Main (
    module Eval, module Parse, main
) where

import System.Environment
import Text.ParserCombinators.Parsec (parse)

import Parse
import Eval

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
