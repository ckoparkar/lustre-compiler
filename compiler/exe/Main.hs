module Main where

import System.Environment ( getArgs )
import Lustre.Compiler ( compileCmd )

--------------------------------------------------------------------------------

main :: IO ()
main = getArgs >>= compileCmd
