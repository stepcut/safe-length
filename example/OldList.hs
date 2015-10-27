module Main where

import GHC.OldList as OldList

main :: IO ()
main = print $ OldList.length (head [['a','b','c']])


