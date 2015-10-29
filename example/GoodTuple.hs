module Main where

import Safe.Length (safeLength)
import Data.Proxy (Proxy(..))

main :: IO ()
main = print $ safeLength (Proxy :: Proxy (Char, Char)) ('a', 'b')

