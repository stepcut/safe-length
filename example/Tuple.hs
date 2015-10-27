module Main where

import Data.Length (safeLength)
import Data.Proxy (Proxy(..))

main :: IO ()
main = print $ safeLength (Proxy :: Proxy [Char]) ('a', 'b')
