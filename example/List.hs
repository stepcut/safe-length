module Main where

import Data.Proxy (Proxy(..))
import Safe.Length (safeLength)

main :: IO ()
main = print $ safeLength (Proxy :: Proxy [Char]) ['a', 'b']
