module Main where

import Data.SafeLength (safeLength)
import Data.Proxy (Proxy(..))

main :: IO ()
main = print $ safeLength (Proxy :: Proxy [Char]) ('a', 'b')
