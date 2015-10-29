{-# OPTIONS_GHC -fdefer-type-errors #-}
------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Stability   : experimental
-- Portability : POSIX
--
------------------------------------------------------------------------------
module Main where

import Data.Length
import Data.Proxy (Proxy(..))
import Test.Hspec
import Test.QuickCheck (property)
import Test.ShouldNotTypecheck (shouldNotTypecheck)

------------------------------------------------------------------------------
-- | Testing main
main :: IO ()
main = hspec spec

------------------------------------------------------------------------------
-- | Spec
spec :: Spec
spec = do
  describe "safeLength tests" $
           tests

tests = do
    it "length of a tuple is 1" $
        property $ safeLength (Proxy :: Proxy (Char, Char)) ('a', 'b') == 1
    it "Can't accidentally take the length of a tuple when we don't mean to" $
       shouldNotTypecheck (safeLength (Proxy :: Proxy [Char]) ('a', 'b'))
    it "Can't accidentally take the length of the wrong level of a nested list" $
       shouldNotTypecheck (safeLength (Proxy :: Proxy [[Char]]) (head [['a', 'b', 'c']] :: [Char]))
    it "Can take the length of a list." $
       property $ safeLength (Proxy :: Proxy [Char]) ['a', 'b'] == 2
