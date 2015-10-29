module Safe.Length where

import Data.Proxy (Proxy(..))

safeLength :: (Foldable f) => Proxy (f a) -> f a -> Int
safeLength _ f = length f

list :: Proxy [a]
list = Proxy
