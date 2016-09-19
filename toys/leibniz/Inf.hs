module Inf where

import Prelude hiding ((.))

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g x = f (g x)   -- [.]


ex0 :: Int
ex0 = ((1 +) . (2 +)) 3



