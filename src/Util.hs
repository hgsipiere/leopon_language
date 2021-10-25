module Util where

import Control.Applicative

liftA4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e 
liftA4 f w x y z = f <$> w <*> x <*> y <*> z
