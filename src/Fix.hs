-- This module is yanked staright from vt-2016 example project. 
module Fix where

newtype Fix f = Fix { unFix :: f (Fix f) }

cata :: Functor f => (f a -> a) -> Fix f -> a
cata f = f . fmap (cata f) . unFix

ana :: Functor f => (a -> f a) -> a -> Fix f
ana f = Fix . fmap (ana f) . f

bottomUp :: Functor f => (Fix f -> Fix f) -> Fix f -> Fix f
bottomUp f = f . Fix . fmap (bottomUp f) . unFix

topDown :: Functor f => (Fix f -> Fix f) -> Fix f -> Fix f
topDown f = Fix . fmap (topDown f) . unFix . f
