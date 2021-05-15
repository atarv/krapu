{-|
Module         : Helpers
Description    : Some miscellaneous helper functions
Copyright      : (c) Aleksi Tarvainen, 2021
License        : BSD3
Maintainer     : a.aleksi.tarvainen@student.jyu.fi
-}
module Helpers where

import           Control.Applicative
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Data.Map.Strict                ( Map )

import qualified Data.Map.Strict               as Map


-- | @onHeadOf f xs@ returns the list @xs@ with function @f@ applied on it's 
-- first element leaving rest of the list as is.
--
-- >>> (+ 1) `onHeadOf` (0 :| [2, 3])
-- 1 :| [2,3]
onHeadOf :: (a -> a) -> NonEmpty a -> NonEmpty a
onHeadOf fn (x :| xs) = fn x :| xs

-- | Lookup through a foldable structure containing maps. Stops once a match 
-- is found, so this works for infinite structures too.
--
-- >>> lookupMaps "k" $ [Map.fromList []] ++ repeat (Map.fromList [("k", 1)])
-- Just 1
lookupMaps :: (Foldable t, Ord k) => k -> t (Map k a) -> Maybe a
lookupMaps key = foldr (\map' found -> Map.lookup key map' <|> found) Nothing

