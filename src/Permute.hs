module Permute where

{-# LANGUAGE FlexibleContexts  #-}

import qualified Data.Map as M

getOrElse :: Maybe a -> a -> a
getOrElse (Just a) _ = a
getOrElse Nothing a = a

lookupFallBack :: Ord a => M.Map a a -> a -> a
lookupFallBack m a = let replacement = M.lookup a m 
                      in getOrElse replacement a

applyMap :: Ord a => M.Map a a -> [a] -> [a]
applyMap m = map (lookupFallBack m)

--takes a permutation in cycle notation, applies to a list of things
permute :: Ord a => [a] -> [a] -> [a]
permute cycle items = let pairs = zip cycle (tail cycle)
                          pmap = M.fromList pairs
                       in applyMap pmap items

compose :: [a->a] -> a -> a
compose = foldl (.) id

--takes a list of permutions in cycle notation, composes them, applies them to a list
permutes :: Ord a => [[a]] -> [a] -> [a]
permutes cycles items = let pmappers = map permute cycles
                            composed = compose pmappers
                         in composed items

rotateL :: [a] -> [a]
rotateL [] = []
rotateL [h] = [h]
rotateL (h:t) = t ++ [h]