{-# LANGUAGE TypeSynonymInstances #-}


module Utilities where

import Data.Map.Internal.Debug (showTreeWith)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (inits,intercalate)
import Data.Maybe 


class IsoListCont c where
    toList :: Ord a => c a -> [a]
    fromList :: Ord a => [a] -> c a

type List a = [a]

instance IsoListCont Set where
    toList = Set.toList
    fromList = Set.fromList

-- ordId :: Ord a => a -> a
-- ordId a = a

instance IsoListCont [] where
    toList = id
    fromList = id

squishSetList :: Ord a => [Set a] -> Set a 
squishSetList = foldl Set.union Set.empty 

compose' :: [a -> a] -> a -> a
compose' [] a     = a
compose' (f:fs) a = f (compose fs a)
-- compose' fs a = foldr ($) a 

compose :: [a -> a] -> a -> a
compose [] a     = a
compose (f:fs) a = compose fs (f a)

(<&>) :: Ord a => Set a -> Set a -> Set a
(<&>) = Set.union


showSet :: Show a => Set a -> String
showSet s = "{ "++(intercalate ", " $ Set.toList s)++" }"

fsetify :: Ord b => (a -> [b]) -> a -> Set b
fsetify f = Set.fromList . f


showTree t = showTreeWith (\k x -> show k ++ " : " ++ showSet x) False True t
putsTree = putStrLn . showTree