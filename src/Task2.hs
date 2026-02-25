{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task2 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment

import Data.Ord qualified as Prelude
import Task1
import Prelude hiding (Ordering (..), compare, foldl, foldr)

-- * Type definitions

-- | Ordering enumeration
data Ordering = LT | EQ | GT
  deriving (Show, Eq)

-- | Binary comparison function indicating whether first argument is less, equal or
-- greater than the second one (returning 'LT', 'EQ' or 'GT' respectively)
type Cmp a = a -> a -> Ordering

-- * Function definitions

-- | Binary comparison function induced from `Ord` constraint
--
-- Usage example:
--
-- >>> compare 2 3
-- LT
-- >>> compare 'a' 'a'
-- EQ
-- >>> compare "Haskell" "C++"
-- GT
compare :: (Ord a) => Cmp a
compare a b = case Prelude.compare a b of
  Prelude.LT -> LT
  Prelude.EQ -> EQ
  Prelude.GT -> GT

-- | Conversion of list to binary search tree
-- using given comparison function
--
-- Usage example:
--
-- >>> listToBST compare [2,3,1]
-- Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf)
-- >>> listToBST compare ""
-- Leaf
listToBST :: Cmp a -> [a] -> Tree a
listToBST cmp = foldl (flip $ tinsert cmp) Leaf

-- | Conversion from binary search tree to list
--
-- Resulting list will be sorted
-- if given tree is valid BST with respect
-- to some 'Cmp' comparison.
--
-- Usage example:
--
-- >>> bstToList (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- [1,2,3]
-- >>> bstToList Leaf
-- []
bstToList :: Tree a -> [a]
bstToList = torder InOrder Nothing

-- | Tests whether given tree is a valid binary search tree
-- with respect to given comparison function
--
-- Usage example:
--
-- >>> isBST compare (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- True
-- >>> isBST compare (Leaf :: Tree Char)
-- True
-- >>> isBST compare (Branch 5 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- False
isBST :: Cmp a -> Tree a -> Bool
isBST cmp = isUniqueSorted . bstToList
  where
    isUniqueSorted [] = True
    isUniqueSorted [_] = True
    isUniqueSorted (x : xs@(y : _)) = (cmp x y) == LT && isUniqueSorted xs

-- | Searches given binary search tree for
-- given value with respect to given comparison
--
-- Returns found value (might not be the one that was given)
-- wrapped into 'Just' if it was found and 'Nothing' otherwise.
--
-- Usage example:
--
-- >>> tlookup compare 2 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Just 2
-- >>> tlookup compare 'a' Leaf
-- Nothing
-- >>> tlookup (\x y -> compare (x `mod` 3) (y `mod` 3)) 5 (Branch 2 (Branch 0 Leaf Leaf) (Branch 2 Leaf Leaf))
-- Just 2
tlookup :: Cmp a -> a -> Tree a -> Maybe a
tlookup cmp value = tlookup'
  where
    tlookup' Leaf = Nothing
    tlookup' (Branch nodeValue leftTree rightTree) = case (cmp value nodeValue) of
      LT -> tlookup' leftTree
      EQ -> Just nodeValue
      GT -> tlookup' rightTree

-- | Inserts given value into given binary search tree
-- preserving its BST properties with respect to given comparison
--
-- If the same value with respect to comparison
-- was already present in the 'Tree' then replaces it with given value.
--
-- Usage example:
--
-- >>> tinsert compare 0 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 (Branch 1 (Branch 0 Leaf Leaf) Leaf) (Branch 3 Leaf Leaf)
-- >>> tinsert compare 1 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf)
-- >>> tinsert compare 'a' Leaf
-- Branch 'a' Leaf Leaf
tinsert :: Cmp a -> a -> Tree a -> Tree a
tinsert cmp value = tinsert'
  where
    tinsert' Leaf = Branch value Leaf Leaf
    tinsert' (Branch nodeValue leftTree rightTree) = case (cmp value nodeValue) of
      LT -> Branch nodeValue (tinsert' leftTree) rightTree
      EQ -> Branch value leftTree rightTree
      GT -> Branch nodeValue leftTree (tinsert' rightTree)

-- | Deletes given value from given binary search tree
-- preserving its BST properties with respect to given comparison
--
-- Returns updated 'Tree' if the value was present in it;
-- or unchanged 'Tree' otherwise.
--
-- Usage example:
--
-- >>> tdelete compare 1 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 Leaf (Branch 3 Leaf Leaf)
-- >>> tdelete compare 'a' Leaf
-- Leaf
tdelete :: Cmp a -> a -> Tree a -> Tree a
tdelete cmp value = tdelete'
  where
    tdelete' Leaf = Leaf
    tdelete' (Branch nodeValue leftTree rightTree) = case (cmp value nodeValue) of
      LT -> Branch nodeValue (tdelete' leftTree) rightTree
      EQ -> merge leftTree rightTree
      GT -> Branch nodeValue leftTree (tdelete' rightTree)
    --
    merge tree Leaf = tree
    merge Leaf tree = tree
    merge leftTree rightTree = Branch (findMin rightTree) leftTree (deleteMin rightTree)
    --
    findMin Leaf = undefined
    findMin (Branch nodeValue Leaf _) = nodeValue
    findMin (Branch _ leftTree _) = findMin leftTree
    --
    deleteMin Leaf = Leaf
    deleteMin (Branch _ Leaf rightTree) = rightTree
    deleteMin (Branch nodeValue leftTree rightTree) = Branch nodeValue (deleteMin leftTree) rightTree

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ acc [] = acc
foldl f acc (x : xs) = foldl f (f acc x) xs
