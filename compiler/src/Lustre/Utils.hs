module Lustre.Utils where

import GHC.Stack ( HasCallStack )

--------------------------------------------------------------------------------

zipExact :: HasCallStack => (a -> b -> c) -> [a] -> [b] -> [c]
zipExact f xs ys
  | length xs == length ys = zipWith f xs ys
  | otherwise              = error "zipExact: (length xs /= length ys)"

takeExact :: Int -> [a] -> [a]
takeExact n ls
  | n <= m    = take n ls
  | otherwise = error $ "takeExact: " ++ show n ++ " > " ++ show m
  where
    m = length ls

splitAtExact :: Int -> [a] -> ([a], [a])
splitAtExact n ls
  | n <= m    = splitAt n ls
  | otherwise = error $ "splitAtExact: " ++ show n ++ " > " ++ show m
  where
    m = length ls

allSame :: Eq a => [a] -> Bool
allSame xs = and (zipWith (==) xs (tail xs))

todo :: (Show a, HasCallStack) => a -> b
todo a = error $ "TODO: " ++ show a
