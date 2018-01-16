{-# LANGUAGE ViewPatterns #-}

module Main where

import Lib

import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
                  (a -> b)
                  -> (b -> c)
                  -> f a
                  -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g. f) x)

functorCompose' :: (Eq (f c), Functor f) =>
                   f a
                   -> Fun a b
                   -> Fun b c
                   -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

f :: [Int] -> Bool
f x = functorIdentity x

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

main :: IO ()
main = do
  quickCheck f
  quickCheck (functorCompose' :: IntFC)
  quickCheck (functorIdentity :: Identity String -> Bool)
  quickCheck (functorCompose' :: Identity Int -> IntToInt -> IntToInt -> Bool)
  quickCheck (functorIdentity :: Pair String -> Bool)
  quickCheck (functorCompose' :: Pair Int -> IntToInt -> IntToInt -> Bool)
  quickCheck (functorIdentity :: Two Int String -> Bool)
  quickCheck (functorCompose' :: Two Int Int -> IntToInt -> IntToInt -> Bool)
  quickCheck (functorIdentity :: Three Int String Int -> Bool)
  quickCheck (functorCompose' :: Three Int Int Int -> IntToInt -> IntToInt -> Bool)
  quickCheck (functorIdentity :: Three' Int String -> Bool)
  quickCheck (functorCompose' :: Three' Int Int -> IntToInt -> IntToInt -> Bool)
  quickCheck (functorIdentity :: Four Int String Int String -> Bool)
  quickCheck (functorCompose' :: Four Int Int Int Int -> IntToInt -> IntToInt -> Bool)
  quickCheck (functorIdentity :: Four' Int String -> Bool)
  quickCheck (functorCompose' :: Four' Int Int -> IntToInt -> IntToInt -> Bool)
