module FunctorsAreStacked where

n = Nothing
w = Just "woohoo"
ave = Just "ave"
lms = [ave, n, w]
--List (Maybe String)

replaceWithP = const 'p'

--fmap :: Functor f => (m -> n) -> f m -> f n
--(List (Maybe String)) -> (Maybe String)
--const :: Char -> (Maybe String)
--fmap :: Functor g => (x -> y) -> g x -> g y
--(Maybe String) -> String
--const :: Char -> String

ha = Just ["Ha", "Ha"]
lmls = [ha, Nothing, Just []]
--List (Maybe (List String))
--[Maybe [[Char]]] -> [Char] for fmap
--[Maybe [[Char]]] -> [Maybe Char] for fmap.fmap
--[Maybe [[Char]]] -> [Maybe [Char]] for fmap.fmap.fmap
--[Maybe [[Char]]] -> [Maybe [[Char]]] for fmap.fmap.fmap.fmap
