zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = let smaller = quicksort' (filter (<= x) xs)
                        bigger = quicksort' (filter (> x) xs)
                    in smaller ++ [x] ++ bigger 

largestDivisible :: (Integral a) => a
largestDivisible = let predicate x = (x `mod` 3829 == 0) in head (filter predicate [100000, 99999..])

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain x
  | even x    = x : chain (x `div` 2)
  | otherwise = x : chain (3 * x + 1)

numLongChains :: Int
numLongChains = length (filter p (map chain [1..100]))
  where p xs = length xs > 15

elem' :: (Eq a) => a -> [a] -> Bool
elem' e xs = foldl (\acc x -> acc || (x == e)) False xs

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> (f x) : acc) [] xs

