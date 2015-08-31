import Data.Char (digitToInt)


asInt xs = loop 0 xs

loop acc [] = acc
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                  in loop acc' xs
                  

-- ffoldl _    acc []     = acc
-- ffoldl func acc (x:xs) = let acc' = func acc x
--                          in ffoldl func acc' xs

-- of
ffoldl _    initacc []     = initacc
ffoldl func initacc (x:xs) = ffoldl func (func initacc x) xs

-- Try these:

test1 = ffoldl (+) 0 [1,2,3,4,5,6,7,8,9,10]
test2 = ffoldl (+) 10 [1,2,3,4,5,6,7,8,9,10]
test3 = ffoldl (*) 1 [1,2,3,4,5,6,7,8,9,10]
test4 = ffoldl (++) "___" ["bob", "joe", "jim"]


ffoldr _    initacc []     = initacc
ffoldr func initacc (x:xs) = func x (ffoldr func initacc xs)

-- Chapter 4, Q1
asInt2 s = let timestenplus a b = a * 10 + digitToInt b
          in foldl timestenplus 0 s
        
asInt3 s
    | head s == '-' = -1 * asInt2 (tail s)
    | otherwise = asInt2 s
    
-- mypp xs ys = foldr (:) ys xs

(mypp ys) = fold (:) ys

