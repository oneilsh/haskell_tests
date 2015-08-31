#!/usr/bin/env runhaskell


myDrop n xs = if n <= 0 || null xs
	then xs
	else myDrop (n - 1) (tail xs)

fib n = if n == 1 || n == 2
	then 1
	else fib (n - 1) + fib (n - 2)
	
data BookInfo = Book Int String [String]
				deriving (Show)
				
data MagazineInfo = Magazine Int String [String]
					deriving (Show)
					
myInfo = Book 989835 "APCB" ["Shawn T ONeil"]

type Year = Int
type Journal = String
type Title = String
type Authors = [String]

data ArticleInfo = ArticleInfo Year Journal Title Authors
					deriving (Show)

data Color = Yellow
			deriving (Show)
			
fib2 1 = 1
fib2 2 = 1
fib2 n = fib2 (n - 1) + fib2 (n - 2)

data List a = Cons a (List a)
			| Nil
			deriving (Show)
			
--data Tree a = Node a (Tree a) (Tree a)
--			| Empty
--			deriving (Show)

-- parameterized binary tree	
data Tree2 a = Tree2 {
	nodedata :: a,
	left :: Tree2 a,
	right :: Tree2 a
	} | Empty
	deriving (Eq, Show)

insert Empty a = Tree2 a Empty Empty
insert t a = if a < (nodedata t)
			then	if left t == Empty
						then Tree2 (nodedata t) (Tree2 a Empty Empty) (right t)
						else Tree2 (nodedata t) (insert (left t) a) (right t)
			else
					if right t == Empty
						then Tree2 (nodedata t) (left t) (Tree2 a Empty Empty)
						else Tree2 (nodedata t) (left t) (insert (right t) a)


							

		
t = Empty
t2 = insert t "G"
t3 = insert t2 "A"
t4 = insert t3 "T"
t5 = insert t4 "R"

-- chapter 3 exercises
-- 1

-- with guard patterns (sort of "if statements"); otherwise is True
-- signature for # 2, but I'm not sure how I'm supposed to know this other
-- than using :type countels beforehand
countels :: (Eq t, Num a) => [t] -> a
countels l
   | l == [] = 0
   | otherwise = 1 + (countels (tail l))

sumels l
   | l == [] = 0
   | otherwise = (head l) + (sumels (tail l))

-- Q3
meanels l
   | l == [] = 0
   | otherwise = (sumels l) / (countels l)

-- Q4
palindrome l
	| l == [] = []
	| otherwise = [(head l)] ++ (palindrome (tail l)) ++ [(head l)]

-- Q5; hmmmmmmmmmmm

-- Q6;
odds l 
	| l == [] = []
	| length l == 1 = [(head l)]
	| length l == 2 = [(head l)]
	| otherwise = [(head l)] ++ (odds (tail (tail l)))

evens l
	| l == [] = []
	| length l == 1 = []
	| length l == 2 = tail l
	| otherwise = odds (tail l)
	
merge l1 l2 comparator
	| l1 == [] = l2
	| l2 == [] = l1
	| otherwise = let {firstl1 = head l1;
                       restl1 = tail l1;
                       firstl2 = head l2;
                       restl2 = tail l2 }
	              in if (comparator firstl1) < (comparator firstl2)
	                 then [firstl1] ++ merge restl1 l2 comparator
	                 else [firstl2] ++ merge l1 restl2 comparator

mergesort l comparator
	| l == [] = []
	| length l == 1 = l
	| otherwise = let left = odds l
	                  right = evens l
	              in merge (mergesort left comparator) (mergesort right comparator) comparator

-- eg (Just from above a type of maybe, an "identity")
-- mergesort [1, 8, -6, 7, 0, -2, 3, 4, 9, 5] abs
-- mergesort [1, 8, -6, 7, 0, -2, 3, 4, 9, 5] Just
-- mergesort ["bo", "cathy", "jimmy", "zed", "charlotte"] length
-- mergesort ["bo", "cathy", "jimmy", "zed", "charlotte"] Just


-- Q7/8 (weird one)
joinlist l sep 
    | l == [] = []
    | length l == 1 = head l
    | otherwise = head l ++ [sep] ++ (joinlist (tail l) sep)




-- Q9
-- Didn't end up using this one, but still a cool exercise
mymax l 
    | l == [] = -10000000
    | length l == 1 = head l
    | otherwise = let {restmax = mymax (tail l);
                       first = head l}
                  in if first > restmax
                     then first
                     else restmax
                        

treeheight t
    | t == Empty = 0
    | otherwise = let {leftheight = treeheight (left t);
                       rightheight = treeheight (right t)}
                  in if leftheight > rightheight
                     then leftheight + 1
                     else rightheight + 1

-- Q10
data Direction = StraightTurn
        | LeftTurn
        | RightTurn
     deriving (Eq, Show)
     
-- Q11       
data Point2d x y = Point2d {
    x :: x,
    y :: y }
    deriving (Eq, Show)

-- straight from wiki on Graham's scan...
turn p1 p2 p3 = let {z1 = ((x p2) - (x p1))*((y p3) - (y p1));
                     z2 = ((y p2) - (y p1))*((x p3) - (x p1));
                     z = z1 - z2}
                in if z /= 0
                   then if z < 0
                        then RightTurn
                        else LeftTurn
                   else StraightTurn  

-- Q12
points_to_dirlist l
    | length l < 3 = []
    | otherwise = let {first = head l;
                       second = head (tail l);
                       third = head (tail (tail l)) }
                  in (turn first second third) : points_to_dirlist (tail l)


plist = [(Point2d 0 0),
         (Point2d 1 1),
         (Point2d 2 0),
         (Point2d 2 1),
         (Point2d 3 1),
         (Point2d 3 2),
         (Point2d 4 (-2)),
         (Point2d 5 0)]

-- Q13 (using monotone scan for lower and upper hulls)
lowerhull l 
    | length l < 3 = l
    | otherwise = let {sortedl = mergesort l x;
                       lowerhullrest = lowerhull (tail l)}
                  in if turn (head l) (head lowerhullrest) (head (tail lowerhullrest)) == RightTurn
                     then lowerhull ((head l) : (tail lowerhullrest))
                     else (head l) : lowerhullrest

upperhull l
    | length l < 3 = l
    | otherwise = let {sortedl = mergesort l x;
                       upperhullrest = upperhull (tail l)}
                  in if turn (head l) (head upperhullrest) (head (tail upperhullrest)) == LeftTurn
                      then upperhull ((head l) : (tail upperhullrest))
                      else (head l) : upperhullrest

rev l 
    | length l < 2 = l
    | otherwise = rev (tail l) ++ [head l]

convexhull l = (upperhull l) ++ (tail (rev (lowerhull l)))
