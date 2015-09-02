#!/usr/bin/env runhaskell

import qualified Data.Function.Memoize as Memoize

rep xs n
	| n == 1 = xs 
    | otherwise = xs ++ (rep xs (n - 1))


score_pair a b 
	| a == ' ' && b == ' ' = 0
    | a == '-' = -4
    | b == '-' = -4
    | a == b = 2
    | otherwise = -3

score_aln a b 
	| length a == 1 && length b == 1 = score_pair (head a) (head b)
	| otherwise = score_aln (tail a) (tail b) + score_pair (head a) (head b) 

type Unalign = String
type Align = String
type Score = Int

data Answer = Answer {
      a :: Unalign,
      b :: Unalign,
      aaln :: Align,
      baln :: Align,
      score :: Score}
      deriving (Show)

base_case a b 
	| a == " " && b == " " = Answer " "   " "   " "   " "  0
	| a == " " = let 
	                a_aln = (rep "-" ((length b) - 1)) ++ " "
	             in
                    Answer a b a_aln b (score_aln a_aln b)
	| otherwise = let
	                b_aln = (rep "-" ((length a) - 1)) ++ " "
	             in
	                Answer a b a b_aln (score_aln a b_aln)
	                
max_answer a b c 
	| score a > score b && score a > score c = a
	| score b > score a && score b > score c = b
	| otherwise = c
	


-- wooohoo recursion
global_aln a b
	| length a == 1 || length b == 1 = base_case a b
	| otherwise = let {b_a = head a;
	                   b_b = head b;
	                   s_a = tail a;
	                   s_b = tail b;
	                   
	                   left = global_aln_mem s_a b;
	                   center = global_aln_mem s_a s_b;
	                   right = global_aln_mem a s_b;
	                   
	                   left_aln_a = b_a : (aaln left);
	                   left_aln_b = '-' : (baln left);
	                   center_aln_a = b_a : (aaln center);
	                   center_aln_b = b_b : (baln center);
	                   right_aln_a = '-' : (aaln right);
	                   right_aln_b = b_b : (baln right);
	                   
	                   answer_left = Answer   a   b   left_aln_a   left_aln_b   (score left + score_pair b_a  '-');
	                   answer_center = Answer   a   b   center_aln_a  center_aln_b  (score center + score_pair b_a b_b);
	                   answer_right = Answer   a   b   right_aln_a  right_aln_b   (score right + score_pair '-' b_b)}
	              in
	                   max_answer answer_left answer_center answer_right

global_aln_mem = Memoize.memoize2 global_aln


main = do
          putStrLn (show (global_aln "TCGATCGATCGATCGATCGA " "TCGGTCCGTCATCATCGA "))

