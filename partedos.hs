module Partedos where

saludo :: String
saludo = "Ejercicios capitulos 4, 5 y 6"

-- CAPITULO CUATRO

{- 1. Using library functions, define a function halve :: [a] -> ([a],[a])  that splits an even lengthed list 
into two halves. For example:
 >halve	[1,2,3,4,5,6]
 ([1,2,3],[4,5,6])
-}
splitAt :: Int -> [a] -> ([a],[a])
splitAt n xs = (take n xs, drop n xs)

mitad xs = splitAt (length xs `div` 2) xs
mitad [8,9]

{- 2. Define a function third :: [a] -> a that returns the third element in a list that contains at least this 
many elements using:
a. head	and	tail;
b. list	indexing!!;
c. pattern matching.
-}

third_ht xs = head(tail(tail xs))
third_ht [4,5,6]

third_li xs = xs !! 2
third_li [8,9,10]

third_pm (_:_:a) = a
third_pm [0,1,2]


{- 3. Consider a function safetail :: [a] -> [a] that behaves in the same way as tail except that it maps the 
empty list to itself rather than producing an error. Using tail and the function null :: [a] -> Bool that decides 
if a list is empty or not, define safetail using:

 a. a conditional expression;
 b. guarded equations;
 c. pattern matching.
-}
--condicional
safetail_ce xs = if null xs then [] else tail xs

--ecuación protegida
safetali_ge xs
  | null xs =[]
  | otherwise = tail xs

--concordancia de patrones
safetali_cp [] = []
safetail_cp (_:xs) = xs


{- 4. In a similar way to && in section 4.4, show how the disjunction operator || can be defined in four different 
ways using pattern matching

(||) :: Bool->Bool->Bool
True || True = True
True || False = True
False || True = True
False || False = False
-}

False || False = False
   _  ||  _  = True

False || b = b
True || _ = True

b || c | b ==c =b
       | otherwise =True


{- 5. Without using any other library functions or operators, show how the meaning of the following pattern 
matching definition for logical conjunction && can be formalised using conditional expressions:

True && True = True
 _   &&  _   = False

Hint: use two nested conditional expressions.
-}

a && b = if a then (if b then True else False) else False
 
{- 6. Do the same for the following alternative definition, and note the difference in the number of conditional 
expressions that are required:

 True && b = b
 False && _ = False
-}

a && b = if a then b else False

{- 7. Show how the meaning of the following curried function definition can be formalised in terms of lambda 
expressions:

 mult :: Int -> Int -> Int -> Int -> Int
 mult x and z = x*y*z
-}

\x -> (\y -> (\z ->  x*y*z))

{- 8. Luhn's algorithm is used to check bank card numbers for simple errors such as mistyping a digit, and proceeds 
as follows:

 -consider each digit as a separate number;
 -moving left, double every other number from the second last;
 -subtract 9 from each number that is now greater than 9;
 -add all the resulting numbers together;
 -if the total is divisible by 10, the card number is valid.

 Define a function luhnDouble :: Int -> Int that doubles a digit and subtracts 9 if the result is
greater than 9. For example:

 >luhnDouble 3
 6
 > luhnDouble 6
 3

 Using luhnDouble and the integer remainder function mod, define a function 
luhn :: Int -> Int -> Int -> Int -> Int -> Int -> Bool that decides if a four-digit bank card number is valid.
For example:
 > luhn 1 7 8 4
 True
 > luhn 4 7 8 3
 False
 In the exercises for chapter 7 we will consider a more general version of this function that accepts card numbers 
of any length.
-}

luhnDouble x = if 2*x > 9 then (2*x)-9 else 2*x

--luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0


-- CAPITULO CINCO


{- 1. Using a list comprehension, give an expression that calculates the sum 12 + 22 + ... 1002 of the first
one  hundred integer squares.-}

sum [x^2 | x <- [1..100]]

{- 2. Suppose that a coordinate grid of size m × n is given by the list of all pairs (x, y) of integers such
that 0 <= x <=m and 0 <= y <=n. Using a list comprehension, define anfunction grid :: Int -> Int-> [(Int,Int)] 
that returns a coordinate grid of a given size. For example:
>grid 1 2
>[(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
-}
 
grid a b = [(x, y) | x <- [0..a], y <- [0..b]]

{- 3. Using a list comprehension and the function grid above, define a function square :: Int -> [(Int,Int)]
that returns a coordinate square of size n, excluding the diagonal from	(0, 0) to (n,n). For example:
 >square	2
 >[(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)]
-}

square n = [(x, y) | (x, y) <- grid n n, x /= y]

 {- 4. In a similar way to the function length, show how the library function replicate :: Int -> a-> [a] 
that produces a list of identical elements can be defined using	a list comprehension. For example:
 >replicate 3 True
 >[True,True,True]
-}

replicate' n x = [x | _ <- [1..n]]

 {- 5. A triple (x, y, z) of positive integers is Pythagorean if it satisfies the equation x2 + y2 = z2. Using a
list comprehension with three generators, define a function pyths :: Int -> [(Int,Int,Int)]
that returns the list of all such triples whose components are at most a given limit. For example:
>pyths 10
>[(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
-}
 
pyths n =[(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 +y^2 == z^2 ]

{- 6. A positive integer is perfect if it equals the sum of all of its factors, excluding the number itself. 
Usinga list comprehension and the function factors, define a function perfects :: Int -> [Int] 
that returns the list of all perfect numbers up to a given limit. For example:
 >perfects 500
 >[6,28,496]
-}

perfects n = [x | x <- [1..n], x == sum (factors x)]


{- 7. Show how the list comprehension [(x,y) | x <- [1,2], y <- [3,4]] with two generators
can be re-expressed using two comprehensions with single generators.	
Hint: nest one comprehension within the other and make use of the library function concat :: [[a]] -> [a].
-}

concat [[(x, y) | y <- [3,4]] | x <- [1,2]]

-- 8. Redefine the function positions using then function find.

new_positions x xs = find x (zip xs [0..])
new_positions False [True, False, True, False] 

{- 9. The scalar product of two lists of integers xs and ys of length n is given by the sum of the products
of corresponding integers: In a similar manner to chisqr, show how a list comprehension can be used to define a function
scalarproduct :: [Int] -> [Int] -> Int that returns the scalar product of two lists. For example:
>scalarproduct [1,2,3] [4,5,6]
>32
-}

scalarproduct xs ys = sum [ x * y | (x, y) <- zip xs ys] 
scalarproduct [1,2,3] [4,5,6]

-- 10. Modify the Caesar cipher program to also handle upper-case letters.

import Data.Char (isLower, isUpper, ord, chr)

let2int c = ord c - ord 'a'
int2let n = chr (ord 'a' + n)

let2intUpper c = ord c - ord 'A'
int2letUpper n = chr (ord 'A' + n)

:{
shift_lU n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | isUpper c = int2letUpper ((let2intUpper c + n) `mod` 26)
  | otherwise = c
:}
encode_lU n xs = [shift_lU n x | x <- xs]
encode_lU 3 "haskell is FUN"



-- CAPITULO SEIS
{- 1. How does the recursive version of the factorial function behave if applied to a negative argument,
such as (-1)? Modify the definition to prohibit	negative arguments by adding a guard to	the recursive case. -}



{- 2. Define a recursive function sumdown :: Int -> Int that returns the sum of the	non-negative
 integers from a given value down to zero. For example, sumdown 3should return the	result
 3+2+1+0 = 6. -}

{- 3. Define the exponentiation	operator ^ for non-negative integers using the same	pattern	of recursion
 as the	multiplication operator	*, and show how	the expression	2 ^ 3 is evaluated	using your
 definition. -}

{-  4. Define a	recursive function euclid :: Int -> Int	-> Int that implements Euclid’s	algorithm
 for calculating the greatest common divisor of	two non-negative integers: if the two	numbers	are
 equal,	this number is the result; otherwise, the smaller number is subtracted from the	larger,	and the
 same process is then repeated.	For example:
 >euclid 6 27
 3  -}


{- 5. Using the	recursive definitions given in this chapter, nshow how length [1,2,3], drop	3
 [1,2,3,4,5], and init [1,2,3] are evaluated. -}


































