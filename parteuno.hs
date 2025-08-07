import Partedos --importa partedos.hs

-- CAPITULO UNO

-- 1. Give another posible calculation for the result of double	(double	2).

double (double 2)= double 2 + double 2
                 = (2+2) + (2+2)
                 = (4) + (4)
                 = 8 


-- 2. Show that sum [x]	= x for any number x.

sum[x]= x + sum []
      = x + 0
      = x


{-3. Define a function product that produces the product of a list of numbers, and show using your definition that product [2,3,4] = 24.-}

producto [] = 1
producto (n:ns) = n * producto ns
product [2,3,4]

{-4. how should the definition of the function qsort be	modified so that it produces a reverse sorted versión of a list?-}

trosq [] = []; trosq (x:xs) = trosq [a | a <- xs, a >= x] ++ [x] ++ trosq [b | b <- xs, b < x]


 
{-5. What would be the effect of replacing <= by < in the original definition of qsort? Hint: consider the example qsort [2,2,3,1,1].-}

ordena de manera contraria, como lo pide el ejercicio anterior



-- CAPITULO DOS


--1. Work through the examples from this chapter using GHCi.

2+3*4

(2+3)*4

sqrt (3^2 + 4^2)

--Select the first element of a non-empty list:
head [1,2,3,4,5]

--Remove the first element of a non-empty list:
tail [1,2,3,4,5]

--Select the nth element of list (counting from cero):
 [1,2,3,4,5] !! 2

--Select the first n elements of  a list:
take 3 [1,2,3,4,5]

--Remove the first n elements from a list:
drop 3 [1,2,3,4,5]

-- Calculate the lenght of a list:
length [1,2,3,4,5]

-- Calculate the sum of a list of numbers:
sum [1,2,3,4,5]

-- Calculate the product of a list of numbers:
product [1,2,3,4,5]

--Append two lists:
 [1,2,3] ++ [4,5]

--Reverse a list:
reverse [1,2,3,4,5]

double x = x + x

quadruple x = double (double x)
quadruple 10

take (double 2) [1,2,3,4,5]

factorial n = product [1..n]

average ns = sum ns 'div' length ns

--:reload

factorial 10

average [1,2,3,4,5]


{-2. Parenthesise the following numeric expressions:
 2^3*4	
2*3+4*5	
2+3*4^5  -}

(2^3)*4
(2*3)+(4*5)
2+(3*(4^5))

{-3. The script below contains three syntactic errors. Correct these errors and then check that your
 script Works properly using GHCi.

 N = a ’div’ length xs
 where
 a = 10
 xs = [1,2,3,4,5]  -}

n = a ´div´ (length xs)
  where
     a = 10
     xs = [1,2,3,4,5] 


{-4. The library function last selects the last element of a non-empty list; for example, last
 [1,2,3,4,5] = 5. Show how the function last could be defined in terms of the other library
 functions introduced in this chapter. Can you think of another posible definition?-}

ultimo xs = drop ((length xs) -1) xs


{-5. The library function init removes the last element from a non-empty list; for example, init [1,2,3,4,5] = [1,2,3,4]. Show how init could similarly be defined in two different ways.-}

inicio xs =  reverse (tail (reverse xs))
inicio [1,2,3,4,5]

inicio2 xs = take (length xs -1) xs
inicio2 [1,2,3,4,5]



-- CAPITULO TRES

1. What are the	types of the following values?
 [’a’,’b’,’c’] :: [char]
 (’a’,’b’,’c’) :: (char,char,char)
[(False,’O’),(True,’1’)] ::[(bool,char)]
 ([False,True],[’0’,’1’]) :: ([bool],[char])
 [tail,	init,	reverse] :: [[a] ->[a]]


2. Write down definitions that have the following types; it does not matter what the definitions actually do as long as they are type correct.
 
 bools :: [Bool]
 bools :: [True,False,True]

 nums :: [[Int]]
 nums :: [[1],[2,3]]

 add :: Int -> Int -> Int -> Int
 add a b :: a + b

 copy :: a -> (a,a)
 copy "lupita" = ("lupita", "lupita")
 
 apply :: (a -> b) -> a -> b
 apply f x = f x


3. What are the types of the following functions?
 
 --Devuelve el segundo elemento de una lista
 second ::[a]-> a
 second xs = head (tail xs)
 
 --Intercambia los elementos de una tupla
 swap :: (a,b) -> (b,a)
 swap (x,y) = (y,x)
 
 -- Crea uan tupla a partir de dos valores
 pair :: a -> b -> (a,b)
 pair x y = (x,y)
 
 --Duplica un número (requiere que sea número)
 double :: Num a => a -> a
 double x = x*2
 
 --Verifica si una lista es palindromo (requiere comparar los elementos)
 palindrome :: Eq a => [a] -> bool
 palindrome xs = reverse xs == xs
 
 --Aplica una función dos veces 
 twice :: (a ->a) -> a ->a
 twice f x = f (f x)
 
 Hint: take care to include the necessary class constraints in the types if the functions are defined
 using overloaded operators.
 

4. Check your answers to the preceding three questions using GHCi.

second [21,30,26]

swap ("Saludos",True)

pair 3 's'

double 6

palindrome "reconocer"
palindrome"12321"

twice(*2) 7

5. Why is it not feasible in general for function types to be instances of the Eq class? When is it
feasible? Hint: two functions of the same type are equal if they always return equal results for 
equal arguments.

R1: porque para comparar funciones se requiere comprobar que siempre devuelven el mismo resultado para
    todos los valores

R2: Es viable cuando el dominio es finito y acotado. 


main :: IO ()
main = do

