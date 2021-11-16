-- Лабораторна робота №3
-- студента групи КН-32
-- підгрупи 1
-- Бури Максима
-- Варіант №7

-- Мета: Набути досвiду визначення та використання функцiй вищого порядку.

-- Завдання 1. Визначити частоту кожного елемента списку, 
-- напр.: "aaabbcaadddd"⇒[(’a’,5), (’b’,2), (’c’,1), (’d’,4)].

fun :: (Eq a) =>  [a] -> [(a, Int)]
fun []  = []
fun [x] = [(x, 1)]
fun (x:xs) = (x, count (x:xs) x): fun (deleteDup xs x) 
  where 
  count :: (Eq a) => [a] -> a -> Int 
  count [] _ = 0
  count (x:xs) a 
    |x == a = 1 + count xs a 
    |otherwise = count xs a 
  deleteDup :: (Eq a) => [a] -> a -> [a]
  deleteDup [] _ = [] 
  deleteDup (x:xs) a
    |x == a = deleteDup xs a
    |otherwise = x : deleteDup xs a 



-- Завдання 2. Знайти простi дiльники числа.

divisors :: Int-> [Int]
divisors k = onlyPrime (divisors' 2 k)
  where
    divisors' n k 
     | n*n > k = [k]
     | n*n == k = [n, k]
     | k `mod` n == 0 = (n:(k `div` n):result)
     | otherwise = result
      where result = divisors' (n+1) k
    
    onlyPrime :: [Int] -> [Int]
    onlyPrime [] = []
    onlyPrime (x:xs) 
      | is_prime x = x : onlyPrime (xs)
      | otherwise = onlyPrime xs

    is_prime :: Int -> Bool
    is_prime 1 = False
    is_prime 2 = True
    is_prime n 
      | (length [x | x <- [2 .. n-1], mod n x == 0]) > 0 = False
      | otherwise = True


-- Висновок: В ході виконання лабораторної роботи, я набув досвiду визначення та використання функцiй вищого порядку.