import Data.List (nub)
-- Лабораторна робота №2
-- студента групи КН-32
-- підгрупи 1
-- Бури Максима
-- Варіант №7

-- Мета: Набути досвiду визначення рекурсивних функцiй, використання механiзму
-- зiставлення зi зразком i роботи з кортежами та списками.

-- Завдання 1. Послiдовнiсть тотожних елементiв списку замiнити одним елементом,
-- напр.: [1,1,1,5,5,3, 1,1,222,222,222,222] ⇒ [1,5,3,1,222].

--a)
unique1 :: (Eq a) => [a] -> [a]
unique1 [] = []
unique1 (x:xs)
  | has xs x  = unique1 xs
  | otherwise = x : unique1 xs

  where
    has :: (Eq a) => [a] -> a -> Bool
    has [] _ = False
    has (x:xs) a
      | x == a    = True
      | otherwise = has xs a

--б)
unique2 :: (Eq a) => [a] -> [a]
unique2 = nub

-- Завдання 2. Визначити, чи два числа взаємно простi.

--a)
isP2 :: Int -> Int -> Bool
isP2 _ n
    | n <= 0 = False 
isP2 n _ 
    | n <= 0 = False
isP2 m k 
    | my m k == 1 = True 
    | otherwise = False

    where
      my :: Int -> Int -> Int  
      my m k 
         | m == k = m
         | m < k = my m k-m  
         | otherwise = my (m-k) k

--б) 
isP1 :: Int -> Int -> Bool 
isP1 _ n
    | n <= 0 = False 
isP1 n _ 
    | n <= 0 = False 
isP1 m k 
    | gcd m k == 1 = True 
    | otherwise = False

-- Висновок: В ході виконання лабораторної роботи, я набув досвiду визначення рекурсивних функцiй, 
-- використання механiзму зiставлення зi зразком i роботи з кортежами та списками. 