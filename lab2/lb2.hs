-- Лабораторна робота №2
-- студента групи КН-32
-- підгрупи 1
-- Бури Максима
-- Варіант №6

-- Мета: Набути досвiду визначення рекурсивних функцiй, використання механiзму
-- зiставлення зi зразком i роботи з кортежами та списками.

-- Завдання 1. Визначте вказанi функцiї в кожному з завдань: 
-- а) без застосування, 
-- б) з застосуванням вбудованих функцiй.
-- Перетворити багаторiвневий список на однорiвневий.

-- а)
data NestedList a = Elem a | List [NestedList a]

flatten1 :: NestedList a -> [a]
flatten1 (Elem a   )   = [a]
flatten1 (List (x:xs)) = flatten1 x ++ flatten1 (List xs)
flatten1 (List [])     = []

-- б)

flatten2 :: NestedList a -> [a]
flatten2 (Elem x) = [x]
flatten2 (List x) = concatMap flatten2 x

-- Завдання 2. Визначте вказанi функцiї в кожному з завдань: 
-- а) без застосування, 
-- б) з застосуванням вбудованих функцiй.
-- Визначити, чи є число простим.

-- а)
isPrime1 :: Int -> Int -> Int -> Bool 
isPrime1 n x y | x * y == n = False 
               | n == 1 = False
               | y < 100 = isPrime1 n x (y+1)
               | x < 100 = isPrime1 n (x+1) 2
               | otherwise = True

-- б)
isPrime2 :: Int -> Bool
isPrime2 1 = False
isPrime2 2 = True
isPrime2 n  | (length [x | x <- [2 .. n-1], mod n x == 0]) > 0 = False
            | otherwise = True

-- Висновок: В ході виконання лабораторної роботи, я набув досвiду визначення рекурсивних функцiй, 
-- використання механiзму зiставлення зi зразком i роботи з кортежами та списками. 