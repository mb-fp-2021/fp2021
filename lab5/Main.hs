-- Лабораторна робота №5
-- студента групи КН-32
-- підгрупи 1
-- Бури Максима
-- Варіант №7

-- Мета:
-- Ознайомитись з модульною органiзацiєю програм та засобами введення-
-- виведення. Набути досвiду компiляцiї Haskell-програм.

-- Реалiзувати та скомпiлювати одну з програм, розроблених у 
-- лабораторнiй роботi No 3 для Вашого варiанта з введенням даних: 
-- а) з клавiатури, б) з файлу 
-- та виведенням результатiв: 
-- в) на екран, г) у файл.
module Main where
    import Prelude
    import System.IO
    import Lb3
    
    main = do
        putStrLn "Знайти найменше спiльне кратне двох чисел"
        
        input1 <- getLine
        input2 <- getLine
        let x = (read input1 :: Int)
        let y = (read input2 :: Int)
        
        putStrLn (show(lcm1 x y))

       
        input <- openFile "data.txt" ReadMode
        contents <- hGetContents input
        let numbers = words contents
        let x1 = (read (head numbers) :: Int)
        let x2 = (read (last numbers) :: Int)
        let result = lcm1 x1 x2
        
        withFile "result.txt" WriteMode (\handle -> do
            hPutStrLn handle $ show result
            hClose handle
            )