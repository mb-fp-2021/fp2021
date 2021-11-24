-- Лабораторна робота №4
-- студента групи КН-32
-- підгрупи 1
-- Бури Максима
-- Варіант №6

-- Мета: Ознайомитись з системою типiв та класiв типiв. Набути досвiду визначення
-- нових типiв та класiв типiв i їх використання.

-- Публiкацiї. Зберiгаються данi про публiкацiї, якi можуть бути книгою (ав-
-- тор/спiвавтори, назва, мiсто, видавництво, рiк), статтею (автор/спiвавтори, на-
-- зва статтi, назва журналу, рiк, номер журналу, сторiнки) або тезами доповiдi
-- (автор/спiвавтори, назва доповiдi, назва конференцiї, мiсто, рiк, сторiнки). Ви-
-- значне функцiї для : визначення чи є публiкацiя з певною назвою книгою, статтею або тезами;

    type Authors = [String]
    type Title = String
    type City = String
    type Publisher = String
    type Year = Int
    type Journal_Title = String
    type Number = Int
    type Pages = Int
    type Conference_Title = String

    data Publication =
        Book Authors Title City Publisher Year                  |
        Article Authors Title Journal_Title Year Number Pages   |
        Thesis Authors Title Conference_Title City Year Pages

    class MyClass t where
        getTitle :: t -> [String]   
    
    instance MyClass Publication where
        getTitle (Book _ x _ _ _) = [x]
        getTitle (Article _ x _ _ _ _) = [x]
        getTitle (Thesis _ x _ _ _ _) = [x]

    instance Show Publication where
        show (Book _ _ _ _ _) = "Book"
        show (Article _ _ _ _ _ _) = "Article"
        show (Thesis _ _ _ _ _ _) = "Thesis"

    findType :: MyClass t => String -> [t] -> [t]
    findType title publications = (filter (elem title . getTitle) publications)

-- Висновок: В ході виконання лабораторної роботи, я ознайомився з системою типiв та класiв типiв. 
-- Набув досвiду визначення нових типiв та класiв типiв i їх використання.