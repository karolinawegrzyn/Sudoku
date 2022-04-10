type Punkt = [Int]
type Wiersz = [Punkt]
type Sudoku = [Wiersz]

wyswietlWiersz :: Wiersz -> String
wyswietlWiersz [] = ""
wyswietlWiersz (x:xs) | length x == 1 = show (head x) ++ " " ++ wyswietlWiersz xs
 |otherwise = show 0 ++ " " ++ wyswietlWiersz xs

wyswietlSudoku :: Sudoku -> String
wyswietlSudoku [] = ""
wyswietlSudoku (x:xs) = wyswietlWiersz x ++ "\n" ++ wyswietlSudoku xs

wyswietlSudoku2 :: Sudoku -> IO ()
wyswietlSudoku2 ts | null ts = putStr "Brak rozwiazania"
 |otherwise = putStr (wyswietlSudoku ts)


segmenty _ [] = []
segmenty n xs = take n xs : segmenty n (drop n xs)

listaNaWiersz :: [Int] -> Wiersz
listaNaWiersz [] = []
listaNaWiersz xs = map (\x -> if x == 0 then [1..9] else [x]) xs

listyNaSudoku :: [Int] -> Sudoku
listyNaSudoku [] = []
listyNaSudoku xs = map listaNaWiersz (segmenty 9 xs)


jednoelementoweListyWWierszu :: [[Int]] -> [[Int]]
jednoelementoweListyWWierszu = filter (\x -> length x == 1)

usunLiczbyZPunktu :: [Int] -> Punkt -> [Int]
usunLiczbyZPunktu _ [] = []
usunLiczbyZPunktu ys (x:xs) | x `elem` ys = usunLiczbyZPunktu ys xs
 |otherwise = x:usunLiczbyZPunktu ys xs

usunPowtorzeniaZWiersza :: Wiersz -> Wiersz
usunPowtorzeniaZWiersza xs = map (\x -> if length x > 1 then usunLiczbyZPunktu (concat (jednoelementoweListyWWierszu xs)) x else x) xs

usunPowtorzeniaZWierszy :: Sudoku -> Sudoku
usunPowtorzeniaZWierszy = map usunPowtorzeniaZWiersza


transpozycja ([]:_) = []
transpozycja x = map head x : transpozycja (map tail x)

usunPowtorzeniaZKolumn :: Sudoku -> Sudoku
usunPowtorzeniaZKolumn xs = transpozycja (usunPowtorzeniaZWierszy (transpozycja xs))


podzialWierszyNaTrojki = map (segmenty 3)

kwadrat3x3 :: Sudoku -> Sudoku
kwadrat3x3 xs = segmenty 9 (concat (concat (transpozycja (podzialWierszyNaTrojki xs))))

usunPowtorzeniaZkwadratu3x3 :: Sudoku -> Sudoku
usunPowtorzeniaZkwadratu3x3 xs = kwadrat3x3 (kwadrat3x3 (usunPowtorzeniaZWierszy (kwadrat3x3 xs)))


usunPowtorzeniaZSudoku :: Sudoku -> Sudoku
usunPowtorzeniaZSudoku xs = usunPowtorzeniaZkwadratu3x3 (usunPowtorzeniaZKolumn (usunPowtorzeniaZWierszy xs))



minPunkt :: [Punkt] -> Int -> Punkt -> Punkt
minPunkt [] n p = p
minPunkt (x:xs) n p | length x < n && length x > 1 = minPunkt xs (length x) x
 |otherwise = minPunkt xs n p

znajdzMinPunkt :: Sudoku -> Punkt
znajdzMinPunkt s = minPunkt (concat s) 10 []


czyPunktWWierszu :: Wiersz -> Punkt -> Bool
czyPunktWWierszu [] _ = False
czyPunktWWierszu (x:xs) ys | x == ys = True
 |otherwise = czyPunktWWierszu xs ys

dodajDoKazdegoElementListy x = map (x:)

zamienWiersz :: Punkt -> Wiersz -> [Wiersz] 
zamienWiersz [] _ = []
zamienWiersz(x:xs) ys = ([x]:ys) : zamienWiersz xs ys

zamienWierszNaTablice :: Wiersz -> Punkt -> [Wiersz]
zamienWierszNaTablice [] _ = []
zamienWierszNaTablice (x:xs) ys | x == ys =  zamienWiersz x xs
 | otherwise = dodajDoKazdegoElementListy x (zamienWierszNaTablice xs ys)

zamienSudoku :: [Wiersz] -> Sudoku -> [Sudoku]
zamienSudoku [] _ = []
zamienSudoku (x:xs) ys = (x:ys) : zamienSudoku xs ys

-- [[[1,2,3],[4]],  [1,4] -> [[[[1,2,3],[4]],   [[[1,2,3],[4]],
-- [[1,4],[3]],                [[1],[3]]],        [[4],[3]]],
-- [[1]]]                      [[1]]]             [[1]]]]

zamienSudokuNaTablice :: Sudoku -> Punkt -> [Sudoku]
zamienSudokuNaTablice [] _ = []
zamienSudokuNaTablice (x:xs) ys | czyPunktWWierszu x ys = zamienSudoku (zamienWierszNaTablice x ys) xs
 | otherwise = dodajDoKazdegoElementListy x (zamienSudokuNaTablice xs ys)


czyPowtorzenia :: Punkt -> Wiersz -> Bool
czyPowtorzenia _ [] = False
czyPowtorzenia ys xs = ys `elem` xs  && length ys == 1

czyPowtorzeniaWWierszu :: Wiersz -> Bool
czyPowtorzeniaWWierszu [] = False
czyPowtorzeniaWWierszu (x:xs) | czyPowtorzenia x xs = True
 | otherwise = czyPowtorzeniaWWierszu xs

czyPowtorzeniaWWierszach :: Sudoku -> Bool
czyPowtorzeniaWWierszach [] = False
czyPowtorzeniaWWierszach (x:xs) | czyPowtorzeniaWWierszu x = True
 | otherwise = czyPowtorzeniaWWierszach xs

czyPowtorzeniaWKolumnach :: Sudoku -> Bool
czyPowtorzeniaWKolumnach [] = False
czyPowtorzeniaWKolumnach xs = czyPowtorzeniaWWierszach (transpozycja xs)

czyPowtorzeniaWKwadratach3x3 :: Sudoku -> Bool
czyPowtorzeniaWKwadratach3x3 xs = czyPowtorzeniaWWierszach (kwadrat3x3 xs)

czyPowtorzeniaWSudoku :: Sudoku -> Bool
czyPowtorzeniaWSudoku xs | czyPowtorzeniaWWierszach xs || czyPowtorzeniaWKolumnach xs || czyPowtorzeniaWKwadratach3x3 xs = True
 | otherwise = False


rozmiarWiersza :: Wiersz -> Int
rozmiarWiersza xs = sum (map length xs)

rozmiarSudoku :: Sudoku -> Int
rozmiarSudoku xs = sum (map rozmiarWiersza xs)

czyWSudokuPustyPunkt :: [[Int]] -> Bool
czyWSudokuPustyPunkt [] = False
czyWSudokuPustyPunkt (x:xs) | null x = True
 | otherwise = czyWSudokuPustyPunkt xs


rozwiazSudoku :: Sudoku -> Sudoku
rozwiazSudoku s | czyPowtorzeniaWSudoku s || czyWSudokuPustyPunkt (concat s)  = []
 | rozmiarSudoku s == 81 = s
 | s /= usunPowtorzeniaZSudoku s = rozwiazSudoku (usunPowtorzeniaZSudoku s)
 | rozw /= [] = rozw
 | otherwise = rozwiazSudoku (head xs)
 where 
     (x:xs) = zamienSudokuNaTablice s (znajdzMinPunkt s)
     rozw = rozwiazSudoku x


readInt :: String -> Int
readInt = read

readIntList :: String -> [Int]
readIntList = map read . words

main :: IO ()
main = do
    x <- readFile "test.txt"
    let xs = readIntList x
    wyswietlSudoku2 (rozwiazSudoku (listyNaSudoku xs))