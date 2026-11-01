{-
  LABORATORIJAS DARBS NR. 1
  Variants: 4
-}

module Main where

import Data.Char (toUpper)
import Data.List (sortBy)

-- ============================================================================
-- UZDEVUMS NR. 1 (Variants 1-5: 1. risinajums)
-- Kartot sarakstu dilstosa seciba un izvadit pirmo skaitli ar isaprakstu.
-- ============================================================================
maxNumberSort :: (Ord a, Show a) => [a] -> String
maxNumberSort [] = "Saraksts ir tukss!"
maxNumberSort xs = case sortBy (\a b -> compare b a) xs of
  (top:_) -> "Maksimalais skaitlis ir " ++ show top
  []      -> "Saraksts ir tukss!"


-- ============================================================================
-- UZDEVUMS NR. 2 (Variants 1-5: 1. risinajums)
-- Apgriezt varda simbolus
-- ============================================================================
reverseManual :: [Char] -> [Char]
reverseManual str = rev str []
  where
    rev [] acc     = acc
    rev (x:xs) acc = rev xs (x:acc)


-- ============================================================================
-- UZDEVUMS NR. 3 (Variants 2,3,4,5,7,8,9,10: 2. risinajums)
-- Katra teikuma varda pirmais burts liels (izmantojot reducesanas funkciju / fold)
-- ============================================================================
capitalizeWord :: String -> String
capitalizeWord []     = []
capitalizeWord (x:xs) = toUpper x : xs

titleCaseFold :: String -> String
titleCaseFold str = foldr joinWords "" (words str)
  where
    joinWords w acc
      | null acc  = capitalizeWord w
      | otherwise = capitalizeWord w ++ " " ++ acc


-- ============================================================================
-- UZDEVUMS NR. 4 (Variants 4-5)
-- Faktoriala funkcija. Jautajums: kas notiek, ja izsauc fac (-3)?
-- ============================================================================
fac :: (Ord a, Num a) => a -> a
fac n
  | n < 0     = error "Faktorialis nav definets negativiem skaitliem!"
  | n == 0    = 1
  | otherwise = n * fac (n - 1)


-- ============================================================================
-- UZDEVUMS NR. 5 (Variants 4)
-- No saraksta nonemt secigus atkartotus elementus.
-- ============================================================================
remSuccessiveDuplicates :: Eq a => [a] -> [a]
remSuccessiveDuplicates []  = []
remSuccessiveDuplicates [x] = [x]
remSuccessiveDuplicates (x:y:xs)
  | x == y    = remSuccessiveDuplicates (y:xs)
  | otherwise = x : remSuccessiveDuplicates (y:xs)


-- ============================================================================
-- UZDEVUMS NR. 6 (Variants 4-5)
-- Funkcija split, kas sadala sarakstu apaksarakstos ar noradito garumu.
-- ============================================================================
split :: Int -> [a] -> [[a]]
split _ [] = []
split n xs
  | n <= 0    = error "Garumam jabut lielakam par 0!"
  | otherwise = take n xs : split n (drop n xs)


-- ============================================================================
-- UZDEVUMS NR. 7 
-- Binarie meklesanas koki (BST)
-- ============================================================================
data Tree a = Branch a (Tree a) (Tree a) | Leaf
  deriving (Show, Eq)

isSearchTree :: Ord a => Tree a -> Bool
isSearchTree t = check Nothing Nothing t
  where
    check _ _ Leaf = True
    check low high (Branch x left right) =
      maybe True (<= x) low &&
      maybe True (x <=) high &&
      check low (Just x) left &&
      check (Just x) high right

insertTree :: Ord a => a -> Tree a -> Tree a
insertTree e Leaf = Branch e Leaf Leaf
insertTree e (Branch x left right)
  | e <= x    = Branch x (insertTree e left) right
  | otherwise = Branch x left (insertTree e right)

badInsertTree :: Ord a => a -> Tree a -> Tree a
badInsertTree e Leaf = Branch e Leaf Leaf
badInsertTree e (Branch x left right)
  | e <= x    = Branch x left (badInsertTree e right)
  | otherwise = Branch x (badInsertTree e left) right


-- ============================================================================
-- PROGRAMMAS IZPILDE (GHCi testesanai)
-- ============================================================================
main :: IO ()
main = do
  putStrLn "============================================================"
  putStrLn "HASKELL LABORATORIJAS DARBS NR. 1 (4. VARIANTS) - REZULTATI"
  putStrLn "============================================================"

  putStrLn "\n--- UZDEVUMS NR. 1 ---"
  let sampleList = [5, 10, 11, 15, 3, 8]
  putStrLn $ "Saraksts: " ++ show sampleList
  putStrLn $ "Rezultats: " ++ maxNumberSort sampleList

  putStrLn "\n--- UZDEVUMS NR. 2 ---"
  let sampleWord = "Haskell"
  putStrLn $ "Vards: " ++ sampleWord
  putStrLn $ "Apgriezts (Manual): " ++ reverseManual sampleWord

  putStrLn "\n--- UZDEVUMS NR. 3 ---"
  let sampleSentence = "programmesanas valoda haskell ir funkcionala"
  putStrLn $ "Ievade: " ++ sampleSentence
  putStrLn $ "Rezultats (Foldr): " ++ titleCaseFold sampleSentence

  putStrLn "\n--- UZDEVUMS NR. 4 ---"
  putStrLn $ "fac 5 = " ++ show (fac 5)
  putStrLn "Piezime: fac (-3) izsauks kludu, kas apstradata koda (Skat. atskaiti)."

  putStrLn "\n--- UZDEVUMS NR. 5 ---"
  let dupList = [1, 2, 2, 3, 2, 4 :: Int]
  putStrLn $ "Ievade: " ++ show dupList
  putStrLn $ "remSuccessiveDuplicates: " ++ show (remSuccessiveDuplicates dupList)

  putStrLn "\n--- UZDEVUMS NR. 6 ---"
  let splitList = [1, 2, 3, 4, 5, 6, 7 :: Int]
  putStrLn $ "Ievade: split 2 " ++ show splitList
  putStrLn $ "Rezultats: " ++ show (split 2 splitList)

  putStrLn "\n--- UZDEVUMS NR. 7 (BST Eksperiments) ---"
  let bst = insertTree (4 :: Int) (insertTree 2 (insertTree 6 Leaf))
  putStrLn $ "Koks ar korektu insert: isSearchTree = " ++ show (isSearchTree bst)
  
  let brokenBst = badInsertTree (4 :: Int) (insertTree 2 (insertTree 6 Leaf))
  putStrLn $ "Koks ar kludainu insert: isSearchTree = " ++ show (isSearchTree brokenBst)
  putStrLn "Secinajums: isSearchTree veiksmigi atklaj kludainu koka strukturu."

  putStrLn "\n============================================================"