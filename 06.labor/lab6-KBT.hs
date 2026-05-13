import Data.List (nub, sort, elemIndices)
import GHC.Base (VecElem(Int16ElemRep))
import System.Posix (SysVar(ChildLimit))

--prime check

isPrime :: Int -> Bool
isPrime n = length [x | x <- [1..n], n `mod` x == 0]==2;

valodiOszto :: Int -> [Int]
valodiOszto n = [x | x <- [1..(n-1)], n `mod` x == 0]

fel1 :: IO()
fel1 = do
    putStrLn "Elso szam: "
    a <- readLn :: IO Int
    putStrLn "Masodik szam: "
    b <- readLn :: IO Int

    let elso = min a b
    let utolso = max a b
    let szamok = [elso .. utolso]

    putStrLn "Eredmeny: "
    putStrLn("Osszeg: "++ show(sum szamok))

    let primSzamok = [x | x <- szamok, isPrime x]
    putStrLn("Primek osszege: " ++ show (sum primSzamok))

    let maxOsztok = maximum [length (valodiOszto x) | x <- szamok]

    let legtobbOsztok = [x | x <- szamok, length(valodiOszto x)==maxOsztok]
    putStrLn("Legtobb valodi osztoval valo szamok: "++ show legtobbOsztok)


fib :: Int -> Int -> Int -> [Int]
fib a b limit
    | a > limit = []
    | otherwise = a : fib b (a+b) limit