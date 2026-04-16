import Data.List (sort)
import System.Posix.Internals (lstat)

szuro :: Int -> [(String,Int)] -> [(String,Int)]
szuro n lista = filter (\(varos,nepesseg) -> nepesseg > n) lista

-- 1.feladat
fel1 = do
    let ls = [("sepsiszentgyorgy", 54000), ("kolozsvár", 330000),
                 ("marosvasarhely", 130000), ("temesvar", 310000), ("arad", 160000), ("gyergyoszentmiklos", 18000), ("nagyvarad",196000)]
        n = 150000
        ls1 = filter (\(_, nepesseg) -> nepesseg >= n) ls 
        ls2 = sort (map fst ls1)
    if null ls2 
        then 
            putStrLn("Nincs " ++ show n ++ "nepessegel rendelkezo blabla")
        else do
            putStrLn ("A(z)" ++ show n ++ "nepessegel erteknel rendelkezo blabla:")
            mapM_ (\v -> putStrLn ("- " ++ v)) ls2

-- 2.feladat

nincsNulla n  
    | mod n 10 == 0 = False 
    | n < 10 = True
    | otherwise = nincsNulla (div n 10)

fel2 = do
    let ls = [17603, 4005, 3223, 816252, 70, 23561, 9018007, 807, 61, 300]
        ls1 = filter nincsNulla ls 
    if ls1 == []
        then putStrLn "Nincsenek olyan szamok, amelyek nem tartalmazzak a 0 szamjegyet"
        else do
            putStrLn ("A 0 szamjegyeket nem tartalmazo szamok: "
                        ++ unwords (map show ls1))

fel3 = do
    let ls = ["2023tuple", "function", "fl oat", "higher-order", "variable10", "may13be", "0recursion", "monad", "class"]
        ls2 = filter (not . any isDigit2) ls    
            where 
                isDigit2 c = c 'elem' "0123456789"
        ls3 = filter (not . any isDigit) ls    
    if null ls2 
        then putStrLn "NINCSENEK"
        else do
            putStrLn "A karakterlancok: "
            putStrLn $ intercalate "\n" (sort ls2)
            mapM_ putStrLn (sort ls2)