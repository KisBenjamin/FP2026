import Data.List(sort)

szuro :: Int -> [(String,Int)] -> [(String,Int)]
szuro n lista = filter (\(varos,nepesseg) -> nepesseg > n) lista

rendezettVarosok :: [(String,Int)] -> [String]
rendezettVarosok lista = sort (map fst lista)

feladat1 :: Int -> [(String,Int)] -> IO ()
feladat1 n lista = do
    let vegeredmeny = rendezettVarosok (szuro n lista)

    if null vegeredmeny
        then putStrLn ("Nincs " ++ show n ++ " erteknel nagyobb nepesseg bla bla")
        else do
            putStrLn ("A(z) " ++ show n ++ " nepesseg erteknlel nagyobb rendelkzeo varos")
            putStrLn (unlines vegeredmeny)


---- Feladat2

nincsBenneNulla :: Int -> Bool
nincsBenneNulla szam = notElem '0' (show szam)

szuro2 :: [Int] -> [Int]
szuro2 szamok = filter(\szam -> nincsBenneNulla szam) szamok

feladat2 :: [Int] ->  IO ()
feladat2 szamok = do
    let szurtlista = szuro2 szamok
    let strSzurtlista = map show szurtlista

    if null szurtlista
        then putStrLn ("Nincs olyan szamok, amelyek nem tartalmaznak a 0 szamjegyet")
        else do
            putStrLn("A 0 szamjegyet nem tartalamzo szamok a kovetkezok: " ++ unwords strSzurtlista)