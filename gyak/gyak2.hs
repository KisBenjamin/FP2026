import Data.List(sort, group, sortBy)
import Data.Char(isDigit)
import Data.Ord (comparing)

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

--------3.feladat

nincsBenneSzamjegy :: String -> Bool
nincsBenneSzamjegy szo = not (any isDigit szo)

szuro3 :: [String] -> [String]
szuro3 szavak = filter(\szo -> nincsBenneSzamjegy szo) szavak

feladat3 :: [String] -> IO()
feladat3 szavak = do
    let szurtlista = szuro3 szavak

    if null szurtlista
        then putStrLn ("Nincsenek olyan karakterlancok, amelyek nem tartalmaznak szamot.")
        else do
            putStrLn ("A karakterlancok, amelyek nem tartalmaznak szamokat: ")
            putStrLn (unlines (sort szurtlista))

------4.feladat

feladat4 :: String -> [String] -> IO ()
feladat4 s isS = do
    let sortedSzavak = sort isS
    let vagottLista = break (==s) sortedSzavak
    let bal = last(fst vagottLista)
    let jobb = head(tail (snd vagottLista))

    putStrLn( s ++ " bal szomszedja: " ++ bal ++ " es jobb szomszedja pedig: " ++  jobb)


----5. feladat

findtheMax :: [(String,Int,Int)] -> Int
findtheMax lista =
    let eladottLista = map (\(marka,eladas,ar) -> eladas) lista
    in maximum eladottLista

feladat5 :: [(String,Int,Int)] -> IO ()
feladat5 lista = do
    let maxSzam = findtheMax lista

    let maxLista = filter (\(marka,eladas,ar) -> eladas == maxSzam) lista
    let finalLista =  map (\(marka,eladas,ar) -> marka) maxLista

    if null maxLista
        then putStrLn("Nincsen egy max?!")
        else do
            putStrLn("A maximalis eladasi ertek "++ show maxSzam ++ ". A telefonok, amelyeknek ennyi az eladasi erteke a kovetkezok: ")
            putStrLn(unlines (sort finalLista))

----6.feladat

feladat6 :: [Int] -> IO ()
feladat6 lista = do
    let groupList= group (sort lista)

    let elofordulas = map(\alLista -> (length alLista,head alLista)) groupList
    let paratlanLista = filter(\(hossz,szam) -> mod hossz 2 == 1) elofordulas
    let sortedList = sortBy (comparing fst) paratlanLista

    if null sortedList
        then putStrLn("Nem jo az adatok")
        else do
            mapM_ (\(hossz,szam) -> putStrLn("Elofordulas: "++ show hossz ++ " ertek: "++ show szam)) sortedList

