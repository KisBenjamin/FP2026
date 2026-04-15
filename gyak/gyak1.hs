import Data.List (sort)

szuro :: Int -> [(String,Int)] -> [(String,Int)]
szuro n lista = filter (\(varos,nepesseg) -> nepesseg > n) lista