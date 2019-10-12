import System.IO
import Data.Char
import Data.List

--convToMaterto matrici di char in matrici di int
convToMat = map (map (read::String -> Int).words).lines

--elimino testa e coda
--tail prendo la seconda parte della lista
--init prendo la prima parte della lista
remHeadTail x = (init.tail) x

--calcolo in numero massimo
indTop line = fst (foldl (\(acc,mass)sky -> if sky > mass then (acc+1,sky) else (acc,mass))(0,0) line) 

--controllo il top
controlTop line =  ((head(reverse line)) == (indTop(reverse(remHeadTail line))))&&((head line) == (indTop ( remHeadTail line)))

--controllo tutte le righe
controlRows matrix = not (elem False (map controlTop matrix))

--controllo se gli elementi di una lista sono unici
unique :: (Eq a) => [a] -> Bool
unique []     = True
unique (x:xs) = x `notElem` xs && unique xs

--trovo range di una lista
range :: (Ord a) => [a] -> (a,a)
range a = (minimum a, maximum a)


main = do
 file <- readFile "game3x3.txt"
 print(file)
 --controllo che i dati siano corretti
 if (controlRows (remHeadTail(convToMat file)) && controlRows(remHeadTail(transpose(convToMat file))))
 then print ("DATI CORRETTI")
 else print ("DATI ERRATI")
 
 --trovo la matrice di gioco effettiva 
 print("righe")
 let mat=map remHeadTail (remHeadTail(convToMat file))
 print(mat)
 --controllo che il range sia esatto
 let d= map range mat
 print(d)
 --controllo che su ogni riga i numeri siano unici
 let f=map unique mat
 print(f)
 
 --ripeto per le colonne
 print("colonne")
 let mat=map remHeadTail (remHeadTail(transpose(convToMat file)))
 print(mat)
 let d= map range mat
 print(d)
 let f=map unique mat
 print(f)