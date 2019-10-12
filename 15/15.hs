import System.IO
import Data.Char
import Data.List
import System.Random


--trasformo la matrice in lista
--convToList = map (read::String -> Int).words

convToMat = map (map (read::String -> Int).words).lines


sposta0 :: (Ord a, Num a)=> Int ->[a] -> [a]
sposta0 x lista
 | x < 0                = lista
 | x > length lista     = lista
 | elem 0 lista == True = take x (filter (/=0) lista) ++ [0] ++ drop x (filter (/=0) lista) 
 | otherwise            = lista

--indice0 :: (Eq a, Num a)=> [a]-> a ->a
--indice0 (x:xs) count
-- | x/=0      = indice0 xs count+1
-- | otherwise = count

movemat :: (Ord a ,Num a )=> Int -> Int ->[[a]]->[[a]]
movemat x y mat
 | elem 0 (mat!!x)                  = map (sposta0 y) mat
 | elem 0 ((transpose mat)!!y)      = transpose (map (sposta0 x) (transpose mat))
 | otherwise                        = mat


---controllo se il gioco è risolto
controlWin :: (Ord a,Num a) => [[a]]->[[a]]-> String
controlWin newmat originmat
 | compare newmat originmat == EQ = "HAI VINTO"
 | otherwise                      = "PROVA CON UN'ALTRA MOSSA"

--genero n mosse random 
--listrandom :: (Num a)=> a -> [[a]]
--listrandom n =  

main = do
 file <- readFile "15.txt"
 print(file)
 
 let mat =convToMat file



 print("matrice")
 mapM_ print mat
  
 loop mat mat
 ---genero la lista di mosse che voglio fare
 
 
loop mattt mat= do

 
 print("inserire x")
 x <- getLine
 let cordx=(read x :: Int)
 print("inserire y")
 y <- getLine
 let cordy=(read y :: Int)
 print(cordx,cordy)
 
 print("")
 let newMat = movemat cordx cordy mattt
 mapM_ print newMat

 let result = controlWin newMat mat
 print result
 loop newMat mat
