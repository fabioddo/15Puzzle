import System.IO
import Data.Char
import Data.List
import Data.List.Split
import Control.Monad
import System.Random

main = do
    print ("Select width: ")
    ws <- getLine
    w <- return $ read ws
    print ("Select height: ")
    hs <- getLine
    h <- return $ read hs
    matrix <- return (chunksOf w ([1..((w*(h))-1)]++[0]))
    gen <- getStdGen
    print (matrix)
    let matrixR = diffMov w h gen matrix
    print (matrixR)
    forever $ do
        print ("Get move x: ")
        xs <- getLine
        let x = read xs
        print ("Get move y: ")
        ys <- getLine
        let y = read ys
        matrix <- return (moveMat x y matrix)
        print (matrix)
        if (solved w h (concat matrix))
            then print ("Solved")
            else print ("Not Solved")



--get index of zero
indexZero :: (Eq a, Num a, Num t) => [a] -> t -> t
indexZero (x:xs) cont
        | x /= 0 = indexZero xs (cont+1)
        |otherwise = cont

--move :: [a] -> a -> [a]
move mat m
     | m < (indexZero mat 0) = moveR mat m
     | (m < (length mat)) && (m >= (indexZero mat 0)) = moveL mat m
     | otherwise = mat

-- move matrix
moveMat x y mat
        | elem 0 (mat !! x) = map (\xl -> if(elem 0 xl) then (move xl y) else xl) mat
        | otherwise = transpose (map (\xl -> if(elem 0 xl) then (move xl x) else xl) (transpose mat))

--remove zero from list
noZero :: (Num a , Eq a) => [a] -> [a]
noZero l = filter (/=0) l

-- move to the rigth
moveR :: (Num a, Eq a) => [a] -> Int -> [a]
moveR l x = (take (x) (noZero l)) ++ [0] ++ (drop (x) (noZero l))

--move to the left
moveL :: (Num a, Eq a) => [a] -> Int -> [a]
moveL l x = (take (indexZero l 0) l) ++ (moveR (drop (indexZero l 0) l) (x - (indexZero l 0)))

--check if movement is in 0 lines
hasZero :: (Num a, Eq a) => Int -> Int -> [[a]] -> Bool
hasZero x y mat = (elem 0 (mat !! x)) || (elem 0 ((transpose mat) !! y))

--check if problem is solved
solved :: (Num a, Eq a, Enum a) => a -> a -> [a] -> Bool
solved x y mat = [1..((x*y)-1)] == mat

-- random movement
randomMove w h mat gen =
    let (x,genx) = randomR (0,h-1) gen
        (y,geny) = randomR (0,w-1) genx
    in if(hasZero x y mat) then ((moveMat x y mat), geny) else (mat, geny)

-- make n ramdom moves
randomNMove _ _ 0 _ mat = mat
randomNMove w h n gen mat =
    let (m, rgen) = randomMove w h mat gen
    in randomNMove w h (n-1) rgen m
