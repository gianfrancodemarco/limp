module Array where
import Grammar

getFilledArray :: Int -> [Int]
getFilledArray 0 = []
getFilledArray n = 0 : getFilledArray (n-1)


replaceElemAt :: [Int] -> Int -> Int -> [Int]
replaceElemAt (x:xs) 0 value = value : xs
replaceElemAt (x:xs) index value = x : replaceElemAt xs (index-1) value
replaceElemAt [] index value = error "Exceed array bound"