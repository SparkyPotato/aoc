import Data.Char
import Data.List
import Data.Maybe
import System.IO
import Control.Monad

firstN :: String -> Int
firstN s = ord (fromJust $ find (isDigit) s) - 48

word :: String -> Maybe Int
word ('1':xs) = Just 1
word ('2':xs) = Just 2
word ('3':xs) = Just 3
word ('4':xs) = Just 4
word ('5':xs) = Just 5
word ('6':xs) = Just 6
word ('7':xs) = Just 7
word ('8':xs) = Just 8
word ('9':xs) = Just 9
word _ = Nothing

numbers :: String -> [Int]
numbers s = mapMaybe (word . flip drop s) [0..length s - 1]

main :: IO ()
main = do
    h <- openFile "input1" ReadMode
    c <- hGetContents h
    print $ sum $ map (\x -> let n = numbers x in (head n) * 10 + (last n)) (lines c)

