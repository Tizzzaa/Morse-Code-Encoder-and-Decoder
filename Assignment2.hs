-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE NoGeneralizedNewtypeDeriving, Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}


module Assignment2 (encodeWord , encodeWords , encodeText ,
                    decodeText ,
                    decodeTextWithTree ,
                    ramify ,
                    tabulate ,
                    tree) where

import Types
import Data.List

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------


{- Question 1 -}
encodeWord :: Table -> String -> Code
encodeWord codeTable "" = []
encodeWord codeTable (c:[]) = case lookup c codeTable of
    Nothing -> []
    Just x -> x
encodeWord codeTable (x:xs) = case lookup x codeTable of
    Nothing -> []
    Just y ->  y ++ shortGap++ encodeWord codeTable xs

-- test :: Table -> String -> Code
-- test codeTable a = lookup (head a) codeTable

encodeWords :: Table -> [String] -> Code
encodeWords codeTable [] = []
encodeWords codeTable (x:[]) = encodeWord codeTable x
encodeWords codeTable (x:xs) = encodeWord codeTable x ++ mediumGap ++ encodeWords codeTable xs

encodeText :: Table -> String -> Code
encodeText codeTable text = encodeWords codeTable (split 0 " " text "")

split :: Eq a => Int -> [a] -> [a] -> [a] -> [[a]]
split _ _ [] currentWord = [currentWord]
split num indicator x currentWord | take (length indicator) x==indicator = if num == 1 then (currentWord++[head x]) : split num indicator (drop (length indicator+1) x)  []
                                                                       else currentWord : split num indicator (drop (length indicator) x)  []
                              | otherwise  = split num indicator (tail x) (currentWord ++ [head x])


{- Question 2 -}
decodeText :: Table -> Code -> String
decodeText codeTable inputCode = arrayToText (arrayOfStrings codeTable inputCode)

arrayToText :: [String] -> String
arrayToText [] = ""
arrayToText (x:[]) = x
arrayToText (x:xs) = x ++ " " ++ arrayToText xs

arrayOfStrings :: Table -> Code -> [String]
arrayOfStrings codeTable x = listToString codeTable (splittingWords x)

listToString :: Table -> [Code] -> [String]
listToString _ [] = []
listToString codeTable (x:xs) = arrayToString codeTable x : listToString codeTable xs

arrayToString :: Table -> Code -> String
arrayToString codeTable y = codeToString codeTable (splittingLetters y)

splittingWords :: Code -> [Code]
splittingWords [] = [[]]
splittingWords x =  split 1 mediumGap x []

splittingLetters :: Code -> [Code]
splittingLetters [] = [[]]
splittingLetters x = split 1 shortGap x []

codeToString :: Table -> [Code] -> String
codeToString _ []= ""
codeToString codeTable (x:xs)= convertFromMaybe (codeToChar codeTable x) : codeToString codeTable xs

codeToChar :: Table -> Code -> Maybe Char
codeToChar codeTable x = case lookup x (reverseTable codeTable) of
    Nothing -> Nothing
    Just y -> Just y

reverseTable :: [(Char,Code)] -> [(Code,Char)]
reverseTable [] = []
reverseTable ((x,y):z) = (y,x) : reverseTable z

{- Question 3 -}

decodeTextWithTree :: Tree -> Code-> String
decodeTextWithTree trees x = arrayToText (convertToLetterString trees (listOfAtomsLetter (listOfListOfCode (splittingWords x))))

listOfListOfCode :: [Code] ->[[Code]]
listOfListOfCode [] =[]
listOfListOfCode (x:xs) = splittingLetters x:listOfListOfCode xs

listOfAtomsLetter :: [[Code]] -> [[[Code]]]
listOfAtomsLetter [] = []
listOfAtomsLetter (x:xs) = listOfAtoms x : listOfAtomsLetter xs


listOfAtoms :: [Code]->[[Code]]
listOfAtoms (x:xs) = convertCodeToDitDah [] x : listOfAtoms xs
listOfAtoms [] = []



convertCodeToDitDah :: Code -> Code -> [Code]
convertCodeToDitDah working []     | working==dit = [working]
                                   | working==dah = [working]
                                   | otherwise = [[]]
convertCodeToDitDah working (x:xs) | working==dit = working : convertCodeToDitDah [] (x:xs)
                                   | working==dah = working :convertCodeToDitDah [] (x:xs)
                                   | otherwise = convertCodeToDitDah (working++[x]) xs

convertToLetterString :: Tree -> [[[Code]]] ->[String]
convertToLetterString _ [] = []
convertToLetterString trees (x:xs) = eachWord trees x : convertToLetterString trees xs

eachWord :: Tree -> [[Code]] -> String
eachWord _ [] = []
eachWord trees (x:xs) =  convertFromMaybe (searchForCharacter trees (codeToAddress x)) : eachWord trees xs

convertFromMaybe :: Maybe a -> a
convertFromMaybe (Just x) = x

data Direction = Dit | Dah deriving (Show)
type Address = [Direction]

codeToAddress :: [Code] -> Address
codeToAddress [] = []
codeToAddress (x:xs) | x == dit = Dit : codeToAddress xs
                     | x == dah = Dah : codeToAddress xs

searchForCharacter :: Tree -> Address -> Maybe Char
searchForCharacter (Branch char dit' dah') [] = char
searchForCharacter (Branch _ dit' _) (Dit:xs) = searchForCharacter dit' xs
searchForCharacter (Branch _ _ dah') (Dah:xs) = searchForCharacter dah' xs
searchForCharacter _ _ =  Nothing



{- Question 4 -}
ramify :: Table -> Tree
ramify x = populateTree [] (Branch Nothing Empty Empty) x

isCurrentValid :: Code -> Table -> Bool
isCurrentValid code ((x,y):z)  | y==code = True
                               | y==code++dit = True
                               | y==code++dah = True
                               | otherwise = isCurrentValid code z
isCurrentValid _ _ = False

codeToStringMaybe :: Table -> [Code] -> [Maybe Char]
codeToStringMaybe _ []= []
codeToStringMaybe codeTable (x:xs)= codeToChar codeTable x: codeToStringMaybe codeTable xs

addressToCode :: Address -> Code
addressToCode [] = []
addressToCode (Dit:xs) = dit++addressToCode xs
addressToCode (Dah:xs) = dah++addressToCode xs

populateTree :: Address -> Tree -> Table -> Tree

populateTree x y inputTable |isCurrentValid (addressToCode (x++[Dit])) inputTable&&isCurrentValid (addressToCode (x++[Dah])) inputTable = Branch (codeToChar inputTable (addressToCode x)) (populateTree (x++[Dit]) (Branch Nothing Empty Empty) inputTable) (populateTree (x++[Dah]) (Branch Nothing Empty Empty) inputTable)
                            |isCurrentValid (addressToCode (x++[Dit])) inputTable&&not (isCurrentValid (addressToCode (x++[Dah])) inputTable)= Branch (codeToChar inputTable (addressToCode x)) (populateTree (x++[Dit]) (Branch Nothing Empty Empty) inputTable) Empty
                            |not (isCurrentValid (addressToCode (x++[Dit])) inputTable)&&isCurrentValid (addressToCode (x++[Dah])) inputTable= Branch (codeToChar inputTable (addressToCode x)) Empty (populateTree (x++[Dah]) (Branch Nothing Empty Empty) inputTable)
                            | otherwise = Branch (codeToChar inputTable (addressToCode x)) Empty Empty

{- Question 5 -}
tabulate :: Tree -> Table
tabulate trees = removeEmptyNodes (zip (traversalNode trees) (traversalCode trees []) )

removeEmptyNodes :: [(Maybe Char, Code)] -> [(Char,Code)]
removeEmptyNodes [] = []
removeEmptyNodes ((x,y):xs) | x==Nothing = removeEmptyNodes xs
                            | otherwise = (convertFromMaybe x,y) : removeEmptyNodes xs

zipTable :: [Maybe Char] -> [Code] -> [(Maybe Char,Code)]
zipTable a b = zip a b

traversalCode :: Tree -> Address -> [Code]
traversalCode Empty _ = []
traversalCode (Branch x y z) a = [addressToCode a] ++ traversalCode y (a++[Dit]) ++ traversalCode z (a++[Dah])

traversalNode :: Tree -> [Maybe Char]
traversalNode Empty = []
traversalNode (Branch x y z) =[x] ++ traversalNode y ++ traversalNode z

{- Question 6 -}
brackets :: Bracket -> String
brackets (Round ts) = "(" ++ concat [brackets t | t <- ts] ++ ")"
brackets (Curly ts) = "{" ++ concat [brackets t | t <- ts] ++ "}"

tree :: String -> Maybe Bracket
tree = undefined

{-
tree :: String -> Maybe Bracket
tree (x:xs) y | x == '(' = Just (Round [convertFromMaybe(treeBuilding xs 1)])
              | x == '(' = Just (Curly [convertFromMaybe(treeBuilding xs 1)])
              | x == ')' = Nothing
              | x == '}' = Nothing
-}
{-
treeBuilding :: String -> Int -> Maybe Bracket
treeBuilding (x:xs) y | x == '(' = Just (Round [convertFromMaybe(treeBuilding xs 1]))
                      | x == '{' = Just (Curly [convertFromMaybe(treeBuilding xs 2]))
                      | x == ')' && y == 2 = Nothing
                      | x == '}' && y == 1 = Nothing
                      | otherwise = 
-}

isWellBracketed :: String -> Bool
isWellBracketed xs = case tree xs of
                      Nothing -> False
                      Just _  -> True
