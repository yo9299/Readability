import Data.List 

candidatesC6 = generateCandidates 6
candidatesC4 = generateCandidates 4
forbidden :: [[Char]]
forbidden = ["111", "222", "221", "122", "33", "232", "322", "223", "3212", "2123", "211211", "112112", "121121"]
forbiddenC4 = ["33", "1112", "1113", "1131", "1311", "3111", "2111", "1211", "1121", "2223", "3222", "2322", "2232", "1222", "2122", "2212" ,"2221"]
forbiddenC6 = forbidden ++ (concat $ fmap rotate ["211211", "112112", "121121"])

generateCandidates:: Int -> [[Char]]
generateCandidates 0 = [[]]
generateCandidates n = [ i:can | can <- generateCandidates (n-1), i <- ['1', '2', '3'] ]

computeValidSequences :: [[Char]] -> [[Char]] -> [[Char]]
-- given a list of possible sequences and a list of forbidden subsequences, computes all the possible sequences 
-- that avoid all the forbidden subsequences, up to symmetry (i.e., equivalence classes)
computeValidSequences xs for = removeEquivalentLists $ validSeq xs for

rotateLastToFirst :: [a] -> [[a]]
rotateLastToFirst [] = []
rotateLastToFirst xs = rotate xs (length xs - 1)
  where
    rotate :: [a] -> Int -> [[a]]
    rotate ys (-1) = []
    rotate ys n = ys' : rotate ys' (n - 1)
      where
        ys' = last ys : init ys

rotateFirstToLast :: [a] -> [[a]]
rotateFirstToLast [] = []
rotateFirstToLast xs = rotate xs (length xs - 1)
  where
    rotate :: [a] -> Int -> [[a]]
    rotate ys (-1) = []
    rotate ys n = ys' : rotate ys' (n - 1)
      where
        ys' =(drop 1 ys) ++ [head ys]

rotate :: [a] -> [[a]]
rotate [] = []
rotate xs = (rotateLastToFirst xs)++ (rotateFirstToLast xs)

equivalentLists :: Eq a => [a] -> [a] -> Bool
equivalentLists xs ys = or [any (== ys) (rotate xs), removeInverse xs ys ]

removeInverse :: Eq a => [a] -> [a] -> Bool
removeInverse xs ys = any (== ys) ((fmap inverseList (rotate xs)) ++ [(inverseList xs)])

removeEquivalentLists :: Eq a => [[a]] -> [[a]]
removeEquivalentLists lists = nubBy equivalentLists lists

inverseList :: [a] -> [a] 
inverseList [] = []
inverseList xs = last xs : (inverseList (take (length xs - 1) xs))

isCharValid :: [Char] -> [[Char]] -> Bool
isCharValid xs for = not notvalid
    where eclass = rotate xs
          notvalid = or [isInfixOf f l | f <- for, l<- eclass]

validSeq :: [[Char]] -> [[Char]] ->[[Char]]
validSeq xs for = filter (\x -> isCharValid x for) xs 

