import Prelude hiding (replicate)

-- allEqual - функция, която ни казва дали всички елементи от списък са еднакви
allEqual :: (Eq a) => [a] -> Bool
allEqual [] = True
allEqual [x] = True
allEqual (x:y:xs) = (x == y) && allEqual (y:xs)
-- allEqual (x:rest@(y:xs)) = x == y && allEqual rest

allEqual' :: (Eq a) => [a] -> Bool
allEqual' [] = True
allEqual' (x:xs) = and $ map (== x) xs -- [True, True, False]
                -- foldl (&&) True $ map (== x) xs

allEqual'' :: (Eq a, Enum a) => [a] -> Bool
allEqual'' [] = True
allEqual'' all@(x:xs) = take (length all) [x,x..] == all
                        -- replicate (length all)

-- split - функция, която приема String и Char и разделя низа според разделителния знак
-- например: split "abc,def,ghi" ',' == ["abc", "def", "ghi"]
--           split "abc,def,ghi" ';' == ["abc,def,ghi"]
--           split "abc;def,ghi" ';' == ["abc", "def,ghi"]
splitHelper :: (Eq a) => [a] -> a -> [a]
splitHelper [] _ = []
splitHelper a b
    | head a == b = []
    | otherwise = head a:splitHelper (tail a) b

split :: (Eq a) => [a] -> a -> [[a]]
split [] _ = []
split a b
    | head a == b = split (tail a) b
    | otherwise = h:split (drop (length h + 1) a) b 
    where h = splitHelper a b

split' :: String -> Char -> [String]
split' [] _ = []
split' str c = p : split (drop q str) c
    where p = (takeWhile (/= c) str)
          q = (length p) + 1

split'' :: (Eq a) => [a] -> a -> [[a]]
split'' [] _ = []
split'' xs sep = untilFirstSeperator : split rest sep
    where untilFirstSeperator = takeWhile (/= sep) xs
          rest = case dropWhile (/= sep) xs of
                    [] -> []
                    (_:xs) -> xs

-- join - функция, която приема списък от Strings и Char и прави един низ, като между всеки елемент
-- от списъка добавя разделителния знак
-- например: join ["abc", "def", "ghi"] ',' == "abc,def,ghi"
--           join ["abc", "def", "ghi"] '' == "abcdefghi"
join :: [[a]] -> a -> [a]
join [] _ = []
join (h:t) b
   | null t = h
   | otherwise = (h ++ [b]) ++ join t b

-- splitByN - функция, която разделя списък на равни части с дадена големина
-- например: splitByN [1..6] 2 == [[1,2],[3,4],[5,6]]
--           splitByN [1..6] 4 == [[1,2,3,4],[5,6]]
--           splitByN [1..6] 7 == [[1,2,3,4,5,6]]
--           splitByN [1..6] 1 == [[1],[2],[3],[4],[5],[6]
splitByN :: [a] -> Int -> [[a]]
splitByN [] _ = []
splitByN xs 0 = [xs]
splitByN xs n = take n xs : splitByN (drop n xs) n

-- replicate - функция, която приема списък и число и ни връща списък, но всеки елемент от
-- оригиналния е повторен колкото даденото число
-- например: replicate [1..5] 2 == [1,1,2,2,3,3,4,4,5,5]
--           replicate [1..3] 4 == [1,1,1,1,2,2,2,2,3,3,3,3]
listOfNX :: a -> Int -> [a]
listOfNX _ 0 = []
listOfNX x n = x : listOfNX x (n - 1)

replicate :: [a] -> Int -> [a]
replicate [] _ = []
replicate (x:xs) n = listOfNX x n ++ replicate xs n

-- transpose - функция, която транспонира дадена матрица
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose matrix
    | all null matrix = []
    | otherwise = map head matrix : transpose (map tail matrix)

-- permutations - функция, която ни дава всички пермутации на даден списък
permutations :: (Eq a) => [a] -> [[a]]
permutations [] = [[]]
permutations xs = [x : rest | x <- xs, rest <- permutations $ xs `without` x] 
    where xs `without` x = filter (/= x) xs