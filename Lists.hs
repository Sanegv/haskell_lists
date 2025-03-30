import Data.Foldable (minimumBy)
sum' :: Num a => [a] -> a
sum' = foldr' (+) 0

product' :: Num a => [a] -> a
product' = foldr' (*) 1

map' :: (a -> b) -> [a] -> [b]
map' f = foldr' (\x acc -> f x : acc) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr' (\x acc -> if p x then x:acc else acc) []

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

init' :: [a] -> [a]
init' [] = error "empty list"
init' [x] = []
init' (h:t) = h: init' t

head' :: [a] -> a
head' [] = error "empty list"
head' (x:_) = x

tail' :: [a] -> [a]
tail' [] = error "empty list"
tail' (_:l) = l

last' :: [a] -> a
last' [] = error "empty list"
last' [x] = x
last' (h:t) = last' t

intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' x l = tail' $ foldr' (\e acc -> x : e : acc) [] l

concat' :: [[a]] -> [a]
concat' = foldr' (++) []

intercalate' :: [a] -> [[a]] -> [a]
intercalate' _ [] = []
intercalate' x l = concat' $ tail' $ foldr' (\e acc -> x : e : acc) [] l

iterate' :: (a -> a) -> a -> [a]
iterate' f x = let result = f x in result : iterate' f result

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' _ [] = ([],[])
splitAt' i (h:t)
    | i <= 0    = ([], (h:t))
    |otherwise  = let tuple = splitAt' (pred i) t in (h:fst tuple, snd tuple)

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' i (h:t) = h:take' (pred i) t

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (h:t) = if p h then h:takeWhile' p t else []

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 l = l
drop' i (h:t) = drop' (pred i) t

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (h:t) = if p h then dropWhile' p t else h:t

span' :: (a -> Bool) -> [a] -> ([a], [a])
span' _ [] = ([],[])
span' p l = (takeWhile' p l, dropWhile' p l)

break' :: (a -> Bool) -> [a] -> ([a], [a])
break' p = span' (not . p)

sortBy' :: (a -> a -> Ordering) -> [a] -> [a]
sortBy' _ [] = []
sortBy' f (h:t) =
    let
        smallerSorted = sortBy' f $ filter' (\x -> x `f` h == EQ || x `f` h == LT) t
        biggerSorted = sortBy' f $ filter' (\x -> x `f` h == GT) t
    in smallerSorted ++ [h] ++ biggerSorted

sort' :: Ord a => [a] -> [a]
sort' = sortBy' compare

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _ [] = []
groupBy' f (h:t) = (h:t1) : groupBy' f t2
    where (t1,t2) = span' (f h) t

group' :: Eq a => [a] -> [[a]]
group' = groupBy' (==)

inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' (h:t) = [] : map' (h:) (inits' t)

tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' (h:t) = (h:t) : tails' t

or' :: [Bool] -> Bool
or' = foldr' (||) False

and' :: [Bool] -> Bool
and' = foldr' (&&) True

not' :: Bool -> Bool
not' x = if x then False else True

any' :: (a -> Bool) -> [a] -> Bool
any' p = or' . map' p

all' :: (a -> Bool)-> [a] -> Bool
all' p = and' . map' p

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ acc [] = acc
foldr' f acc (h:t) = f h (foldr' f acc t)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ acc [] = acc
foldl' f acc (h:t) = f (foldl' f acc t) h

isInfixOf' :: Eq a => [a] -> [a] -> Bool
isInfixOf' sub list =
    let len = length sub in foldl' (\acc x -> (take len x == sub) || acc) False (tails' list)

isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' sub l = foldl' (\acc x -> (x == sub) || acc) False (inits' l)

isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' sub l = foldr' (\x acc -> (x == sub) || acc) False (tails' l)

elem' :: Eq a => a -> [a] -> Bool
elem' e = foldr' (\x acc -> (x==e) || acc) False

notElem' :: Eq a => a -> [a] -> Bool
notElem' e = not' . elem' e

partition' :: (a -> Bool) -> [a] -> ([a],[a])
partition' _ [] = ([], [])
partition' p (h:t) = if p h then ((h:fst next), (snd next)) else ((fst next), (h:snd next))
    where next = partition' p t


find' :: (a -> Bool) -> [a] -> Maybe a
find' p = foldr' (\x acc -> if p x then Just x else acc) Nothing

elemIndex' :: (Eq a) => a -> [a] -> Maybe Int
elemIndex' e l = findIndex' (==e) l

elemIndices' :: (Eq a) => a -> [a] -> [Int]
elemIndices' e l = findIndices' (==e) l

findIndex' :: (Eq a) => (a -> Bool) -> [a] -> Maybe Int
findIndex' p l = index p 0 l
    where
        index _ _ [] = Nothing
        index p i (h:t) = if p h then Just i else index p (succ i) t

findIndices' :: (Eq a) => (a -> Bool) -> [a] -> [Int]
findIndices' p l = indices p 0 l
    where
        indices _ _ [] = []
        indices p i (h:t) = if p h then (i:next) else next
            where next = indices p (succ i) t

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (h1:t1) (h2:t2) = (h1,h2) : zip' t1 t2

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (h1:t1) (h2:t2) = (f h1 h2) : zipWith' f t1 t2

lines' :: String -> [String]
lines' [] = []
lines' l@(h:t) = fst (split l) : lines' (drop' 1 (snd $ split l))
    where split = break' (=='\n')

unlines' :: [String] -> String
unlines' = intercalate' "\n"

words' :: String -> [String]
words' [] = []
words' l@(h:t) = fst (split trimmed) : words' (drop' 1 (snd $ split trimmed))
        where
            p = (\x -> (x =='\n') || (x ==' ')) --todo: better predicate (how do I use chars?)
            split = break' p
            trimmed = reverse' $ dropWhile' p $ reverse' $ dropWhile' p l

unwords' :: [String] -> String
unwords' = intercalate' " "

nubBy' :: (a -> a -> Bool) -> [a] -> [a]
nubBy' _ [] = []
nubBy' f (h:t) = h:(nubBy' f $ remove f h t)
    where
        remove _ _ [] = []
        remove f e (h:t) = if e `f` h then remove f e t else h:(remove f e t)

nub' :: Eq a => [a] -> [a]
nub' = nubBy' (==)

deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' _ _ [] = []
deleteBy' f e (h:t) = if e `f` h then t else h:(deleteBy' f e t)

delete' :: Eq a => a -> [a] -> [a]
delete' = deleteBy' (==)

(\\) :: Eq a => [a] -> [a] -> [a]
(\\) [] _ = []
(\\) _ [] = []
(\\) (h:t) sub = if h `elem'` sub then  t \\ (delete' h sub) else h:(t \\ sub)

unionBy' :: (a -> a -> Bool) -> [a] -> [a] -> [a]
unionBy' f l l2 = l ++ foldr' (\x acc -> if any' (f x) l then acc else x:acc) [] (nubBy' f l2)

union' :: Eq a => [a] -> [a] -> [a]
union' = unionBy' (==)

intersectBy' :: (a -> a -> Bool) -> [a] -> [a] -> [a]
intersectBy' f l1 l2 = foldr' (\x acc -> if any' (f x) l1 then x:acc else acc) [] (nubBy' f l2)

intersect' :: Eq a => [a] -> [a] -> [a]
intersect' = intersectBy' (==)

insertBy' :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy' f e l = fst tuple ++ [e] ++ snd tuple
    where tuple = break' (\x -> e `f` x == LT) l

insert' :: Ord a  => a -> [a] -> [a]
insert' = insertBy' compare

length' :: [a] -> Int
length' = foldr' (\_ acc -> succ acc) 0

maximumBy' :: (a -> a -> Ordering) -> [a] -> a
maximumBy' _ []  = error "empty list"
maximumBy' f (h:t) = foldr' (\x acc -> if x `f` acc == GT then x else acc) h t

minimumBy' :: (a -> a -> Ordering) -> [a] -> a
minimumBy' f = maximumBy' (\x y -> if x `f` y == GT then LT else GT)

maximum' :: Ord a => [a] -> a
maximum' = maximumBy' compare

minimum' :: Ord a => [a] -> a
minimum' = minimumBy' compare