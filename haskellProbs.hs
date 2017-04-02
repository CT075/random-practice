-- Written by Cameron Wong
-- Ordering of parameters in some problems has been changed for convenience
-- In general, list indexing starts at 0 instead of at 1

import Control.Monad
import Control.Arrow

import Data.List
import Data.Function
import Data.Bool
import Data.Maybe

import System.Random

-- util
swap (x,y) = (y,x)

-- 1
last' :: [a] -> a
last' = head . reverse

-- 2
secondFromLast :: [a] -> a
secondFromLast = head . tail . reverse

-- 3
elemAt :: Int -> [a] -> a
elemAt n = head . drop n

-- 4
length' :: [a] -> Int
length' = foldl (const . succ) 0

-- 5
reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

-- 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome = uncurry (==) . (id &&& reverse)

-- 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = join $ map flatten xs

-- 8
compress :: Eq a => [a] -> [a]
compress = nub

compress' :: Eq a => [a] -> [a]
compress' = foldr ffn []
  where
    ffn :: Eq a => a -> [a] -> [a]
    ffn x xs = if x `elem` xs then xs else x:xs

-- 9
pack :: Eq a => [a] -> [[a]]
pack = foldr ffn []
  where
    ffn :: Eq a => a -> [[a]] -> [[a]]
    ffn x [] = [[x]]
    ffn x (xs:xss) = if x `elem` xs then (x:xs):xss else [x]:xs:xss

-- 10
rleEncodePack :: Eq a => [a] -> [(Int, a)]
rleEncodePack = map (length &&& head) . pack

-- 11
data RLEAtom a = Raw a | Run Int a
makeRun l v = if l == 1 then Raw v else Run l v

rleEncodeModified :: Eq a => [a] -> [RLEAtom a]
rleEncodeModified = map (uncurry makeRun) . rleEncodePack

-- 12
rleDecode :: Eq a => [RLEAtom a] -> [a]
rleDecode = foldr decode []
  where
    decode (Raw x) xs = x:xs
    decode (Run l x) xs = replicate l x ++ xs

-- 13
rleEncode :: Eq a => [a] -> [RLEAtom a]
rleEncode [] = []
rleEncode (x:xs) =
  makeRun l x:rleEncode (dropWhile (==x) xs)
  where
    l = 1 + (length $ takeWhile (== x) xs)

-- 14
dupli :: Eq a => [a] -> [a]
dupli = rleDecode . map (Run 2)

-- 15
repli :: Eq a => Int -> [a] -> [a]
repli n = rleDecode . map (Run n)

-- 16
dropNth :: Int -> [a] -> [a]
dropNth n xs =
  let
    l = length xs
    enums = zip [1..l] xs
  in
    map snd $ filter (\(l,_) -> l `mod` n /= 0) enums

-- 17
split :: Int -> [a] -> ([a],[a])
split n = take n &&& drop n

-- 18
slice :: (Int, Int) -> [a] -> [a]
slice (a, b) = take (b-a) . drop a

-- 19
rotate :: Int -> [a] -> [a]
rotate n = uncurry (++) . (drop n &&& take n)

-- 20
rmAt :: Int -> [a] -> [a]
rmAt n = uncurry (++) . (take n &&& drop (n+1))

-- 21
insertAt :: Int -> a -> [a] -> [a]
insertAt n v vs = take n vs ++ (v:drop n vs)

-- 22
range :: Int -> Int -> [Int]
range a b = [a..b]

-- these all require System.Random, and I don't want to deal with that for now
-- 23
-- 24
-- 25

-- 26
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs

-- 27
groups :: Eq a => [Int] -> [a] -> [[[a]]]
groups [] _ = [[]]
groups (l:ls) vals =
  let
    gs = combinations l vals
    remove g = filter (not . flip elem g)
    makeGroup = uncurry ($) . (map . (:) &&& (groups ls . ($ vals) . remove))
  in
    join $ map makeGroup gs

-- 28
lsort :: [[a]] -> [[a]]
lsort = sortBy (compare `on` length)

lfsort :: [[a]] -> [[a]]
lfsort l = sortBy (compare `on` (lfreq . length)) l
  where
    lns = map length l
    lfreq n = length $ filter (== n) lns

-- 31
isPrime :: Int -> Bool
isPrime n = not $ any (\v -> n `mod` v == 0) [2..(n `div` 2)]

-- 32
gcd' :: Int -> Int -> Int
gcd' x y =
  if y == 0 then x else gcd' y (x `mod` y)

-- 33
coprime :: Int -> Int -> Bool
coprime = curry ((== 1) . uncurry gcd)

-- 34
totientPhi :: Int -> Int
totientPhi n = length $ filter (coprime n) [1..(n-1)]

-- 35
primeFactors :: Int -> [Int]
primeFactors n =
  let
    primes = filter isPrime [2..(n `div` 2)]
    factorHelper 1 _ = []
    factorHelper n (l@(p:ps)) =
      if n `mod` p == 0
        then p:factorHelper (n `div` p) l
        else factorHelper n ps
    -- This can't really happen
    factorHelper _ [] = []
  in
    factorHelper n primes

-- 36
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult = map swap . rleEncodePack . primeFactors

-- 37
phi :: Int -> Int
phi = foldl1 (*) . map pwr . primeFactorsMult
  where
    pwr =
      uncurry (*) .
      ((subtract 1 . fst) &&& (uncurry (^) . second (subtract 1)))

-- 38 does not provide a problem statement for which code needs to be written
-- 39
primesR :: Int -> Int -> [Int]
primesR a b = filter isPrime [a..b]

-- 40
goldbach :: Int -> (Int, Int)
goldbach n = head $ dropWhile sumCheck (liftM2 (,) primes primes)
  where
    sumCheck (x,y) = x+y /= n
    primes = primesR 2 n

-- 41
-- This one is pretty inefficient
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList a b = map goldbach $ filter even [a..b]

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' a b x = filter (\(n,m) -> n > x && m > x) $ goldbachList a b

-- 46, 47
-- These are basically the same thing

a `and'` b = a && b
a `or'` b = a || b
infixl 3 `equ'`
a `equ'` b = a == b

ttable :: (Bool -> Bool -> Bool) -> IO ()
ttable f = putStrLn $ intercalate "\n" $
  map (format . (id &&& uncurry f))
  [(True, True), (True, False), (False, True), (False, False)]
  where
    format ((x,y),z) = show x ++ " " ++ show y ++ " " ++ show z

-- 48
ttablen :: Int -> ([Bool] -> Bool) -> IO ()
ttablen n f = putStrLn $ intercalate "\n" $
  map (format . (id &&& f) . boolify n) [0..(2^n-1)]
  where
    boolify :: Int -> Int -> [Bool]
    boolify x 0 = replicate x False
    boolify x n = (not $ even n) : boolify (x-1) (n `div` 2)
    format (l, z) = (intercalate " " $ map show l) ++ " " ++ show z

-- 49
gray :: Int -> [String]
gray 0 = [""]
gray n =
  let
    xs = gray (n-1)
  in
    map ('0':) xs ++ map ('1':) (reverse xs)

-- 50
data HuffTree a = Freq a | Step (HuffTree a, HuffTree a)
-- REQUIRES: input list is sorted by f
htree :: [(Char, Int)] -> HuffTree Char
htree [(c,f)] = Freq c
htree ((c,f):frqs) = Step (Freq c, htree frqs)
-- this can't happen
htree [] = Freq '\0'

getCodes :: HuffTree a -> [(a, String)]
getCodes (Freq c) = [(c, "")]
getCodes (Step (x, y)) =
  map (second ('0':)) (getCodes x) ++ map (second ('1':)) (getCodes y)

huffman :: [(Char, Int)] -> [(Char, String)]
huffman = getCodes . htree . reverse . sortBy (compare `on` snd)

-- 55
data Tree a = EmptyTree | Branch a (Tree a) (Tree a)
              deriving (Eq)

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [EmptyTree]
cbalTree n =
  let
    (q,r) = (n-1) `quotRem` 2
  in
    [Branch 'x' left right | i <- [q..(q+r)]
                           , left <- cbalTree i
                           , right <- cbalTree (n-i-1)]

-- 56
symmetric :: Eq a => Tree a -> Bool
symmetric EmptyTree = True
symmetric (Branch _ l r) = isMirror l r
  where
    isMirror EmptyTree EmptyTree = True
    isMirror (Branch x1 l1 r1) (Branch x2 l2 r2) =
      x1 == x2 && isMirror l1 r2 && isMirror l2 r1
    isMirror _ _ = False

-- 57
construct :: Ord a => [a] -> Tree a
construct [] = EmptyTree
construct (x:xs) = Branch x (construct l) (construct r)
  where (l, r) = splitAt (length xs `div` 2) xs

-- 58
symCbalTrees :: Int -> [Tree Char]
symCbalTrees = filter symmetric . cbalTree

-- 59
hbalTree :: a -> Int -> [Tree a]
hbalTree v 0 = [EmptyTree]
hbalTree v 1 = [Branch v EmptyTree EmptyTree]
hbalTree v n = [Branch v l r | (hl, hr) <- heights
                             , l <- hbalTree v hl
                             , r <- hbalTree v hr]
  where
    heights = [(n-1,n-1),(n-1,n-2),(n-2,n-1)]

-- 60
minNodes :: [Int]
minNodes = 0 : 1 : zipWith ((+).(+1)) minNodes (tail minNodes)

maxHeight :: Int -> Int
maxHeight x = checkIn 0 minNodes
  where
    checkIn y (n:ns) =
      if x <= n
        then y-1
        else checkIn (y+1) ns

hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes v n = filter (\t -> tsize t == n) (hbalTree v height)
  where
    height = maxHeight n
    tsize :: Tree a -> Int
    tsize EmptyTree = 0
    tsize (Branch _ l r) = 1 + tsize l + tsize r

-- 61
countLeaves :: Tree a -> Int
countLeaves EmptyTree = 0
countLeaves (Branch _ EmptyTree EmptyTree) = 1
countLeaves (Branch _ l r) = countLeaves l + countLeaves r

-- 62a
leaves :: Tree a -> [a]
leaves EmptyTree = []
leaves (Branch v EmptyTree EmptyTree) = [v]
leaves (Branch _ l r) = leaves l ++ leaves r

-- 62
internals :: Tree a -> [a]
internals EmptyTree = []
internals (Branch _ EmptyTree EmptyTree) = []
internals (Branch v l r) = v:(internals l ++ internals r)

-- 62b
atLevel :: Tree a -> Int -> [a]
atLevel EmptyTree _ = []
atLevel (Branch v _ _) 0 = [v]
atLevel (Branch _ l r) n = atLevel l (n-1) ++ atLevel r (n-1)

-- 63
completeTree :: Int -> Tree Char
completeTree 0 = EmptyTree
completeTree n = generate 1
  where
    generate x =
      if x > n
        then EmptyTree
        else Branch 'x' (generate (2*x)) (generate (2*x+1))

-- 64
toCoordTree :: Tree a -> Tree (a, (Int,Int))
toCoordTree t = fst (calcLayout 1 1 t)
  where
    calcLayout x _ EmptyTree = (EmptyTree, x)
    calcLayout x y (Branch a l r) =
      let
        (l', x') = calcLayout x (y+1) l
        (r', x'') = calcLayout (x'+1) (y+1) r
      in (Branch (a, (x', y)) l' r', x'')

-- these ones are too hard lol
-- 65
-- 66

-- I've recently discovered that, for 67-69 that these should be for Tree Char
-- instead of arbitrary Tree a. I'll come back and fix them at a later date.
-- 67
instance Show a => Show (Tree a) where
  show EmptyTree = ""
  show (Branch x EmptyTree EmptyTree) = show x
  show (Branch x l r) = show x ++ "(" ++ show l ++ "," ++ show r ++ ")"

-- parsing is also hard

-- 68a
preorder :: Show a => Tree a -> String
preorder EmptyTree = ""
preorder (Branch x l r) = show x ++ preorder l ++ preorder r

inorder :: Show a => Tree a -> String
inorder EmptyTree = ""
inorder (Branch x l r) = inorder l ++ show x ++ inorder r

-- parsing is still hard

-- 69
dotstring :: Show a => Tree a -> String
dotstring EmptyTree = "."
dotstring (Branch x l r) = show x ++ dotstring l ++ dotstring r

-- 70C
-- We *could* use Data.Tree, but we've already declared Tree as a datatype.
data Forest a = Clearing | Shrub a [Forest a]

nnodes :: Forest a -> Int
nnodes Clearing = 0
nnodes (Shrub _ ts) = 1 + (sum $ map nnodes ts)

-- 70

-- 71
ipl :: Forest a -> Int
ipl = ipl' 0
  where ipl' d (Shrub _ ts) = d + sum (map (ipl' (d+1)) ts)

-- 72
bottomUp :: Forest a -> [a]
bottomUp Clearing = []
bottomUp (Shrub x ts) = concatMap bottomUp ts ++ [x]

-- 73
displayLisp :: Show a => Forest a -> String
displayLisp Clearing = "()"
displayLisp (Shrub x []) = show x
displayLisp (Shrub x ts) =
  "(" ++ show x ++ " " ++ (intercalate " " $ map displayLisp ts) ++ ")"

-- 80
-- default type is in graph-term form
-- We will define a "labeled" graph to be a tuple (AbsGraph, [(Int, a)])
data AbsGraph = AGraph ([Int], [(Int, Int)]) deriving Show

edgeEq :: (Int, Int) -> (Int, Int) -> Bool
edgeEq p1 p2@(x2,y2) = p1 == p2 || p1 == (y2,x2)

undirect :: AbsGraph -> AbsGraph
undirect (AGraph (nodes, edges)) = AGraph (nodes, nubBy edgeEq edges)

toEdges :: AbsGraph -> [(Int, Int)]
toEdges (AGraph vs) = snd vs

toArcs :: AbsGraph -> [(Int, Int)]
toArcs = toEdges

fromArcs :: [(Int, Int)] -> AbsGraph
fromArcs = AGraph . ((nub . fltn) &&& id)
  where
    fltn :: [(Int, Int)] -> [Int]
    fltn [] = []
    fltn ((x,y):xs) = x:y:fltn xs

fromEdges :: [(Int, Int)] -> AbsGraph
fromEdges = undirect . fromArcs

toDAdj :: AbsGraph -> [(Int, [Int])]
toDAdj (AGraph (cs, arcs)) = map convert cs
  where
    convert i = (i, [y | (x,y) <- arcs, x==i])

toAdj :: AbsGraph -> [(Int, [Int])]
toAdj (AGraph (cs, edges)) = map convert cs
  where
    convert i = (i, [x | (x,y) <- edges, y==i] ++ [y | (x,y) <- edges, x==i])

fromDAdj :: [(Int, [Int])] -> AbsGraph
fromDAdj = AGraph . (map fst &&& (nub . concat . map makeEdges))
  where
    makeEdges = uncurry map . ((,) *** id)

fromAdj :: [(Int, [Int])] -> AbsGraph
fromAdj = undirect . fromDAdj

-- 81
paths :: Int -> Int -> AbsGraph -> [[Int]]
paths src dst g = paths' src dst $ toArcs g
  where
    paths' src dst arcs =
      if src == dst
        then [[dst]]
        else [src:path |
          arc <- arcs, (fst arc == src),
          path <- (paths' (snd arc) dst) $ filter (/= arc) arcs
        ]

-- 82
cycles :: Int -> AbsGraph -> [[Int]]
cycles n g =
  nubBy (\x y -> (x `isPrefixOf` y) || (y `isPrefixOf` x)) $
  map (++[n]) $ concat $ map (flip (paths n) g) neighbors
  where
    neighbors = snd $ fromJust $ find ((== n) . fst) (toDAdj g)

