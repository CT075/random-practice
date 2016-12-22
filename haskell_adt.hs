{-# LANGUAGE RebindableSyntax #-}
-- We need this line to ensure that we can still use do syntax even with our
-- "monad-like" structure

import Prelude hiding ((>>), (>>=), return)

-- Cameron Wong

class ADT a where
      empty :: a -> Bool

type ADTResult adt a = (adt, a)
type Action adt a = adt -> ADTResult adt a

-- Stack declaration and operations
data Stack a = EmptyStack | Stack { val :: a
                                  , next :: Stack a
                                  } deriving (Show)

instance ADT (Stack a) where
         empty EmptyStack = True
         empty _ = False

push :: a -> Action (Stack a) ()
push n = \s -> (Stack {val=n, next=s}, ())

pop :: Action (Stack a) (Maybe a)
pop EmptyStack = (EmptyStack, Nothing)
pop s = (next s, Just $ val s)

stackPeek :: Stack a -> Maybe a
stackPeek EmptyStack = Nothing
stackPeek s = Just $ val s

-- Queue declaration and operations
-- I spent a solid 11 or 12 hours trying to make a cyclic doubly-linked list
-- implementation work before giving up because i data recursion sucks
data Queue a = Queue { enq_s :: Stack a
                     , deq_s :: Stack a
                     } deriving (Show)

emptyQueue = Queue {enq_s=EmptyStack, deq_s=EmptyStack}

-- TODO: See if we can rewrite this using extract and comonads
enq :: a -> Action (Queue a) ()
enq n q = (q {enq_s = enq_s'}, ())
          where (enq_s', _) = push n $ enq_s q

-- XXX: i love it
deq :: Action (Queue a) (Maybe a)
deq Queue {enq_s=EmptyStack, deq_s=EmptyStack} =
          (Queue {enq_s=EmptyStack, deq_s=EmptyStack}, Nothing)
deq Queue {enq_s=q, deq_s=EmptyStack} =
          let push' EmptyStack s = s
              push' s1 s2 = push' s1' s2'
                            where (s1', Just v) = pop s1
                                  (s2', _) = push v s2
              dqs = push' EmptyStack q
          in deq Queue {enq_s=EmptyStack, deq_s=dqs}
deq q =
    let (dq, v) = pop $ deq_s q
        q' = q {deq_s=dq}
    in (q', v)

-- BST declaration and operations
data BST a = EmptyTree | BST { root :: a
                       , left :: BST a
                       , right :: BST a
                       } deriving (Show)

bstInsert :: Ord a => a -> Action (BST a) ()
bstInsert n = (\x -> (x, ())) . insert'
          where insert' EmptyTree = BST {root=n,left=EmptyTree,right=EmptyTree}
                insert' t
                      | n == root t = t
                      | n < root t = t {left=insert' $ left t}
                      | otherwise = t {right=insert' $ right t}

bstSearch :: Ord a => a -> BST a -> Bool
bstSearch _ EmptyTree = False
bstSearch n t
        | n == root t = True
        | n < root t = bstSearch n $ left t
        | otherwise = bstSearch n $ right t

remHelper BST {left=EmptyTree, right=EmptyTree} = EmptyTree
remHelper BST {left=l, right=EmptyTree} = l
remHelper BST {left=EmptyTree, right=r} = r
remHelper t = removedMin {root=rightMin}
              where minTree BST {root=r, left=EmptyTree} = r
                    --minTree = minTree . left
                    minTree t' = minTree $ left t'
                    rightMin = minTree $ right t
                    (removedMin, _) = bstRem rightMin t

bstRem :: Ord a => a -> Action (BST a) (Maybe a)
bstRem _ EmptyTree = (EmptyTree, Nothing)
bstRem n t
     -- I'm sure there's a far more elegant way to implement the next four
     -- lines, probably involving an inordinate amount of the characters '>'
     -- and '$'.
     | n < root t =
           let (result, v) = bstRem n $ left t
           in (t {left=result}, v)
     | n > root t = 
           let (result, v) = bstRem n $ right t
           in (t {right=result}, v)
     -- n is the value of the current root node
     | otherwise = (remHelper t, Just n)

-- AVL tree functions (uses BST definition above)

treeHeight :: BST a -> Int
treeHeight EmptyTree = 0
--treeHeight t = maximum $ map ((+1) . treeHeight . ($t)) [left, right]
treeHeight BST {left=l, right=r} = 1 + max (treeHeight l) (treeHeight r)

-- actual avl stuff snipped because it sucked and i want to redo it

-- test functions

-- Convenience/notational things
(>>=) :: Action adt a -> (a -> Action adt b) -> Action adt b
f >>= cnt = \s ->
                let (s', v) = f s
                    c = cnt v
                in c s'
(>>) :: Action adt a -> Action adt b -> Action adt b
x >> y = x >>= \_ -> y

return :: a -> Action adt a
return n = \s -> (s, n)

testStackOps = do
  push 3
  push 2
  push 1
  Just x <- pop
  Just y <- pop
  push (x+y)
  Just z <- pop
  Just a <- pop
  push (x*y)
  r <- pop
  return r

testStackFail = do
  push 1
  pop
  pop
  pop

testQueueOps = do
  enq 1
  enq 2
  Just x <- deq
  enq 3
  Just y <- deq
  enq (x+y)
  Just z <- deq
  Just a <- deq
  enq (z*a)
  r <- deq
  return r

testQueueFail = do
  enq 1
  deq
  deq
  deq

