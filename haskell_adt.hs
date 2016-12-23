-- Cameron Wong

import Control.Applicative (Applicative)
import Control.Monad (liftM, ap)

class ADT a where
      empty :: a -> Bool

-- I'm aware that this is basically recreating a state monad
type ADTResult adt a = (adt, a)
newtype Action adt a = Action (adt -> ADTResult adt a)

instance Monad (Action adt) where
         (Action f) >>= cnt = Action $ \s ->
                                          let (s', v) = f s
                                              Action c = cnt v
                                          in c s'
         return n = Action $ \s -> (s, n)

instance Functor (Action adt) where
         fmap = liftM

instance Applicative (Action adt) where
         pure = return
         (<*>) = ap

eval (Action f) = f

-- Stack declaration and operations
data Stack a = EmptyStack | Stack { val :: a
                                  , next :: Stack a
                                  } deriving (Show)

instance ADT (Stack a) where
         empty EmptyStack = True
         empty _ = False

push :: a -> Action (Stack a) ()
push n = Action $ \s -> (Stack {val=n, next=s}, ())

pop :: Action (Stack a) (Maybe a)
pop = Action pop'
      where pop' EmptyStack = (EmptyStack, Nothing)
            pop' s = (next s, Just $ val s)

stackPeek :: Stack a -> Maybe a
stackPeek EmptyStack = Nothing
stackPeek s = Just $ val s

-- Queue declaration and operations
-- I spent a solid 11 or 12 hours trying to make a cyclic doubly-linked list
-- implementation work before giving up because i data recursion sucks
data Queue a = Queue { enq_s :: Stack a
                     , deq_s :: Stack a
                     } deriving (Show)

instance ADT (Queue a) where
         empty q = (empty $ enq_s q) && (empty $ deq_s q)

emptyQueue = Queue {enq_s=EmptyStack, deq_s=EmptyStack}

-- TODO: See if we can rewrite this using extract and comonads
enq :: a -> Action (Queue a) ()
enq n = Action $ \q ->
                    let (enq_s', _) = eval (push n) $ enq_s q
                    in (q {enq_s = enq_s'}, ())

deq' :: Queue a -> (Queue a, Maybe a)
deq' q
   | empty q = (emptyQueue, Nothing)
   | empty $ deq_s q =
             let push' EmptyStack s = s
                 push' s1 s2 = push' s1' s2'
                               where (s1', Just v) = eval pop s1
                                     (s2', _) = eval (push v) s2
                 dqs = push' (enq_s q) EmptyStack
             in deq' Queue {enq_s=EmptyStack, deq_s=dqs}
   | otherwise = let (dq, v) = eval pop $ deq_s q
                     q' = q {deq_s=dq}
                 in (q', v)

deq :: Action (Queue a) (Maybe a)
deq = Action deq'

-- BST declaration and operations
data BST a = EmptyTree | BST { root :: a
                       , left :: BST a
                       , right :: BST a
                       } deriving (Show)

instance ADT (BST a) where
         empty EmptyTree = True
         empty _ = False

bstInsert :: Ord a => a -> Action (BST a) ()
bstInsert n = Action $ ((\x -> (x, ())) . insert')
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

remHelper :: Ord a => BST a -> BST a
remHelper t
        | empty t = EmptyTree
        | empty $ right t = left t
        | empty $ left t = right t
        | otherwise = removedMin {root=rightMin}
                      where minTree u =
                                    if empty $ left u then root u 
                                                      else minTree $ left u
                            rightMin = minTree $ right t
                            (removedMin, _) = bstRem' rightMin t

bstRem' :: Ord a => a -> (BST a -> (BST a, Maybe a))
bstRem' _ EmptyTree = (EmptyTree, Nothing)
bstRem' n t
      -- I'm sure there's a far more elegant way to implement the next four
      -- lines, probably involving an inordinate amount of the characters '>'
      -- and '$'.
      | n < root t =
            let (result, v) = bstRem' n $ left t
            in (t {left=result}, v)
      | n > root t = 
            let (result, v) = bstRem' n $ right t
            in (t {right=result}, v)
      -- n is the value of the current root node
      | otherwise = (remHelper t, Just n)

bstRem :: Ord a => a -> Action (BST a) (Maybe a)
bstRem = Action . bstRem'

-- AVL tree functions (uses BST definition above)

treeHeight :: BST a -> Int
treeHeight EmptyTree = 0
--treeHeight t = maximum $ map ((+1) . treeHeight . ($t)) [left, right]
treeHeight BST {left=l, right=r} = 1 + max (treeHeight l) (treeHeight r)

-- actual avl stuff snipped because it sucked and i want to redo it

-- test functions

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

