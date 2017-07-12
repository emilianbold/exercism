module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef
import Data.Maybe

type Var x = IORef (Maybe x)
data Node a = Node { value  :: a,
                     front  :: Var (Node a),
                     back   :: Var (Node a) 
                } deriving Eq
data Deque a = Deque (Var (Var (Node a), Var (Node a)))

mkDeque :: IO (Deque a)
mkDeque = Deque <$> mkNull 

mkNull :: IO (Var a)
mkNull = newIORef Nothing

mkVar :: a -> IO (Var a)
mkVar a = newIORef (Just a)

set' :: (Var a) -> a -> IO ()
set' ref x = writeIORef ref (Just x)

clear' :: (Var a) -> IO ()
clear' ref = writeIORef ref Nothing

get' :: (Var a) -> IO a
get' ref = fromJust <$> readIORef ref

peek' :: Var a -> IO (Maybe a)
peek' = readIORef

isNull :: (Eq a) => (Var a) -> IO Bool
isNull ref = (==Nothing) <$> readIORef ref 

mkNode :: a -> IO (Node a)
mkNode x = do
        left <- mkNull 
        right <- mkNull 
        return (Node x left right)

-- remove back
pop :: (Eq a) => Deque a -> IO (Maybe a)
pop (Deque ref) = peek' ref >>= remove
    where
          remove :: (Eq a) => Maybe (Var (Node a), Var (Node a)) -> IO (Maybe a) 
          remove Nothing = return Nothing
          remove (Just (_, end)) = do
                    z <- get' end
                    singleton <- isNull (front z)
                    if singleton then do
                        clear' ref
                        return (Just (value z))
                    else do
                        y <- get' (front z)
                        clear' (back y)
                        set' end y
                        return (Just (value z))
 

-- insert back
push :: Deque a -> a -> IO ()
push (Deque ref) v = peek' ref >>= add >> return ()
        where add Nothing = singleton ref v 
              add (Just (_, end)) = do
                    y <- get' end 
                    z <- mkNode v
                    set' (front z) y
                    set' (back y) z
                    set' end z

singleton :: Var (Var (Node a), Var (Node a)) -> a -> IO ()
singleton ref x = do
                    n <- mkNode x
                    front <- mkVar n
                    back <- mkVar n
                    set' ref (front, back)

-- insert front
unshift :: Deque a -> a -> IO ()
unshift (Deque ref) x = peek' ref >>= add >> return ()
    where add Nothing = singleton ref x 
          add (Just (start,_)) = do
            a <- mkNode x 
            b <- get' start
            set' (back a) b
            set' (front b) a
            set' start a

-- remove front
shift :: (Eq a) => Deque a -> IO (Maybe a)
shift (Deque ref) = peek' ref >>= remove
    where remove Nothing = return Nothing
          remove (Just (start, _)) = do
            a <- get' start
            singleton <- isNull (back a)
            if singleton then do
                clear' ref
                return (Just (value a))
            else do
                b <- get' (back a)
                clear' (front b)
                set' start b
                return (Just (value a))
