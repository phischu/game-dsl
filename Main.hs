{-# LANGUAGE StandaloneDeriving,DeriveFunctor #-}
module Main where

import Control.Monad.Free
import Graphics.Gloss
import Control.Monad

data Class = Selected | Stone

deriving instance Eq Class

data Attribute = XPosition | YPosition

deriving instance Eq Attribute

type Entity = Integer

type Action = Free ActionF

data ActionF a =
    A Class (Entity -> a) |
    Get Attribute Entity (Integer -> a) |
    No (Action ()) a |
    Assert Bool a |
    For [a]

deriving instance Functor ActionF

data Clickable = Clickable Area (Effect ())

data Area = Rect Float Float Float Float

type Effect = Free EffectF

data EffectF a =
    Delete Entity a |
    Set Attribute Entity Integer a |
    Revoke Class Entity a |
    Assure Class Entity a

deriving instance Functor EffectF

type Render = Action Picture

data GameState = GameState [(Entity,([Class],[(Attribute,Integer)]))]

runAction :: GameState -> Action a -> [a]
runAction _ (Pure a) = [a]
runAction (GameState gamestate) (Free f) = case f of
    A c k -> do
        (e,(cs,_)) <- gamestate
        guard (elem c cs)
        runAction (GameState gamestate) (k e)
    Get a e k -> do
        (e',(_,m)) <- gamestate
        guard (e == e')
        (a',i) <- m
        guard (a == a')
        runAction (GameState gamestate) (k i)
    No act k -> do
        let as = runAction (GameState gamestate) act
        if null as then runAction (GameState gamestate) k else []
    Assert b k -> do
        if b then runAction (GameState gamestate) k else []
    For ks -> do
        k <- ks
        runAction (GameState gamestate) k

an :: Class -> Action Entity
an c = liftF (A c id)

get :: Attribute -> Entity -> Action Integer
get a e = liftF (Get a e id)

no :: Action a -> Action ()
no act = liftF (No (void act) ())

assert :: Bool -> Action ()
assert b = liftF (Assert b ())

for :: [a] -> Action a
for as = liftF (For as)

delete :: Entity -> Effect ()
delete e = liftF (Delete e ())

set :: Attribute -> Entity -> Integer -> Effect ()
set a e i = liftF (Set a e i ())

revoke :: Class -> Entity -> Effect ()
revoke c e = liftF (Revoke c e ())

assure :: Class -> Entity -> Effect ()
assure c e = liftF (Assure c e ())

move :: Action Clickable
move = do
    selected <- an Selected
    x <- get XPosition selected
    y <- get YPosition selected
    (dx,dy) <- for [(1,0),(-1,0),(0,1),(0,-1)]
    let (nx,ny) = (x + dx, y + dy)
        (tx,ty) = (nx + dx, ny + dy)
    other <- stoneAt nx ny
    no (stoneAt tx ty)
    return (Clickable (fieldRect tx ty) (do
        delete other
        set XPosition selected tx
        set YPosition selected ty))

stoneAt :: Integer -> Integer -> Action Entity
stoneAt x y = do
    stone <- an Stone
    x' <- get XPosition stone
    y' <- get YPosition stone
    assert (x == x')
    assert (y == y')
    return stone

select :: Action Clickable
select = do
    stone <- an Stone
    x <- get XPosition stone
    y <- get YPosition stone
    selected <- an Selected
    return (Clickable (fieldRect x y) (do
        revoke Selected selected
        assure Selected stone))

renderStone :: Render
renderStone = do
    stone <- an Stone
    x <- get XPosition stone
    y <- get YPosition stone
    return (translate (fieldCoordinate x) (fieldCoordinate y) (circle 20))

renderSelected :: Render
renderSelected = do
    selected <- an Selected
    x <- get XPosition selected
    y <- get YPosition selected
    return (translate (fieldCoordinate x) (fieldCoordinate y) (rectangleWire 20 20))

fieldRect :: Integer -> Integer -> Area
fieldRect x y = Rect (fieldCoordinate x) (fieldCoordinate y) 40 40

fieldCoordinate :: Integer -> Float
fieldCoordinate x = fromIntegral x * 20

main :: IO ()
main = putStrLn "hallo"








