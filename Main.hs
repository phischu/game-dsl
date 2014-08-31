{-# LANGUAGE StandaloneDeriving,DeriveFunctor #-}
module Main where

import Control.Monad.Free
import Graphics.Gloss
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map (lookup,keys,filter,insert,empty,delete,adjust)
import Data.Set (Set)
import qualified Data.Set as Set (member,empty,delete,insert)
import Control.Monad.State (State)
import qualified Control.Monad.State as State (get,put,modify)

data Tag = Selected | Stone

deriving instance Eq Tag
deriving instance Ord Tag

data Attribute = XPosition | YPosition

deriving instance Eq Attribute
deriving instance Ord Attribute

type Entity = Integer

type Action = Free ActionF

data ActionF a =
    Tagged Tag (Entity -> a) |
    Get Attribute Entity (Integer -> a) |
    No (Action ()) a |
    Assert Bool a |
    For [a]

deriving instance Functor ActionF

data Clickable = Clickable Area (Effect ())

data Area = Rect Float Float Float Float

type Effect = Free EffectF

data EffectF a =
    New (Entity -> a) |
    Delete Entity a |
    Set Attribute Entity Integer a |
    Untag Tag Entity a |
    Tag Tag Entity a

deriving instance Functor EffectF

type Render = Action Picture

data GameState = GameState Integer (Map Entity (Set Tag,Map Attribute Integer))

runAction :: GameState -> Action a -> [a]
runAction _ (Pure a) = [a]
runAction gamestate@(GameState _ entities) (Free f) = case f of
    Tagged t k -> do
        entity <- Map.keys (Map.filter (\(tags,_) -> Set.member t tags) entities)
        runAction gamestate (k entity)
    Get attribute entity k -> do
        (_,attributes) <- lookupList entity entities
        value <- lookupList attribute attributes
        runAction gamestate (k value)
    No action k -> do
        let units = runAction gamestate action
        if null units
            then runAction gamestate k
            else []
    Assert b k -> do
        if b
            then runAction gamestate k
            else []
    For actions -> do
        action <- actions
        runAction gamestate action


lookupList :: (Ord k) => k -> Map k v -> [v]
lookupList k m = maybe [] (:[]) (Map.lookup k m)

runEffect :: Effect a -> State GameState a
runEffect (Free f) = case f of
    New k -> do
        (GameState newentity entities) <- State.get
        State.put (GameState (newentity + 1) (Map.insert newentity (Set.empty,Map.empty) entities))
        runEffect (k newentity)
    Delete entity k -> do
        (GameState newentity entities) <- State.get
        State.put (GameState newentity (Map.delete entity entities))
        runEffect k
    Set attribute entity value k -> do
        State.modify (modifyEntity entity (\(tags,attributes) -> (tags,Map.insert attribute value attributes)))
        runEffect k
    Untag t entity k -> do
        State.modify (modifyEntity entity (\(tags,attributes) -> (Set.delete t tags,attributes)))
        runEffect k
    Tag t entity k -> do
        State.modify (modifyEntity entity (\(tags,attributes) -> (Set.insert t tags,attributes)))
        runEffect k

modifyEntity :: Entity -> ((Set Tag,Map Attribute Integer) -> (Set Tag,Map Attribute Integer)) -> GameState -> GameState
modifyEntity entity f (GameState newentity entities) = GameState newentity (Map.adjust f entity entities)

tagged :: Tag -> Action Entity
tagged t = liftF (Tagged t id)

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

untag :: Tag -> Entity -> Effect ()
untag t entity = liftF (Untag t entity ())

tag :: Tag -> Entity -> Effect ()
tag t entity = liftF (Tag t entity ())

new :: Effect Entity
new = liftF (New id)

move :: Action Clickable
move = do
    selected <- tagged Selected
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
    stone <- tagged Stone
    x' <- get XPosition stone
    y' <- get YPosition stone
    assert (x == x')
    assert (y == y')
    return stone

select :: Action Clickable
select = do
    stone <- tagged Stone
    x <- get XPosition stone
    y <- get YPosition stone
    selected <- tagged Selected
    return (Clickable (fieldRect x y) (do
        untag Selected selected
        tag Selected stone))

setupBoard :: Effect ()
setupBoard = do
    mapM_ (uncurry newStone) (
        [(x,y) | x <- [-1,0,1], y <- [-1,0,1], not (x == 0 && y == 0)] ++
        [(x,y) | x <- [-1,0,1], y <- [2,3]] ++
        [(x,y) | x <- [-1,0,1], y <- [-2,-3]] ++
        [(x,y) | x <- [2,3], y <- [-1,0,1]] ++
        [(x,y) | x <- [-2,-3], y <- [-1,0,1]])

newStone :: Integer -> Integer -> Effect ()
newStone x y = do
    stone <- new
    tag Stone stone
    set XPosition stone x
    set YPosition stone y


renderStone :: Render
renderStone = do
    stone <- tagged Stone
    x <- get XPosition stone
    y <- get YPosition stone
    return (translate (fieldCoordinate x) (fieldCoordinate y) (circle 20))

renderSelected :: Render
renderSelected = do
    selected <- tagged Selected
    x <- get XPosition selected
    y <- get YPosition selected
    return (translate (fieldCoordinate x) (fieldCoordinate y) (rectangleWire 20 20))

fieldRect :: Integer -> Integer -> Area
fieldRect x y = Rect (fieldCoordinate x) (fieldCoordinate y) 40 40

fieldCoordinate :: Integer -> Float
fieldCoordinate x = fromIntegral x * 20

main :: IO ()
main = putStrLn "hallo"








