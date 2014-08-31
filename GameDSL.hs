{-# LANGUAGE StandaloneDeriving,DeriveFunctor #-}
module GameDSL where

import Control.Monad.Free
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map (lookup,keys,filter,insert,empty,delete,adjust)
import Data.Set (Set)
import qualified Data.Set as Set (member,empty,delete,insert)
import Control.Monad.State (State)
import qualified Control.Monad.State as State (get,put,modify,execState)

data Tag = Selected | Stone

deriving instance Eq Tag
deriving instance Ord Tag
deriving instance Show Tag

data Attribute = XPosition | YPosition

deriving instance Eq Attribute
deriving instance Ord Attribute
deriving instance Show Attribute

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

instance Show Clickable where
    show (Clickable area _) = show area

data Area = Rect Float Float Float Float

deriving instance Show Area

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

deriving instance Show GameState

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
runEffect (Pure a) = return a
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

runGame :: Effect () -> [Action Clickable] -> [Render] -> IO ()
runGame setup actions renders = play
    (InWindow "Game DSL" (500,500) (200,200))
    white
    40
    (run setup actions emptyState)
    (render renders)
    (handle actions)
    (const id)

emptyState :: GameState
emptyState = GameState 0 Map.empty

run :: Effect () -> [Action Clickable] -> GameState -> ([Clickable],GameState)
run effect actions gamestate = (clickables,gamestate') where
    gamestate' = State.execState (runEffect effect) gamestate
    clickables = concatMap (runAction gamestate') actions

render :: [Render] -> ([Clickable],GameState) -> Picture
render renders (_,gamestate) = pictures (concatMap (runAction gamestate) renders)

handle :: [Action Clickable] -> Event -> ([Clickable],GameState) -> ([Clickable],GameState)
handle actions (EventKey (MouseButton _) _ _ (x,y)) (clickables,gamestate) = case filterInside x y clickables of
    [] -> (clickables,gamestate)
    (effect:_) -> run effect actions gamestate
handle _ _ (clickables,gamestate) = (clickables,gamestate)

filterInside :: Float -> Float -> [Clickable] -> [Effect ()]
filterInside x y clickables = do
    Clickable (Rect mx my w h) effect <- clickables
    guard (x < mx + 0.5*w && x > mx - 0.5*w)
    guard (y < my + 0.5*h && y > my - 0.5*h)
    return effect
