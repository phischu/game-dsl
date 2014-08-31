{-# LANGUAGE StandaloneDeriving,DeriveFunctor #-}
module GameDSL where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map (lookup,keys,filter,insert,empty,delete,adjust)
import Data.Set (Set)
import qualified Data.Set as Set (member,empty,delete,insert)
import Control.Monad.State (State,execState)
import qualified Control.Monad.State as State (get,put,modify)
import Control.Monad.Reader

type Entity = Integer

type Query tag attribute = ReaderT (GameState tag attribute) []

type Action tag attribute = State (GameState tag attribute)

data Clickable tag attribute = Clickable Area (Action tag attribute ())

type Rule tag attribute = Query tag attribute (Clickable tag attribute)

type Render tag attribute = Query tag attribute Picture

data GameState tag attribute = GameState {
    newEntity :: Entity,
    entityMap :: Map Entity (Properties tag attribute)}

data Properties tag attribute = Properties {
    propertyTags :: Set tag,
    propertyAttributes :: Map attribute Value}

deriving instance (Show attribute,Show tag) => Show (Properties tag attribute)

type Value = Integer

instance Show (Clickable tag attribute) where
    show (Clickable area _) = show area

data Area = Rect Float Float Float Float

deriving instance Show Area

deriving instance (Show attribute,Show tag) => Show (GameState tag attribute)

runQuery :: Query tag attribute a -> GameState tag attribute ->  [a]
runQuery = runReaderT

for :: [a] -> Query tag attribute a
for = lift

tagged :: (Ord tag) => tag -> Query tag attribute Entity
tagged t = do
    entities <- asks entityMap
    for (Map.keys (Map.filter (Set.member t . propertyTags) entities))

tags :: (Ord tag) => Entity -> Query tag attribute (Set tag)
tags entity = do
    entities <- asks entityMap
    properties <- for (lookupList entity entities)
    return (propertyTags properties)

hasTag :: (Ord tag) => tag -> Entity -> Query tag attribute ()
hasTag t entity = tags entity >>= guard . Set.member t

get :: (Ord attribute) => attribute -> Entity -> Query tag attribute Value
get attribute entity = do
    entities <- asks entityMap
    properties <- for (lookupList entity entities)
    for (lookupList attribute (propertyAttributes properties))

gather :: Query tag attribute a -> Query tag attribute [a]
gather query = ReaderT (\gamestate -> do
    return (runReaderT query gamestate))

no :: Query tag attribute a -> Query tag attribute ()
no query = do
    as <- gather query
    guard (null as)

lookupList :: (Ord k) => k -> Map k v -> [v]
lookupList k m = maybe [] (:[]) (Map.lookup k m)

runAction :: Action tag attribute a -> GameState tag attribute -> GameState tag attribute
runAction = execState
{-
runEffect (Pure a) = return a
runEffect (Free f) = case f of
    New k -> do

    Delete entity k -> 
    Set attribute entity value k -> 
    Untag t entity k -> do
        runEffect k
    Tag t entity k -> do
        runEffect k
-}
modifyEntity :: Entity -> (Properties tag attribute -> Properties tag attribute) -> GameState tag attribute -> GameState tag attribute
modifyEntity entity f (GameState newentity entities) = GameState newentity (Map.adjust f entity entities)


new :: Action tag attribute Entity
new = do
    (GameState newentity entities) <- State.get
    State.put (GameState (newentity + 1) (Map.insert newentity emptyProperties entities))
    return newentity

emptyProperties :: Properties tag attribute
emptyProperties = Properties Set.empty Map.empty

delete :: Entity -> Action tag attribute ()
delete entity = do
    (GameState newentity entities) <- State.get
    State.put (GameState newentity (Map.delete entity entities))

set :: (Ord attribute) => attribute -> Entity -> Value -> Action tag attribute ()
set attribute entity value = do
    State.modify (modifyEntity entity (\(Properties ts attributes) ->
        Properties ts (Map.insert attribute value attributes)))

untag :: (Ord tag) => tag -> Entity -> Action tag attribute ()
untag t entity = do
    State.modify (modifyEntity entity (\(Properties ts attributes) ->
        Properties (Set.delete t ts) attributes))

tag :: (Ord tag) => tag -> Entity -> Action tag attribute ()
tag t entity = do
    State.modify (modifyEntity entity (\(Properties ts attributes) ->
        Properties (Set.insert t ts) attributes))

type RunState tag attribute = ([Clickable tag attribute],GameState tag attribute)

runGame :: Action tag attribute () -> [Rule tag attribute] -> [Render tag attribute] -> IO ()
runGame setup actions renders = play
    (InWindow "Game DSL" (500,500) (200,200))
    white
    40
    (run setup actions emptyGameState)
    (render renders . snd)
    (handle actions)
    (const id)

emptyGameState :: GameState tag attribute
emptyGameState = GameState 0 Map.empty

run :: Action tag attribute () -> [Rule tag attribute] -> GameState tag attribute -> ([Clickable tag attribute],GameState tag attribute)
run effect actions gamestate = (clickables,gamestate') where
    gamestate' = runAction effect gamestate
    clickables = concatMap (flip runQuery gamestate') actions

render :: [Render tag attribute] -> GameState tag attribute -> Picture
render renders gamestate = pictures (concatMap (flip runQuery gamestate) renders)

handle :: [Rule tag attribute] -> Event -> RunState tag attribute -> RunState tag attribute
handle actions (EventKey (MouseButton _) _ _ (x,y)) (clickables,gamestate) = case filterInside x y clickables of
    [] -> (clickables,gamestate)
    (effect:_) -> run effect actions gamestate
handle _ _ (clickables,gamestate) = (clickables,gamestate)

filterInside :: Float -> Float -> [Clickable tag attribute] -> [Action tag attribute ()]
filterInside x y clickables = do
    Clickable (Rect mx my w h) effect <- clickables
    guard (x < mx + 0.5*w && x > mx - 0.5*w)
    guard (y < my + 0.5*h && y > my - 0.5*h)
    return effect
