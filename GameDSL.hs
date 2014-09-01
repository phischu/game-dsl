{-# LANGUAGE StandaloneDeriving,DeriveFunctor #-}
module GameDSL where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map (lookup,keys,insert,empty,delete,adjust)
import Data.Set (Set)
import qualified Data.Set as Set (member,empty,delete,insert)
import Control.Monad.State (State,execState)
import qualified Control.Monad.State as State (get,put,modify)
import Control.Monad.Reader



type Query tag attribute = ReaderT (GameState tag attribute) []

type Action tag attribute = State (GameState tag attribute)

data Element tag attribute = Element {
    elementPicture :: Picture,
    elementTrigger :: Trigger,
    elementAction :: Action tag attribute ()}

draw :: Picture -> Element tag attribute
draw picture = Element picture NoTrigger (return ())

trigger :: Trigger -> (Action tag attribute ()) -> Element tag attribute
trigger tr action = Element blank tr action

type Rule tag attribute = Query tag attribute (Element tag attribute)

type Entity = Integer

type Value = Integer

data Trigger =
    NoTrigger |
    ClickableRect Float Float Float Float

deriving instance Show Trigger

instance Show (Element tag attribute) where
    show (Element tr _ _) = show tr

data GameState tag attribute = GameState {
    newEntity :: Entity,
    entityMap :: Map Entity (Properties tag attribute)}

data Properties tag attribute = Properties {
    propertyTags :: Set tag,
    propertyAttributes :: Map attribute Value}

deriving instance (Show attribute,Show tag) => Show (Properties tag attribute)

deriving instance (Show attribute,Show tag) => Show (GameState tag attribute)

runQuery :: Query tag attribute a -> GameState tag attribute ->  [a]
runQuery = runReaderT

getEntity :: Query tag attribute Entity
getEntity = do
    entities <- asks entityMap
    for (Map.keys entities)

getTags :: Entity -> Query tag attribute (Set tag)
getTags entity = do
    entities <- asks entityMap
    properties <- lookupMZ entity entities
    return (propertyTags properties)

getTag :: (Ord tag) => tag -> Entity -> Query tag attribute Bool
getTag t entity = do
    tags <- getTags entity
    return (Set.member t tags)

entityTagged :: (Ord tag) => tag -> Query tag attribute Entity
entityTagged t = do
    entity <- getEntity
    getTag t entity >>= guard
    return entity

getAttributes :: (Ord attribute) => Entity -> Query tag attribute (Map attribute Value)
getAttributes entity = do
    entities <- asks entityMap
    properties <- lookupMZ entity entities
    return (propertyAttributes properties)

getAttribute :: (Ord attribute) => attribute -> Entity -> Query tag attribute Value
getAttribute attribute entity = do
    attributes <- getAttributes entity
    lookupMZ attribute attributes

lookupMZ :: (MonadPlus m,Ord k) => k -> Map k v -> m v
lookupMZ k m = maybe mzero return (Map.lookup k m)


for :: [a] -> Query tag attribute a
for = lift

results :: Query tag attribute a -> Query tag attribute [a]
results query = ReaderT (\gamestate -> do
    return (runReaderT query gamestate))

no :: Query tag attribute a -> Query tag attribute ()
no query = do
    as <- results query
    guard (null as)

runAction :: Action tag attribute a -> GameState tag attribute -> GameState tag attribute
runAction = execState

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

setTag :: (Ord tag) => tag -> Entity -> Bool -> Action tag attribute ()
setTag t entity b = do
    State.modify (modifyEntity entity (\(Properties ts attributes) ->
        Properties ((if b then Set.insert else Set.delete) t ts) attributes))

setAttribute :: (Ord attribute) => attribute -> Entity -> Value -> Action tag attribute ()
setAttribute attribute entity value = do
    State.modify (modifyEntity entity (\(Properties ts attributes) ->
        Properties ts (Map.insert attribute value attributes)))

type RunState tag attribute = ([Element tag attribute],GameState tag attribute)

runGame :: Action tag attribute () -> [Rule tag attribute] -> IO ()
runGame setup rules = play
    (InWindow "Game DSL" (500,500) (200,200))
    white
    40
    (run setup rules emptyGameState)
    (render . fst)
    (handle rules)
    (const id)

emptyGameState :: GameState tag attribute
emptyGameState = GameState 0 Map.empty

run :: Action tag attribute () -> [Rule tag attribute] -> GameState tag attribute -> ([Element tag attribute],GameState tag attribute)
run action rules gamestate = (elements,gamestate') where
    gamestate' = runAction action gamestate
    elements = concatMap (flip runQuery gamestate') rules

render :: [Element tag attribute] -> Picture
render = pictures . map elementPicture

handle :: [Rule tag attribute] -> Event -> RunState tag attribute -> RunState tag attribute
handle actions (EventKey (MouseButton _) _ _ (x,y)) (clickables,gamestate) = case filterInside x y clickables of
    [] -> (clickables,gamestate)
    (effect:_) -> run effect actions gamestate
handle _ _ (clickables,gamestate) = (clickables,gamestate)

filterInside :: Float -> Float -> [Element tag attribute] -> [Action tag attribute ()]
filterInside x y clickables = do
    Element _ (ClickableRect mx my w h) action <- clickables
    guard (x < mx + 0.5*w && x > mx - 0.5*w)
    guard (y < my + 0.5*h && y > my - 0.5*h)
    return action
