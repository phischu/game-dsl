{-# LANGUAGE StandaloneDeriving,DeriveFunctor #-}
module GameDSL where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map (lookup,keys,insert,empty,delete,adjust)
import Control.Monad.State (State,execState)
import qualified Control.Monad.State as State (get,put,modify)
import Control.Monad.Reader



type Query tag attribute = ReaderT (GameState tag attribute) []

type Action tag attribute = State (GameState tag attribute)

data Element tag attribute = Element {
    elementPicture :: Picture,
    elementAction :: Trigger -> Action tag attribute ()}

element :: Picture -> (Trigger -> Action tag attribute ()) -> Element tag attribute
element = Element

draw :: Picture -> Element tag attribute
draw picture = Element picture (const (return ()))

trigger :: (Trigger -> Action tag attribute ()) -> Element tag attribute
trigger action = Element blank action

clickInRect :: Rect -> Action tag attribute () -> Trigger -> Action tag attribute ()
clickInRect (Rect mx my w h) action (Click x y)
    | x < mx + 0.5*w && x > mx - 0.5*w && y < my + 0.5*h && y > my - 0.5*h = action
    | otherwise = return ()
clickInRect _ _ _ = return ()

tick :: Action tag attribute () -> Trigger -> Action tag attribute ()
tick action Tick = action
tick _ _ = return ()

type Rule tag attribute = Query tag attribute (Element tag attribute)

type Entity = Integer

type Value = Integer

data Trigger =
    Click Float Float |
    Tick

data Rect = Rect Float Float Float Float

deriving instance Show Trigger

instance Show (Element tag attribute) where
    show (Element tr _) = show tr

data GameState tag attribute = GameState {
    newEntity :: Entity,
    entityMap :: Map Entity (Properties tag attribute)}

data Properties tag attribute = Properties {
    propertyTags :: Map tag (),
    propertyAttributes :: Map attribute Value}

deriving instance (Show attribute,Show tag) => Show (Properties tag attribute)

deriving instance (Show attribute,Show tag) => Show (GameState tag attribute)

runQuery :: Query tag attribute a -> GameState tag attribute ->  [a]
runQuery = runReaderT

getEntity :: Query tag attribute Entity
getEntity = do
    entities <- asks entityMap
    for (Map.keys entities)

getTags :: Entity -> Query tag attribute (Map tag ())
getTags entity = do
    entities <- asks entityMap
    properties <- lookupMZ entity entities
    return (propertyTags properties)

getTag :: (Ord tag) => tag -> Entity -> Query tag attribute ()
getTag tag entity = do
    tags <- getTags entity
    lookupMZ tag tags

getAttributes :: (Ord attribute) => Entity -> Query tag attribute (Map attribute Value)
getAttributes entity = do
    entities <- asks entityMap
    properties <- lookupMZ entity entities
    return (propertyAttributes properties)

getAttribute :: (Ord attribute) => attribute -> Entity -> Query tag attribute Value
getAttribute attribute entity = do
    attributes <- getAttributes entity
    lookupMZ attribute attributes

entityTagged :: (Ord tag) => tag -> Query tag attribute Entity
entityTagged tag = getEntity >>= has (getTag tag)

lookupMZ :: (MonadPlus m,Ord k) => k -> Map k v -> m v
lookupMZ k m = maybe mzero return (Map.lookup k m)


for :: [a] -> Query tag attribute a
for = lift

results :: Query tag attribute a -> Query tag attribute [a]
results query = ReaderT (\gamestate -> do
    return (runReaderT query gamestate))

ensure :: Query tag attribute a -> Query tag attribute ()
ensure query = do
    as <- results query
    guard (not (null as))

has :: (a -> Query tag attribute b) -> a -> Query tag attribute a
has query a = do
    ensure (query a)
    return a

ensureNot :: Query tag attribute a -> Query tag attribute ()
ensureNot query = do
    as <- results query
    guard (null as)

hasnt :: (a -> Query tag attribute b) -> a -> Query tag attribute a
hasnt query a = do
    ensureNot (query a)
    return a

runAction :: Action tag attribute a -> GameState tag attribute -> GameState tag attribute
runAction = execState

new :: Action tag attribute Entity
new = do
    (GameState newentity entities) <- State.get
    State.put (GameState (newentity + 1) (Map.insert newentity emptyProperties entities))
    return newentity

emptyProperties :: Properties tag attribute
emptyProperties = Properties Map.empty Map.empty

delete :: Entity -> Action tag attribute ()
delete entity = do
    (GameState newentity entities) <- State.get
    State.put (GameState newentity (Map.delete entity entities))

setTag :: (Ord tag) => tag -> Entity -> Action tag attribute ()
setTag tag entity = do
    State.modify (modifyEntity entity (\(Properties tags attributes) ->
        Properties (Map.insert tag () tags) attributes))

unsetTag :: (Ord tag) => tag -> Entity -> Action tag attribute ()
unsetTag tag entity = do
    State.modify (modifyEntity entity (\(Properties tags attributes) ->
        Properties (Map.delete tag tags) attributes))

setAttribute :: (Ord attribute) => attribute -> Entity -> Value -> Action tag attribute ()
setAttribute attribute entity value = do
    State.modify (modifyEntity entity (\(Properties ts attributes) ->
        Properties ts (Map.insert attribute value attributes)))

unsetAttribute :: (Ord attribute) => attribute -> Entity -> Action tag attribute ()
unsetAttribute attribute entity = do
    State.modify (modifyEntity entity (\(Properties tags attributes) ->
        Properties tags (Map.delete attribute attributes)))

modifyEntity :: Entity -> (Properties tag attribute -> Properties tag attribute) -> GameState tag attribute -> GameState tag attribute
modifyEntity entity f (GameState newentity entities) = GameState newentity (Map.adjust f entity entities)

type RunState tag attribute = ([Element tag attribute],GameState tag attribute)

runGame :: Action tag attribute () -> [Rule tag attribute] -> IO ()
runGame setup rules = play
    (InWindow "Game DSL" (500,500) (200,200))
    white
    40
    (run setup rules emptyGameState)
    (render . fst)
    (handle rules)
    (step rules)

emptyGameState :: GameState tag attribute
emptyGameState = GameState 0 Map.empty

run :: Action tag attribute () -> [Rule tag attribute] -> GameState tag attribute -> ([Element tag attribute],GameState tag attribute)
run action rules gamestate = (elements,gamestate') where
    gamestate' = runAction action gamestate
    elements = concatMap (flip runQuery gamestate') rules

render :: [Element tag attribute] -> Picture
render = pictures . map elementPicture

handle :: [Rule tag attribute] -> Event -> RunState tag attribute -> RunState tag attribute
handle rules (EventKey (MouseButton _) _ _ (x,y)) (elements,gamestate) = run actions rules gamestate where
    actions = forM_ elements (\(Element _ action) -> action (Click x y))
handle _ _ (clickables,gamestate) = (clickables,gamestate)

step :: [Rule tag attribute] -> Float -> RunState tag attribute -> RunState tag attribute
step rules _ (elements,gamestate) = run actions rules gamestate where
    actions = forM_ elements (\(Element _ action) -> action Tick)
