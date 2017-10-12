{-# LANGUAGE    ExistentialQuantification    #-}
{-# LANGUAGE    DeriveFunctor                #-}
{-# LANGUAGE    TemplateHaskell              #-}
{-# LANGUAGE    TypeOperators                #-}
{-# LANGUAGE    FlexibleContexts             #-}
{-# LANGUAGE    RankNTypes                   #-}
{-# LANGUAGE    MultiParamTypeClasses        #-}
{-# LANGUAGE    TypeSynonymInstances         #-}
{-# LANGUAGE    FlexibleInstances            #-}

module TextAd.Model.Dsl where

import BasicPrelude
import Control.Monad.Free     (Free, liftF, MonadFree)

import TextAd.Model.Core
import TextAd.Util.CoProduct  ((:<:), inj)

--------------------------------------------------------------------------------

-- | Add a title to Story.
setSTitle :: (SetSTitleF :<: f) => Text -> Free f ()
setSTitle title = liftF . inj $ SetSTitle title ()

-- | Define player
mkPlayer :: (MkPlayerF :<: f)
         => [Oid]
         -- ^ @object ref@s to be added to player's inventory.
         -> Rid
         -- ^ @room ref@ of player's initial location
         -> Free f ()
mkPlayer inventory location = liftF . inj $ MkPlayer inventory location ()

-- | Define a room.
-- e.g.
--
-- > mkRoom roomId $ do
-- >   setRTitle "Room"
mkRoom :: (MkRoomF :<: f)
       => Rid
       -- ^ @room ref@ of room to be defined
       -> RoomBuilder ()
       -- ^ the room definition
       -> Free f ()
mkRoom rid rb = liftF . inj $ MkRoom rid rb ()

-- | Define an object.
-- e.g.
--
-- > mkObject objId $ do
-- >    setOTitle "Object"
mkObject :: (MkObjectF :<: f)
         => Oid
         -- ^ @object ref@ of object to be defined
         -> ObjectBuilder ()
         -- ^ the object definition
         -> Free f ()
mkObject oid obj  = liftF . inj $ MkObject oid obj ()

-- | Introduce state with a label and initial value.
-- e.g.
--
-- > stateRef <- mkState "stateLabel" False
mkState :: (MkStateF :<: f, Typeable a) => Text -> a -> Free f (Sid a)
mkState l val = liftF . inj $ MkState l val id

-- | Add some initial action to be run when starting a new game. Expected to be
-- used for displaying text with 'printLn'
-- e.g.
--
-- > setSInit $ do
-- >   printLn "Welcome to game"
setSInit :: (SetSInitF :<: f) => Action () -> Free f ()
setSInit atn = liftF . inj $ SetSInit (Just atn) ()

setMaxScore :: (SetMaxScoreF :<: f) => Int -> Free f ()
setMaxScore i = liftF . inj $ SetMaxScore i ()


--------------------------------------------------------------------------------


-- | Add title to room.
setRTitle :: (SetRTitleF :<: f) => Text -> Free f ()
setRTitle title = liftF . inj $ SetRTitle title ()

-- | Add description to room. Will be displayed each time room is entered.
-- It takes an action for dynamic text (e.g. checking state first)
-- Note, it should not be used for side-effecting actions, since will be run multiple times.
setRDescr :: (SetRDescrF :<: f) => Action () -> Free f ()
setRDescr atn = liftF . inj $ SetRDescr atn ()

-- | Add exits to room
setRExits :: (SetRExitsF :<: f) => ExitsBuilder () -> Free f ()
setRExits atn = liftF . inj $ SetRExits atn ()

-- | Add an item to room.
setRItems :: (SetRItemsF :<: f) => [Oid] -> Free f ()
setRItems oids = liftF . inj $ SetRItems oids ()



--------------------------------------------------------------------------------


-- | Add a title to object.
setOTitle :: (SetOTitleF :<: f) => Text -> Free f ()
setOTitle title = liftF . inj $ SetOTitle title ()

-- | Define the object noun type - for textual conjugations.
setONounType :: (SetONounTypeF :<: f) => NounType -> Free f ()
setONounType nounType = liftF . inj $ SetONounType nounType ()

-- | Define the object noun type - for textual conjugations.
setOIsPlural :: (SetOIsPluralF :<: f) => Bool -> Free f ()
setOIsPlural isPlural = liftF . inj $ SetOIsPlural isPlural ()

-- | Add a description to object.
setODescr :: (SetODescrF :<: f) => Action () -> Free f ()
setODescr atn = liftF . inj $ SetODescr atn ()

-- | Set whether the player can pick the object up.
setOCanPickUp :: (SetOCanPickUpF :<: f) => Bool -> Free f ()
setOCanPickUp canPickUp = liftF . inj $ SetOCanPickUp canPickUp ()

class SetUse m where
  toSetUse :: m -> Either (Action ()) (UseAction ())

instance SetUse (Action ()) where
  toSetUse = Left

instance SetUse (UseAction ()) where
  toSetUse = Right


-- | Define the behaviour for using the object with another.
setOUse :: (SetOUseF :<: f, SetUse su)
        => su
        -> Free f ()
setOUse useAction = liftF . inj $ SetOUse (toSetUse useAction) ()

-- | Define the talk behaviour. Primarily for use with 'say', but accepts any
-- actions to permit checking state first.
-- e.g.
--
-- > talkO $ do
-- >   say "Hello" $ do
-- >       printLn "You say hello. There is no response"
-- >   state <- getState stateRef
-- >   when state $ do
-- >     say "state was true" $ return ()
talkO :: (SetOTalkF :<: f) => Action () -> Free f ()
talkO atn = liftF . inj $ SetOTalk atn ()

--------------------------------------------------------------------------------

-- | Display text.
printLn :: (PrintLnF :<: f) => Text -> Free f ()
printLn txt = liftF . inj $ PrintLn txt ()

-- | Add item to room.
addItem :: (AddItemF :<: f) => Rid -> Oid -> Free f ()
addItem rid oid = liftF . inj $ AddItem rid oid ()

-- | Add item to player's inventory.
takeItem :: (TakeItemF :<: f) => Oid -> Free f ()
takeItem oid = liftF . inj$ TakeItem oid ()

-- | Remove item from it's location .
-- Location can be a room, or in player's inventory.
destroyItem :: (DestroyItemF :<: f) => Oid -> Free f ()
destroyItem oid = liftF . inj $ DestroyItem oid ()

class RidAction m where
  toAction :: m -> Action (Maybe Rid)

instance RidAction (Action (Maybe Rid)) where
  toAction = id

instance RidAction Rid where
  toAction = return . Just

-- | Add an exit to room.
exit :: (MonadFree f m, AddExitF :<: f, RidAction ra)
     => Text
     -> DirHint
     -> ra
     -> m ()
-- exit :: (MonadFree f m, AddExitF :<: f) =>-} Text -> DirHint -> Rid -> Free (AddExitF :+: SetStateF) ()
-- exit :: (AddExitF :<: f) => Text -> DirHint -> Rid -> Free f ()
exit l dh ridAction = liftF . inj $ AddExit (Exit l dh (toAction ridAction)) ()

-- | Type helper for use with exit, to avoid ambiguous types
action :: Action r -> Action r
action = id

-- | Increase the game progress.
incScore :: (IncScoreF :<: f) => Int -> Free f ()
incScore i = liftF . inj $ IncScore i ()

-- | Add a say option when talking. Also add the behaviour to be invoked on
-- selection of the option. This may lead to more dialogue.
say :: (SayF :<: f) => Text -> Action () -> Free f ()
say s atn = liftF . inj $ Say s atn ()

-- | Get the current state value.
getState :: ((GetStateF :<: f), Typeable a) => Sid a -> Free f a
getState sid = liftF . inj $ GetState sid id

-- | Set the state value.
setState :: ((SetStateF :<: f), Typeable a) => Sid a -> a -> Free f ()
setState sid val = liftF . inj $ SetState sid val ()

-- | Does the player have the object?
playerHas :: (PlayerHasF :<: f) => Oid -> Free f Bool
playerHas oid = liftF . inj $ PlayerHas oid id

-- | Is the object in the room?
roomHas :: (RoomHasF :<: f) => Rid -> Oid -> Free f Bool
roomHas rid oid = liftF . inj $ RoomHas rid oid id

-- | Which room has the object?
-- If more than one room has the object, then it is not defined which will be
-- returned.
roomOf :: (MonadFree f m, (RoomOfF :<: f)) => Oid -> m (Maybe Rid)
roomOf oid = liftF . inj $ RoomOf oid id

-- | Which room has the object?
-- If more than one room has the object, then it is not defined which will be
-- returned.
currentRoom :: (MonadFree f m, (CurrentRoomF :<: f)) => m Rid
currentRoom = liftF . inj $ CurrentRoom id


-- | Define the behaviour for using the object with another.
-- Not including the flexible (WithF) constraint, and opted for fixing to the UseAction
-- to help with type-inferrence of SetUse (Which can take either UseAction or Action)
--with :: (WithF :<: f) => Oid -> Action () -> Free f ()
with :: Oid -> Action () -> UseAction ()
with oid atn = liftF . inj $ With oid atn ()
