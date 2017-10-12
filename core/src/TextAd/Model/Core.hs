{-# LANGUAGE    ExistentialQuantification    #-}
{-# LANGUAGE    DeriveFunctor                #-}
{-# LANGUAGE    TemplateHaskell              #-}
{-# LANGUAGE    TypeOperators                #-}
{-# LANGUAGE    FlexibleContexts             #-}
{-# LANGUAGE    RankNTypes                   #-}
{-# LANGUAGE    MultiParamTypeClasses        #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-} -- caused by lens generation

module TextAd.Model.Core where

import BasicPrelude
import Data.Dynamic              (Dynamic)
import Control.Monad.Free        (Free)
import Lens.Family.TH            (makeLenses)
import qualified Data.Text       as T
import qualified Data.Map        as M

import TextAd.Util.CoProduct     ((:+:), (:*:))
import TextAd.Util.Pairing       (Pairing, pair)

error' = error . T.unpack

show' :: Show a => a -> Text
show' = T.pack . show

newtype Rid   = Rid Text deriving (Eq, Show, Ord)
newtype Oid   = Oid Text deriving (Eq, Show, Ord)
newtype Sid a = Sid {sidK :: Text} deriving (Eq, Show)

data Story = Story
  { _sTitle    :: Text
  , _sPlayer   :: Player
  , _sRooms    :: M.Map Rid Room
  , _sObjects  :: M.Map Oid Object
  , _sStates   :: M.Map Text Dynamic
  , _sScore    :: Int
  , _sMaxScore :: Maybe Int
  , _sSay      :: [(Text, Action ())]
  , _sInit     :: Maybe (Action ())
  }

data Player = Player
  { _pInventory :: [Oid]
  , _pLocation  :: Rid
  }

data DirHint = N | W | E | S | U
  deriving (Eq, Ord)


data Exit = Exit
  { _eLabel   :: Text
  , _eDirHint :: DirHint
  , _eRid     :: Action (Maybe Rid)
  }

instance Eq Exit where
  e1 == e2 = _eLabel e1 == _eLabel e2

instance Ord Exit where
  compare e1 e2 = compare (_eDirHint e1) (_eDirHint e2)

data Room = Room
  { _rTitle        :: Text
  , _rDescr        :: Action () -- TODO can we ensure at least one String has been printed?
  , _rItems        :: [Oid]
  , _rExitsBuilder :: ExitsBuilder ()
  }

data NounType = Proper
              | Quantitive -- "some"
              | Particular -- "the" , "a/an"

data Object = Object
  { _oTitle      :: Text
  , _oNounType   :: NounType
  , _oIsPlural   :: Bool
  , _oDescr      :: Action ()
  , _oCanPickUp  :: Bool                              -- we could set this to True automatically when usewith is used - however, there are some object to be picked up where useWith is not required.
  , _oUse        :: Either (Action ()) (UseAction ()) -- only applies if oCanPickUp = True (TODO enforce envariant during compilation)
  , _oTalk       :: Maybe (Action ())
  }


--------------------------------------------------------------------------------

data SetSTitleF   k =                         SetSTitle   Text                               k  deriving (Functor)
data MkPlayerF    k =                         MkPlayer    [Oid] Rid                          k  deriving (Functor) -- TODO can we enforce set once-and-only-once?
data MkObjectF    k =                         MkObject    Oid (ObjectBuilder ())             k  deriving (Functor)
data MkRoomF      k =                         MkRoom      Rid (RoomBuilder ())               k  deriving (Functor)
data MkStateF     k = forall a. Typeable a => MkState     Text a                 ((Sid a) -> k)
data SetSInitF    k =                         SetSInit    (Maybe (Action ()))                k  deriving (Functor)
data SetMaxScoreF k =                         SetMaxScore Int                                k  deriving (Functor)

-- we can't derive Functor with existential quantification
instance Functor MkStateF where
  fmap f (MkState l val k) = MkState l val (f . k)

type StoryBuilderSyntax = SetSTitleF :+: MkPlayerF :+: MkObjectF :+: MkRoomF :+: MkStateF :+: SetSInitF :+: SetMaxScoreF
type StoryBuilder = Free StoryBuilderSyntax
type StoryDef = StoryBuilder ()

data CoSetSTitleF   k = CoSetSTitle   (                        Text                    ->         k ) deriving (Functor)
data CoMkPlayerF    k = CoMkPlayer    (                        [Oid] -> Rid            ->         k ) deriving (Functor)
data CoMkObjectF    k = CoMkObject    (                        Oid -> ObjectBuilder () ->         k ) deriving (Functor)
data CoMkRoomF      k = CoMkRoom      (                        Rid -> RoomBuilder ()   ->         k ) deriving (Functor)
data CoMkStateF     k = CoMkState     (forall a. Typeable a => Text -> a               -> (Sid a, k))
data CoSetSInitF    k = CoSetSInit    (                        Maybe (Action ())       ->         k ) deriving (Functor)
data CoSetMaxScoreF k = CoSetMaxScore (                        Int                     ->         k ) deriving (Functor)

instance Functor CoMkStateF where
  fmap f (CoMkState k) = CoMkState (\t a -> let (s, k2) = k t a in (s, f k2))

type CoStoryBuilderSyntax = CoSetSTitleF :*: CoMkPlayerF :*: CoMkObjectF :*: CoMkRoomF :*: CoMkStateF :*: CoSetSInitF :*: CoSetMaxScoreF


instance Pairing CoSetSTitleF SetSTitleF where
  pair f (CoSetSTitle g) (SetSTitle t k) =
    f (g t) k

instance Pairing CoMkPlayerF MkPlayerF where
  pair f (CoMkPlayer g) (MkPlayer os r k) =
    f (g os r) k

instance Pairing CoMkObjectF MkObjectF where
  pair f (CoMkObject g) (MkObject o ob k) =
    f (g o ob) k

instance Pairing CoMkRoomF MkRoomF where
  pair f (CoMkRoom g) (MkRoom r rb k) =
    f (g r rb) k

instance Pairing CoMkStateF MkStateF where
  pair f (CoMkState g) (MkState t a k) =
    pair f (g t a) k

instance Pairing CoSetSInitF SetSInitF where
  pair f (CoSetSInit g) (SetSInit ma k) =
    f (g ma) k

instance Pairing CoSetMaxScoreF SetMaxScoreF where
  pair f (CoSetMaxScore g) (SetMaxScore i k) =
    f (g i) k


--------------------------------------------------------------------------------

data SetRTitleF k = SetRTitle Text              k  deriving (Functor)
data SetRDescrF k = SetRDescr (Action ())       k  deriving (Functor)
data SetRExitsF k = SetRExits (ExitsBuilder ()) k  deriving (Functor)
data SetRItemsF k = SetRItems [Oid]             k  deriving (Functor)

type RoomBuilderSyntax = SetRTitleF :+: SetRDescrF :+: SetRExitsF :+: SetRItemsF
type RoomBuilder = Free RoomBuilderSyntax

data CoSetRTitleF k = CoSetRTitle (Text            -> k) deriving (Functor)
data CoSetRDescrF k = CoSetRDescr (Action ()       -> k) deriving (Functor)
data CoSetRExitsF k = CoSetRExits (ExitsBuilder () -> k) deriving (Functor)
data CoSetRItemsF k = CoSetRItems ([Oid]           -> k) deriving (Functor)

type CoRoomBuilderSyntax = CoSetRTitleF :*: CoSetRDescrF :*: CoSetRExitsF :*: CoSetRItemsF


instance Pairing CoSetRTitleF SetRTitleF where
  pair f (CoSetRTitle g) (SetRTitle t k) =
    f (g t) k

instance Pairing CoSetRDescrF SetRDescrF where
  pair f (CoSetRDescr g) (SetRDescr a k) =
    f (g a) k

instance Pairing CoSetRExitsF SetRExitsF where
  pair f (CoSetRExits g) (SetRExits ea k) =
    f (g ea) k

instance Pairing CoSetRItemsF SetRItemsF where
  pair f (CoSetRItems g) (SetRItems os k) =
    f (g os) k

--------------------------------------------------------------------------------

data SetOTitleF     k = SetOTitle     Text           k  deriving (Functor)
data SetONounTypeF  k = SetONounType  NounType       k  deriving (Functor)
data SetOIsPluralF  k = SetOIsPlural  Bool           k  deriving (Functor)
data SetODescrF     k = SetODescr     (Action ())    k  deriving (Functor)
data SetOCanPickUpF k = SetOCanPickUp Bool           k  deriving (Functor)
data SetOUseF       k = SetOUse       (Either (Action ()) (UseAction ()) ) k  deriving (Functor)
data SetOTalkF      k = SetOTalk      (Action ())    k  deriving (Functor)

type ObjectBuilderSyntax = SetOTitleF :+: SetONounTypeF :+: SetOIsPluralF :+: SetODescrF :+: SetOCanPickUpF :+: SetOUseF :+: SetOTalkF
type ObjectBuilder = Free ObjectBuilderSyntax

data CoSetOTitleF     k = CoSetOTitle     (Text         -> k) deriving (Functor)
data CoSetONounTypeF  k = CoSetONounType  (NounType     -> k) deriving (Functor)
data CoSetOIsPluralF  k = CoSetOIsPlural  (Bool         -> k) deriving (Functor)
data CoSetODescrF     k = CoSetODescr     (Action ()    -> k) deriving (Functor)
data CoSetOCanPickUpF k = CoSetOCanPickUp (Bool         -> k) deriving (Functor)
data CoSetOUseF       k = CoSetOUse       (Either (Action ()) (UseAction ()) -> k) deriving (Functor)
data CoSetOTalkF      k = CoSetOTalk      (Action ()    -> k) deriving (Functor)

type CoObjectBuilderSyntax = CoSetOTitleF :*: CoSetONounTypeF :*: CoSetOIsPluralF :*: CoSetODescrF :*: CoSetOCanPickUpF :*: CoSetOUseF :*: CoSetOTalkF


instance Pairing CoSetOTitleF SetOTitleF where
  pair f (CoSetOTitle g) (SetOTitle t k) =
    f (g t) k

instance Pairing CoSetONounTypeF SetONounTypeF where
  pair f (CoSetONounType g) (SetONounType nt k) =
    f (g nt) k

instance Pairing CoSetOIsPluralF SetOIsPluralF where
  pair f (CoSetOIsPlural g) (SetOIsPlural b k) =
    f (g b) k

instance Pairing CoSetODescrF SetODescrF where
  pair f (CoSetODescr g) (SetODescr a k) =
    f (g a) k

instance Pairing CoSetOCanPickUpF SetOCanPickUpF where
  pair f (CoSetOCanPickUp g) (SetOCanPickUp b k) =
    f (g b) k

instance Pairing CoSetOUseF SetOUseF where
  pair f (CoSetOUse g) (SetOUse ua k) =
    f (g ua) k

instance Pairing CoSetOTalkF SetOTalkF where
  pair f (CoSetOTalk g) (SetOTalk a k) =
    f (g a) k

--------------------------------------------------------------------------------

data GetStateF k = forall a. Typeable a => GetState (Sid a)   (a -> k)
data SetStateF k = forall a. Typeable a => SetState (Sid a) a       k

type StateSyntax = GetStateF :+: SetStateF

instance Functor GetStateF where
  fmap f (GetState    sid       next) = GetState    sid       (f . next)

instance Functor SetStateF where
  fmap f (SetState    sid val   next) = SetState    sid val   (f next)

data CoGetStateF        k = CoGetState (forall a. Typeable a => Sid a      -> (a, k))
data CoSetStateF        k = CoSetState (forall a. Typeable a => Sid a -> a ->     k )

instance Functor CoGetStateF where
  fmap f (CoGetState k) = CoGetState (\s -> let (v, k2) = k s in (v, f k2))

instance Functor CoSetStateF where
  fmap f (CoSetState k) = CoSetState (\s a -> let k2 = k s a in f k2)

type CoStateSyntax = CoGetStateF :*: CoSetStateF

instance Pairing CoGetStateF GetStateF where
  pair f (CoGetState g) (GetState s k) =
    pair f (g s) k
    -- let (a, k2) = g s
    -- in f k2 (k a)

instance Pairing CoSetStateF SetStateF where
  pair f (CoSetState g) (SetState s a k) =
    f (g s a) k

--------------------------------------------------------------------------------


data PrintLnF     k = PrintLn     Text                           k  deriving (Functor)
data AddItemF     k = AddItem     Rid Oid                        k  deriving (Functor)
data TakeItemF    k = TakeItem    Oid                            k  deriving (Functor)
data DestroyItemF k = DestroyItem Oid                            k  deriving (Functor)
data IncScoreF    k = IncScore    Int                            k  deriving (Functor)
data SayF         k = Say         Text (Action ())               k  deriving (Functor)
data PlayerHasF   k = PlayerHas   Oid              (Bool      -> k) deriving (Functor)
data RoomHasF     k = RoomHas     Rid Oid          (Bool      -> k) deriving (Functor)
data RoomOfF      k = RoomOf      Oid              (Maybe Rid -> k) deriving (Functor) -- assumes oid only in one room...
data CurrentRoomF k = CurrentRoom                  (Rid       -> k) deriving (Functor)

type ActionSyntax = PrintLnF :+: AddItemF :+: TakeItemF :+: DestroyItemF :+: IncScoreF :+: SayF :+: PlayerHasF :+: RoomHasF :+: RoomOfF :+: CurrentRoomF :+: GetStateF :+: SetStateF
type Action = Free ActionSyntax

data CoPrintLnF     k = CoPrintLn     (Text              ->             k ) deriving (Functor)
data CoAddItemF     k = CoAddItem     (Rid -> Oid        ->             k ) deriving (Functor)
data CoTakeItemF    k = CoTakeItem    (Oid               ->             k ) deriving (Functor)
data CoDestroyItemF k = CoDestroyItem (Oid               ->             k ) deriving (Functor)
data CoIncScoreF    k = CoIncScore    (Int               ->             k ) deriving (Functor)
data CoSayF         k = CoSay         (Text -> Action () ->             k ) deriving (Functor)
data CoPlayerHasF   k = CoPlayerHas   (Oid               -> (Bool,      k)) deriving (Functor)
data CoRoomHasF     k = CoRoomHas     (Rid -> Oid        -> (Bool,      k)) deriving (Functor)
data CoRoomOfF      k = CoRoomOf      (Oid               -> (Maybe Rid, k)) deriving (Functor)
data CoCurrentRoomF k = CoCurrentRoom (                     (Rid,       k)) deriving (Functor)

type CoActionSyntax = CoPrintLnF :*: CoAddItemF :*: CoTakeItemF :*: CoDestroyItemF :*: CoIncScoreF :*: CoSayF :*: CoPlayerHasF :*: CoRoomHasF :*: CoRoomOfF :*: CoCurrentRoomF :*: CoGetStateF :*: CoSetStateF


instance Pairing CoPrintLnF PrintLnF where
  pair f (CoPrintLn g) (PrintLn t k) =
    f (g t) k

instance Pairing CoAddItemF AddItemF where
  pair f (CoAddItem g) (AddItem r o k) =
    f (g r o) k

instance Pairing CoTakeItemF TakeItemF where
  pair f (CoTakeItem g) (TakeItem o k) =
    f (g o) k

instance Pairing CoDestroyItemF DestroyItemF where
  pair f (CoDestroyItem g) (DestroyItem o k) =
    f (g o) k

instance Pairing CoIncScoreF IncScoreF where
  pair f (CoIncScore g) (IncScore i k) =
    f (g i) k

instance Pairing CoSayF SayF where
  pair f (CoSay g) (Say s a k) =
    f (g s a) k

instance Pairing CoPlayerHasF PlayerHasF where
  pair f (CoPlayerHas g) (PlayerHas o k) =
    pair f (g o) k
    -- let (a, k2) = g o
    -- in f k2 (k a)

instance Pairing CoRoomHasF RoomHasF where
  pair f (CoRoomHas g) (RoomHas r o k) =
    pair f (g r o) k
    -- let (a, k2) = g r o
    -- in f k2 (k a)

instance Pairing CoRoomOfF RoomOfF where
  pair f (CoRoomOf g) (RoomOf o k) =
    pair f (g o) k
    -- let (a, k2) = g o
    -- in f k2 (k a)

instance Pairing CoCurrentRoomF CurrentRoomF where
  pair f (CoCurrentRoom g) (CurrentRoom k) =
    pair f g k


--------------------------------------------------------------------------------



data AddExitF k = AddExit Exit k deriving (Functor)

type ExitsBuilder = Free (AddExitF :+: GetStateF)

data CoAddExitF k = CoAddExit (Exit -> k) deriving (Functor)

instance Pairing CoAddExitF AddExitF where
  pair f (CoAddExit g) (AddExit e k) =
    f (g e) k


--------------------------------------------------------------------------------



data WithF k = With Oid (Action ()) k deriving (Functor)

type UseAction = Free (WithF :+: GetStateF)

data CoWithF k = CoWith (Oid -> Action () -> k) deriving (Functor)

instance Pairing CoWithF WithF where
  pair f (CoWith g) (With o a k) =
    f (g o a) k


--------------------------------------------------------------------------------

$(makeLenses ''Story)
$(makeLenses ''Player)
$(makeLenses ''Room)
$(makeLenses ''Object)
