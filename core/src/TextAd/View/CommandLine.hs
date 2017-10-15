module TextAd.View.CommandLine
  ( runCommandLine
  ) where


import BasicPrelude              hiding (putStrLn)
import Control.Monad.Morph       (hoist, generalize)
import Control.Monad.Trans.State (get, modify, evalState, evalStateT, execStateT, State, StateT)
import Data.Either               (isLeft)
import Lens.Family               ((^.), (.~))
import qualified BasicPrelude    as BP

import TextAd.Interpreter.Action
import TextAd.Interpreter.StoryBuilder
import TextAd.Model.Core
import TextAd.Model.Util

type SS a = StateT Story IO a

hoistG :: State Story a -> StateT Story IO a
hoistG = hoist generalize

toObject2 :: Story -> Oid -> Object
toObject2 story oid = evalState (toObject oid) story

toRoom2 :: Story -> Rid -> Room
toRoom2 story rid = evalState (toRoom rid) story

-- | Runs the app as a command-line app
runCommandLine :: StoryBuilder () -> IO ()
runCommandLine storyDef = do
  let story = toStory storyDef
      title = story ^. sTitle
      rId = story ^. sPlayer ^. pLocation
  putStrLn "----------"
  putStrLn title
  putStrLn "----------"

  story' <- execStateT (displayRoom rId) story
  evalStateT loop story'

putStrLn :: MonadIO m => Text -> m ()
putStrLn = liftIO . BP.putStrLn

putStrLns :: MonadIO m => [Text] -> m ()
putStrLns = putStrLn . intercalate "\n"

accessibleItems :: SS [Oid]
accessibleItems = do
  story <- get
  let player = story ^. sPlayer
      room   = toRoom2 story (player ^. pLocation)
  return $  player ^. pInventory
         <> room ^. rItems


loop :: SS ()
loop = do
  story <- get
  room  <- hoistG $ toRoom $ story ^. sPlayer ^. pLocation
  exits <- hoistG $ listExits room
  displayInRoom room
  let roomItems     =  room ^. rItems
      itemsToPickUp =  filter (_oCanPickUp . toObject2 story) roomItems
      inventory     = story ^. (sPlayer . pInventory)
      itemsToUse    =  {-inventory
                    <>-} filter (isLeft . _oUse . toObject2 story) roomItems
      toTalkTo      =  [(t, atn) | (t, Just atn) <- (((^. oTitle) &&& (^. oTalk)) . toObject2 story) <$> roomItems]
  selectOption $
       (if null inventory     then [] else [("Show inventory", onSelectInventory inventory    )])
    <> (if null itemsToPickUp then [] else [("Take an item"  , onSelectTake      itemsToPickUp)])
    <> (if null itemsToUse    then [] else [("Use"           , onSelectUse       itemsToUse   )])
    <> (if null roomItems     then [] else [("Examine"       , onSelectExamine   roomItems    )])
    <> (if null exits         then [] else (\(Exit l _ rAction) -> ("Go "      <> l, gotoS rAction)) <$> exits   )
    <> (if null toTalkTo      then [] else (\(t, atn)           -> ("Talk to " <> t, sayAction atn)) <$> toTalkTo)
  loop


displayRoom :: Rid -> SS ()
displayRoom rid = do
  room <- hoistG $ toRoom rid
  putStrLn $ "\n" <> room ^. rTitle <> "\n"
  descr <- hoistG $ runAction $ room ^. rDescr
  putStrLns descr

displayInRoom :: Room -> SS ()
displayInRoom room = do
  items <- hoistG $ listItems room
  exits <- hoistG $ listExits room
  when (not $ null $ room ^. rItems) $
    putStrLn $ "\n" <> items
  putStrLn ""
  putStrLn $ "The following exits are visible: " <> intercalate ", " (_eLabel <$> exits) <> "."


selectOption :: [(Text, SS ())] -> SS ()
selectOption options = do
    putStrLn ""
    putStrLns (toString <$> os)
    putStrLn "Enter number:"
    s <- liftIO $ getLine
    case find (\(Option i _ _) -> i == s) os of
      Just (Option _ _ atn) -> atn
      Nothing               -> selectOption options
  where
    toString (Option i l _) = i <> ") " <> l
    os = zipWith (\(l, atn) i -> Option (tshow i) l atn) options [(1 :: Int)..] -- explicit type to avoid warning

gotoS :: Action (Maybe Rid) -> SS ()
gotoS roomAction = do
  eRid <- hoistG $ goto roomAction
  either putStrLns displayRoom eRid

onSelectInventory :: [Oid] -> SS ()
onSelectInventory inventory = do
  story <- get
  selectOption $ ((^. oTitle) . toObject2 story &&& onSelectInventoryO) <$> inventory

onSelectInventoryO :: Oid -> SS ()
onSelectInventoryO oid = do
    story <- get
    selectOption
      [ ("Examine " <> toObject2 story oid ^. oTitle                , onSelectExamineO oid)
      , ("Use "     <> toObject2 story oid ^. oTitle <> suffix story, onSelectUseO     oid)
      ]
  where
    suffix story =
      case toObject2 story oid ^. oUse of
        Left  _ -> ""
        Right _ -> " with "

onSelectTake :: [Oid] -> SS ()
onSelectTake items = do
  story <- get
  selectOption $ (("Take " <>) . (^. oTitle) . toObject2 story &&& onSelectTakeO) <$> items

onSelectTakeO :: Oid -> SS ()
onSelectTakeO oid = do
  o <- hoistG $ toObject oid
  putStrLn $ "You take " <> the o <> "."
  hoistG $ takeItemS oid

onSelectExamine :: [Oid] -> SS ()
onSelectExamine items = do
  story <- get
  selectOption $ ((^. oTitle) . toObject2 story &&& onSelectExamineO) <$> items

onSelectExamineO :: Oid -> SS ()
onSelectExamineO oid = do
  o <- hoistG $ toObject oid
  descr <- hoistG $ runAction $ o ^. oDescr
  putStrLn $ "You examine " <> the o <> "."
  putStrLn $ " " <> intercalate "\n" descr

onSelectUse :: [Oid] -> SS ()
onSelectUse items = do
  story <- get
  selectOption $ ((^. oTitle) . toObject2 story &&& onSelectUseO) <$> items

onSelectUseO :: Oid -> SS ()
onSelectUseO oid = do
  o <- hoistG $ toObject oid
  case o ^. oUse of
    Left  _ -> onSelectUseItselfO oid
    Right _ -> accessibleItems >>= onSelectUseWith oid . (filter (/= oid))

onSelectUseWith :: Oid -> [Oid] -> SS ()
onSelectUseWith oid1 items = do
  story <- get
  selectOption $ (((^. oTitle)) . toObject2 story &&& onSelectUseWithO oid1) <$> items

onSelectUseWithO :: Oid -> Oid -> SS ()
onSelectUseWithO oid1 oid2 = do
  o1 <- hoistG $ toObject oid1
  o2 <- hoistG $ toObject oid2
  putStr $ "You use " <> the o1 <> " with " <> the o2 <> ". "
  txt <- hoistG $ use oid1 (Just oid2)
  if null txt
    then putStrLn "Nothing happens."
    else putStrLns txt

onSelectUseItselfO :: Oid -> SS ()
onSelectUseItselfO oid = do
  o <- hoistG $ toObject oid
  putStr $ "You use " <> the o <> ". "
  txt <- hoistG $ use oid Nothing
  if null txt
    then putStrLn "Nothing happens."
    else putStrLns txt

sayAction :: Action () -> SS ()
sayAction action = do
  modify $ sSay .~ []
  txt   <- hoistG $ runAction action
  putStrLns txt
  story <- get
  let nextSayOptions = story ^. sSay
  if not $ null nextSayOptions
    then selectSayOptions nextSayOptions
    else return ()

selectSayOptions ::  [(Text, Action ())] -> SS ()
selectSayOptions sayOptions =
  selectOption $ (\(s, action) -> ("Say \"" <> s <> "\"", sayAction action)) <$> sayOptions



data Option = Option Text Text (SS ())
