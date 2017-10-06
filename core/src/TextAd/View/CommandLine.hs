module TextAd.View.CommandLine
  ( runCommandLine
  ) where


import BasicPrelude
import Control.Monad.Morph       (hoist, generalize)
import Control.Monad.Trans.State (get, modify, evalState, evalStateT, execStateT, State, StateT)
import Lens.Family               ((^.), (.~))
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
  liftIO $ putStrLn "----------"
  liftIO $ putStrLn title
  liftIO $ putStrLn "----------"

  -- TODO run sInit
  -- TODO support history saving/restoring (in a file)

  story' <- execStateT (displayRoom rId) story
  evalStateT loop story'

loop :: SS ()
loop = do
  story <- get
  let rId = story ^. sPlayer ^. pLocation
  displayInRoom rId
  room  <- hoistG $ toRoom $ story ^. sPlayer ^. pLocation
  exits <- hoistG $ listExits room
  let roomItems     = room ^. rItems
      itemsToPickUp = filter (_oCanPickUp . toObject2 story) roomItems
      toTalkTo      = [(t, atn) | (t, Just atn) <- (((^. oTitle) &&& (^. oTalk)) . toObject2 story) <$> roomItems]
      options       =  [("Show inventory", showInventory)]
                    <> (if null itemsToPickUp then [] else [("Take an item",   takeItemI itemsToPickUp)])
                    <> (if null roomItems     then [] else [("Examine",        examineI roomItems)])
                    <> (map (\(Exit l _ rAction) -> ("Go " <> l, goto' rAction)) $ exits)
                    <> (if null toTalkTo      then [] else (\(t, atn) -> ("Talk to " <> t, sayAction atn)) <$> toTalkTo)
  doOption options
  loop


putStrLns :: [Text] -> IO ()
putStrLns = putStrLn . intercalate "\n"

displayRoom :: Rid -> SS ()
displayRoom rid = do
  room <- hoistG $ toRoom rid
  liftIO $ putStrLn ""
  liftIO $ putStrLn $ room ^. rTitle
  liftIO $ putStrLn ""
  descr <- hoistG $ runAction $ room ^. rDescr
  liftIO $ putStrLns descr

displayInRoom :: Rid -> SS ()
displayInRoom rid = do
  room  <- hoistG $ toRoom rid
  items <- hoistG $ listItems room
  exits <- hoistG $ listExits room
  when (not $ null $ room ^. rItems) $
    liftIO $ putStrLn $ "\n" <> items
  liftIO $ putStrLn ""
  liftIO $ putStrLn $ "The following exits are visible: " <> intercalate ", " (_eLabel <$> exits) <> "."


doOption :: [(Text, SS ())] -> SS ()
doOption options = do
    liftIO $ putStrLn ""
    liftIO $ putStrLns (toString <$> os)
    liftIO $ putStrLn "Enter number:"
    s <- liftIO $ getLine
    case find (\(Option i _ _) -> i == s) os of
      Just (Option _ _ atn) -> atn
      Nothing               -> doOption options
  where
    toString (Option i l _) = i <> ") " <> l
    os = zipWith (\(l, atn) i -> Option (show' i) l atn) options [(1 :: Int)..] -- explicit type to avoid warning


showInventory :: SS ()
showInventory = do
  story <- get
  let inventory         = story ^. (sPlayer . pInventory)
      inventoryMenu oid = doOption [ ("Examine " <> (toObject2 story oid ^. oTitle), examineO oid)
                                   , ("Use with"                                   , useWith oid)
                                   ]
  doOption $ ((^. oTitle) . toObject2 story &&& inventoryMenu) <$> inventory

takeItemI :: [Oid] -> SS ()
takeItemI items = do
  story <- get
  doOption $ (("Take " <>) . (^. oTitle) . toObject2 story &&& takeO) <$> items

takeO :: Oid -> SS ()
takeO oid = do
  o <- hoistG $ toObject oid
  liftIO $ putStrLn $ "You take " <> the o <> "."
  hoistG $ takeItemS oid

examineI :: [Oid] -> SS ()
examineI items = do
  story <- get
  doOption $ ((^. oTitle) . toObject2 story &&& examineO) <$> items

examineO :: Oid -> SS ()
examineO oid = do
  o <- hoistG $ toObject oid
  descr <- hoistG $ runAction $ o ^. oDescr
  liftIO $ putStrLn $ "You examine " <> the o <> "."
  liftIO $ putStrLn $ " " <> intercalate "\n" descr

goto' :: Action (Maybe Rid) -> SS ()
goto' roomAction = do
  eRid <- hoistG $ goto roomAction
  either (liftIO . putStrLns) displayRoom eRid

useWith :: Oid -> SS ()
useWith oid1 = do
  story <- get
  let player = story ^. sPlayer
      items = player ^. pInventory <> toRoom2 story (player ^. pLocation) ^. rItems
  doOption $ ((^. oTitle) . toObject2 story &&& useWithO oid1) <$> items

useWithO :: Oid -> Oid -> SS ()
useWithO oid1 oid2 = do
  o1 <- hoistG $ toObject oid1
  o2 <- hoistG $ toObject oid2
  liftIO $ putStr $ "You use " <> the o1 <> " with " <> the o2 <> ". "
  txt <- hoistG $ use oid1 oid2
  if null txt
    then liftIO $ putStrLn "Nothing happens."
    else liftIO $ putStrLns txt

sayAction :: Action () -> SS ()
sayAction action = do
  modify $ sSay .~ []
  txt   <- hoistG $ runAction action
  liftIO $ putStrLns txt
  story <- get
  let nextSayOptions = story ^. sSay
  if not $ null nextSayOptions
    then showSayOptions nextSayOptions
    else return ()

showSayOptions ::  [(Text, Action ())] -> SS ()
showSayOptions sayOptions = do
  doOption $ (\(s, action) -> ("Say \"" <> s <> "\"", sayAction action)) <$> sayOptions



data Option = Option Text Text (SS ())
