module TextAd.Interpreter.UseAction
  ( runUseAction
  ) where

import BasicPrelude
import Control.Comonad        (extend, extract)
import Control.Comonad.Cofree (coiter)
import Control.Comonad.Store  (Store, store, pos)
import Data.Dynamic           (fromDynamic)
import Lens.Family            ((^.))
import qualified Data.Map     as M

import TextAd.Model.Core
import TextAd.Util.CoProduct  ((*:*))
import TextAd.Util.Pairing    (pair)

coWith :: Store (Oid, Story) (Maybe (Action ())) -> CoWithF (Store (Oid, Story) (Maybe (Action ())))
coWith s =
  let (oid, story) = pos s
  in CoWith $ \oid' a -> if oid == oid' then extend (const (Just a) . extract) s
                                        else s
-- TODO assumes there is only one matching action - will return the last one..

coGetState :: Store (Oid, Story) (Maybe (Action ())) -> CoGetStateF (Store (Oid, Story) (Maybe (Action ())))
coGetState s =
  CoGetState $ \sid ->
    let (_, story) = pos s
        val = case M.lookup (sidK sid) (story ^. sStates) of
            Just dyn -> case fromDynamic dyn of
                          Just v  -> v
                          Nothing -> error' $ "could not extract value from" <> (sidK sid)
            Nothing -> error' $ "State " <> show' sid <> " has not been added to story"
    in (val, s)

runUseAction :: Story -> UseAction r -> Oid -> Maybe (Action ())
runUseAction story useAction oid =
  let start = store (const Nothing) (oid, story)
      cofree = coiter (coWith *:* coGetState) start
      free = useAction
      final :: Store (Oid, Story) (Maybe (Action ()))
      final = pair (\l _ -> l) cofree free
  in extract final
