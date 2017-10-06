module TextAd.Interpreter.ExitsBuilder
  ( buildExits
  ) where

import BasicPrelude
import Control.Comonad        (extend, extract)
import Control.Comonad.Cofree (coiter)
import Control.Comonad.Store  (Store, store, pos)
import Data.Dynamic           (fromDynamic)
import Lens.Family            ((^.))
import qualified Data.Map      as M

import TextAd.Model.Core
import TextAd.Util.CoProduct  ((*:*))
import TextAd.Util.Pairing    (pair)

coAddExit :: Store Story [Exit] -> CoAddExitF (Store Story [Exit])
coAddExit s =
  CoAddExit $ \e ->
    extend ((e :) . extract) s

coGetState :: Store Story [Exit] -> CoGetStateF (Store Story [Exit])
coGetState s =
  CoGetState $ \sid ->
    let story = pos s
        val   = case M.lookup (sidK sid) (story ^. sStates) of
                  Just dyn -> case fromDynamic dyn of
                                Just v  -> v
                                Nothing -> error' $ "could not extract value from " <> (sidK sid)
                  Nothing -> error' $ "State " <> show' sid <> " has not been added to story"
    in (val, s)

buildExits :: Story -> ExitsBuilder r -> [Exit]
buildExits story exitsBuilder =
  let start = store (const []) story
      cofree = coiter (coAddExit *:* coGetState) start
      free = exitsBuilder
      final :: Store Story [Exit]
      final = pair (\l _ -> l) cofree free
  in extract final
