{-# LANGUAGE FlexibleContexts #-}

module TextAd.Interpreter.StateInterpreter
  ( coMkState
  , coGetState
  , coSetState
  ) where

import BasicPrelude
import Control.Comonad.Store (ComonadStore, pos, seeks)
import Data.Dynamic          (fromDynamic, toDyn)
import Lens.Family           ((%~), (^.))
import qualified Data.Map    as M

import TextAd.Model.Core

coMkState :: ComonadStore Story w
          => w a
          -> CoMkStateF (w a)
coMkState w = CoMkState $ \l val ->
  let sid = Sid l -- we could generate label instead of client passing in (e.g. UUID)
  in (sid, seeks (sStates %~ (M.insert (sidK sid) (toDyn val))) w)

coGetState :: ComonadStore Story w
           => w a
           -> CoGetStateF (w a)
coGetState w = CoGetState $ \sid ->
  let s   = pos w
      val = case M.lookup (sidK sid) (s ^. sStates) of
              Just dyn -> case fromDynamic dyn of
                            Just val' -> val'
                            Nothing   -> terror $ "could not extract value from" <> sidK sid
              Nothing -> terror $ "State " <> tshow sid <> " has not been added to story"
  in (val, w)

coSetState :: ComonadStore Story w
           => w a
           -> CoSetStateF (w a)
coSetState w = CoSetState $ \sid val ->
  seeks (sStates %~ (M.insert (sidK sid) $ toDyn val)) w
