module TextAd.Test.Util
  ( initStory
  ) where

import BasicPrelude
import Control.Monad.Trans.State       (evalState)

import TextAd.Model.Core
import TextAd.Interpreter.StoryBuilder (toStory)
import qualified TextAd.Model.History  as H

initStory :: StoryDef -> Text -> Either Text ([Text], [Text])
initStory story path =
  either (Left . snd) Right $ evalState (H.initStory path) (toStory story)
