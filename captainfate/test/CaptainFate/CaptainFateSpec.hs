module CaptainFate.CaptainFateSpec
  ( tests
  ) where

import BasicPrelude
import Test.HUnit
import qualified Data.Text     as T

import TextAd.Test.Util        (initStory)
import CaptainFate.CaptainFate (story, walkthrough)

import Debug.Trace (trace)

tests :: Test
tests = TestList
  [ TestLabel "run walkthrough" walkthroughTest
  ]

walkthroughTest :: Test
walkthroughTest = TestCase $
  let (historicTxt, txt) = either (error . T.unpack) id $ initStory story walkthrough
  in trace (T.unpack $ intercalate "\n\n" historicTxt) $ assertEqual "replay of walkthrough doesn't break" [] txt
