module Main where

import TextAd.View.Ghcjs.View  (runGhcjs)
import CaptainFate.CaptainFate (story)

main :: IO ()
main = runGhcjs story
