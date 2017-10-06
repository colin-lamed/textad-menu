module Main where

import TextAd.View.CommandLine (runCommandLine)
import CaptainFate.CaptainFate (story)

main :: IO ()
main = runCommandLine story
