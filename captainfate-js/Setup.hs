import Distribution.Simple (defaultMainWithHooks)
import Pwa.Config
import Pwa.PwaSetup (pwaUserHooks)

main = defaultMainWithHooks $ pwaUserHooks config'

-- config' :: Config
-- config' = Config
--   { name             = "Tiny Text Adventure"
--   , shortName        = "Tiny Text Adventure"
--   , description      = Nothing
--   , dir              = Auto
--   , display          = Standalone
--   , orientation      = PortraitPrimary
--   , tileColour       = "#E09D5A" -- TODO type safe colours
--   , backgroundColour = "#FFFFFF"
--   , themeColour      = Just "#E09D5A"
--   , favicon          = "icons/favicon.jpg"
--   , icons            = [ Icon "icons/icon_128.png" ["128x128"] (Just "image/png")
--                        , Icon "icons/icon_256.png" ["256x256"] (Just "image/png")
--                        , Icon "icons/icon_512.png" ["512x512"] (Just "image/png")
--                        ]
--   , scope            = Nothing
--   , serviceWorkerTemplate = offlineServiceWorkerTemplate
--   , build            = Build { resourceDir = "resources" }
--   }

config' :: Config
config' = config
  { name             = "Captain Fate"
  , shortName        = "Captain Fate"
  , display          = Standalone
  , tileColour       = "#E09D5A" -- TODO type safe colours
  , backgroundColour = "#FFFFFF"
  , themeColour      = Just "#E09D5A"
  , favicon          = "icons/favicon.png"
  , icons            = [ Icon "icons/icon_128.png" ["128x128"] (Just "image/png")
                       , Icon "icons/icon_256.png" ["256x256"] (Just "image/png")
                       , Icon "icons/icon_512.png" ["512x512"] (Just "image/png")
                       ]
  , build            = Build { resourceDir = "resources" }
  }
