module TextAd.View.Ghcjs.Util
  ( readState
  , writeState
  , addHead
  ) where

import  BasicPrelude              hiding (on)
import  GHCJS.DOM.Types           (Document, Nullable, toJSString, nullableToMaybe)
import  GHCJS.DOM.Document        (getHead, createElement)
import  GHCJS.DOM.Element         (setOuterHTML)
import  GHCJS.DOM.Node            (appendChild)
import  GHCJS.Marshal             (fromJSVal)
import  GHCJS.Types               (JSVal, JSString)
import qualified Data.Text        as T

addHead :: MonadIO m => Document -> Text -> m ()
addHead doc htmlString = do
  Just h   <- getHead doc
  Just elm <- createElement doc $ Just ("h" :: Text) -- cannot use Nothing on Safari - name is ignored anyway
  _ <- appendChild h (Just elm)
  setOuterHTML elm $ Just $ T.unpack htmlString

foreign import javascript unsafe
  "$r = window.localStorage.getItem($1);"
  js_getStorageItem :: JSString -> IO (Nullable JSVal)

getStorageItem :: Text -> IO (Maybe Text)
getStorageItem name = do
   nullable <- js_getStorageItem (toJSString name)
   case nullableToMaybe nullable of
     Just jsval -> fromJSVal jsval
     Nothing    -> return Nothing

foreign import javascript unsafe
  "window.localStorage.setItem($1, $2);"
  js_setStorageItem :: JSString -> JSString -> IO ()

setStorageItem :: Text -> Text -> IO ()
setStorageItem name value =
  js_setStorageItem (toJSString name) (toJSString value)

readState :: MonadIO m => Document -> m Text
readState _ =
  liftIO $ (fromMaybe "") <$> getStorageItem "path"

writeState :: MonadIO m => Document -> Text -> m ()
writeState _ path =
  liftIO $ setStorageItem "path" path
