{-# LANGUAGE OverloadedStrings #-}

module Shortener where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (for_)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text, unpack)
import qualified Data.Text.Lazy as LT
import Network.HTTP.Types (status404, status400)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Regex.PCRE
import Web.Scotty

-- onlyLetters :: String -> String
-- onlyLetters s = replaceAll "" $ s *=~ [re|$([^a-zA-Z])|]

isValid :: String -> Bool
isValid url = url =~ ("^(https?):\\/\\/[-a-zA-Z0-9@:%._\\+~#=]{2,256}\\.[a-z]{2,6}\\b([-a-zA-Z0-9@:%_\\+.~#?&//=]*)$" :: String)

shortener :: IO ()
shortener = do
  urlsR <- newIORef (1 :: Int, mempty :: Map Int Text)
  scotty 3000 $ do
    get "/" $ do
      (_, urls) <- liftIO $ readIORef urlsR
      html $
        renderHtml $
          H.html $
            H.body $ do
              H.h1 "Shortener"
              H.form H.! A.method "post" H.! A.action "/" $ do
                H.input H.! A.type_ "text" H.! A.name "url"
                H.input H.! A.type_ "submit"
              H.table $
                for_ (M.toList urls) $ \(i, url) ->
                  H.tr $ do
                    H.td (H.toHtml i)
                    H.td (H.text url)
    post "/" $ do
      url <- param "url"
      ( if isValid (unpack url)
          then liftIO $
            modifyIORef urlsR $
              \(i, urls) ->
                (i + 1, M.insert i url urls)
          else raiseStatus status400 "URL not valid"
        )
      redirect "/"
    get "/:n" $ do
      n <- param "n"
      (_, urls) <- liftIO $ readIORef urlsR
      case M.lookup n urls of
        Just url ->
          redirect (LT.fromStrict url)
        Nothing ->
          raiseStatus status404 "not valid"