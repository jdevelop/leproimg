{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import qualified Text.XML.HXT.XPath as XP
import qualified Text.XML.HXT.Core as X

import qualified Network.Curl as C
import qualified Network.Curl.Code as CC
import qualified Network.Curl.Opts as CO

import System.Environment
import System.Directory (getAppUserDataDirectory, doesFileExist)

import Happstack.Server (nullConf, simpleHTTP, FromReqURI(..),ToMessage(..),
                         toResponse, ok, badRequest, dir, path, flatten)
import Control.Monad (msum, liftM)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Applicative ((<$>))
import Data.Maybe

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL

appName = "leproimg"

getConfigFileName = getAppUserDataDirectory appName

newtype HashId a = HashId { getHashId :: a }

newtype HTMLResponse a = HTMLResponse { getResponseData :: a }

type HashIdInt = HashId Int
type HTMLResponseStr = HTMLResponse String

textHtml = C8.pack "text/html"

instance ToMessage HTMLResponseStr where
  toContentType _ = textHtml
  toMessage = BL.fromChunks . (:[]) . C8.pack . getResponseData

instance FromReqURI HashIdInt where
  fromReqURI  = listToMaybe . map (HashId . fst) . reads

extractImgTags content = X.runX $
  X.readString [X.withValidate X.no, X.withParseHTML X.yes] content X.>>>
  XP.getXPathTrees "//div[@class='dt']/*/img" X.>>>
  X.deep 
      ( X.isElem X.>>> X.hasName "img" X.>>> X.getAttrValue "src" )

loadPostPage url leproCookie = do
  liftIO C.initialize
  (cCode, response) <- liftIO $ C.curlGetString url [ CO.CurlCookie leproCookie ]
  if cCode /= CC.CurlOK
    then return . HTMLResponse $ show cCode
    else liftIO $ goHtml <$> extractImgTags response
  where
    goHtml src = HTMLResponse $ "<html><body>" ++ concatMap (\url -> "<img src=\"" ++ url ++ "\"/>\n") src ++ "</body></html>"

fetchAll leproCookie hashId = flatten $ 
  loadPostPage ( "http://leprosorium.ru/comments/" ++ show (getHashId hashId :: Int) )
               leproCookie

main = do
  fileNameFromDir <- getConfigFileName
  fileExists <- doesFileExist fileNameFromDir
  leproCookie <- readFile fileNameFromDir
  if fileExists 
    then simpleHTTP nullConf $ msum [
              dir "all" $ path (fetchAll leproCookie),
              badRequest ( toResponse "Invalid request URL")
              ]
    else fail $ "Can not read config file " ++ fileNameFromDir
