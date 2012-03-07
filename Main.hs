module Main where

import qualified Text.XML.HXT.XPath as XP
import qualified Text.XML.HXT.Core as X

import qualified Network.Curl as C
import qualified Network.Curl.Code as CC
import qualified Network.Curl.Opts as CO

import System.Environment
import System.Directory (getAppUserDataDirectory, doesFileExist)

appName = "leproimg"

getConfigFileName = getAppUserDataDirectory appName

extractImgTags content = X.runX $
  X.readString [X.withValidate X.no, X.withParseHTML X.yes] content X.>>>
  XP.getXPathTrees "//div[@class='dt']/*/img" X.>>>
  X.deep 
      ( X.isElem X.>>> X.hasName "img" X.>>> X.getAttrValue "src" )

loadPostPage url leproCookie = do
  C.initialize
  (cCode, response) <- C.curlGetString url [ CO.CurlCookie leproCookie ]
  if cCode /= CC.CurlOK
    then fail $ show cCode
    else extractImgTags response >>= putStrLn . goHtml
  where
    goHtml = concatMap (\url -> "<img src=\"" ++ url ++ "\"/>\n")

main = do
  (url:_) <- getArgs
  fileNameFromDir <- getConfigFileName
  go url fileNameFromDir =<< doesFileExist fileNameFromDir
  where
    go _ fileNameFromDir False = fail $ "Can not read config file " ++ fileNameFromDir
    go url fileNameFromDir True = readFile fileNameFromDir >>= loadPostPage url
