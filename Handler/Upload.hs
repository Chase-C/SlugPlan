module Handler.Upload where

{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import Text.Julius (RawJS (..))

import Database.Persist
import Database.Persist.Sql

import Data.Text (unpack)
import qualified Data.Map as M

import Scraper.UCSC
import Scraper.Subjects
import Scraper.ParserTypes

-------------------------------------------------------------------------------

getUploadR :: Handler Html
getUploadR = do
    (uploadWidget,  uploadEnctype)  <- generateFormPost uploadForm
    let uploadSub = Nothing :: Maybe String
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "SlugPlan"
        $(widgetFile "upload")

postUploadR :: Handler Html
postUploadR = do
    ((result, uploadWidget), uploadEnctype) <- runFormPost uploadForm
    let handlerName = "postUploadR" :: Text
    pdfSub <- case result of
        FormSuccess res -> do
            subMap <- liftIO $ getSubjectMap $ Data.Text.unpack $ fileName res
            keys <- insertSubjectMap subMap
            return $ Just $ (show $ subMap) ++ " : " ++ (show $ take 4 keys)
        _                -> return Nothing

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "SlugPlan"
        $(widgetFile "upload")

insertSubjectMap :: SubjectMap -> Handler [Key Course]
insertSubjectMap subMap =
    let courses = map snd $ M.toList subMap
    in  runDB $ concat <$> mapM (mapM insert) courses

uploadForm :: Form FileInfo
uploadForm = renderBootstrap3 BootstrapBasicForm $
    fileAFormReq "Choose a text file to parse"
