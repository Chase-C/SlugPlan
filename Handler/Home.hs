module Handler.Home where

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

import Data.Text (unpack)
import qualified Data.Map as M

import Scraper.UCSC
import Scraper.Subjects

-------------------------------------------------------------------------------------------

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    (pdfWidget,  pdfEnctype)  <- generateFormPost pdfForm
    let pdfSub     = Nothing :: Maybe String
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "SlugPlan"
        $(widgetFile "homepage")


postHomeR :: Handler Html
postHomeR = do
    (pdfWidget,  pdfEnctype)            <- generateFormPost pdfForm
    let pdfSub      = Nothing :: Maybe String
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "SlugPlan"
        $(widgetFile "homepage")

postPdfR :: Handler Html
postPdfR = do
    ((result, pdfWidget), pdfEnctype) <- runFormPost pdfForm
    let handlerName = "postPdfR" :: Text
    pdfSub <- case result of
        FormSuccess res -> do
            --text <- liftIO $ parsePdf $ Data.Text.unpack $ fileName res
            --let subMap = getSubjectMap' text $ Data.Text.unpack $ fileName res
            subMap <- liftIO $ getSubjectMap $ Data.Text.unpack $ fileName res
            keys <- insertSubjectMap subMap
            return $ Just $ (show $ subMap) ++ " : " ++ (show $ take 4 keys)
            --case subMap of
            --    (Just m) -> do
            --        keys <- insertSubjectMap m
            --        return $ Just $ (show $ m) ++ " : " ++ (show $ take 4 keys)
            --        --return $ Just $ show keys
            --    _        -> return Nothing
        _                -> return Nothing

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "SlugPlan"
        $(widgetFile "homepage")
getAllCoursesR :: Handler Html
getAllCoursesR = do
    courses <- runDB $ selectList [] [Asc CourseSubject, Asc CourseNumber]--[CoursePrefix ==. "ANTH"]
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getAllCourses2R :: Handler Html
getAllCourses2R = do
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses2")

getAcenR :: Handler Html
getAcenR = do
    courses <- runDB $ selectList [CoursePrefix ==. "ACEN"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getAnthR :: Handler Html
getAnthR = do
    courses <- runDB $ selectList [CoursePrefix ==. "ANTH"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getAplxR :: Handler Html
getAplxR = do
    courses <- runDB $ selectList [CoursePrefix ==. "APLX"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")





getCourseR :: CourseId -> Handler Html
getCourseR courseId = do
    course <- runDB $ get404 courseId
    defaultLayout $ do
        coursesub  <- return $ courseSubject course
        coursenum  <- return $ courseNumber course
        setTitle ("SlugPlan: " ++ (toHtml coursesub) ++ " " ++ (toHtml coursenum))
        $(widgetFile "course")

insertSubjectMap :: SubjectMap -> Handler [Key Course]
insertSubjectMap subMap =
    let courses = map snd $ M.toList subMap
    in  runDB $ concat <$> mapM (mapM insert) courses

pdfForm :: Form FileInfo
pdfForm = renderBootstrap3 BootstrapBasicForm $ fileAFormReq "Choose a PDF to parse"
