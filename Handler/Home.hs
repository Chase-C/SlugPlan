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
import Database.Persist.Sql


import Data.Text (unpack)
import qualified Data.Map as M

import Scraper.UCSC
import Scraper.Subjects
import Scraper.ParserTypes

-------------------------------------------------------------------------------

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "SlugPlan"
        $(widgetFile "homepage")


postHomeR :: Handler Html
postHomeR = do
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "SlugPlan"
        $(widgetFile "homepage")

getAllCoursesR :: Handler Html
getAllCoursesR = do
    (widgetSJ, enctypeSJ)   <- generateFormPost subjectForm
    (widgetNum, enctypeNum) <- generateFormPost numberForm
    (widgetNA, enctypeNA)   <- generateFormPost nameForm
    courses                 <- runDB $ selectList [] [Asc CourseSubject, Asc CourseNumber]
    defaultLayout $ do
        [whamlet|
                ^{widgetSJ}
            <form method=post action=@{AllCourses3R} enctype=#{enctypeNum}>
                ^{widgetNum}
                <name="numberForm">
            <form method=post action=@{AllCourses4R} enctype=#{enctypeNA}>
                ^{widgetNA}|]
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")

getAllCourses2R :: Handler Html
getAllCourses2R = do
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses2")

postAllCoursesR :: Handler Html
postAllCoursesR = do
    ((resultSJ, _), _)        <- runFormPost subjectForm
    courses <- case resultSJ of
        FormSuccess numString -> runDB $ subjectCourses numString
        _                     -> runDB $ selectList [] [Asc CourseSubject, Asc CourseNumber]
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")

postAllCourses3R :: Handler Html
postAllCourses3R = do
    ((resultNum, _), _)        <- runFormPost numberForm
    courses <- case resultNum of
        FormSuccess numString -> runDB $ numberCourses numString
        _                        -> runDB $ selectList [] [Asc CourseSubject, Asc CourseNumber]
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")

postAllCourses4R :: Handler Html
postAllCourses4R = do
    ((resultNA, _), _)        <- runFormPost nameForm
    courses <- case resultNA of
        FormSuccess numString -> runDB $ nameCourses numString
        _                     -> runDB $ selectList [] [Asc CourseSubject, Asc CourseNumber]
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


subjectForm :: Form Text
subjectForm = renderBootstrap3 BootstrapBasicForm $ areq textField "Subject:" Nothing

subjectCourses :: MonadIO m => Text -> ReaderT SqlBackend m [Entity Course]
subjectCourses strn = rawSql sqlStrn values
    where fields     = ["subject", "prefix", "subject", "prefix"]
          strns      = words strn
          numStrns   = length strns
          searchOrs  = intercalate " or " $ map (\f -> f ++ " like ?") fields
          searchAnds = intercalate " and " $ replicate numStrns ("(" ++ searchOrs ++ ")")
          sqlStrn    = "select ?? from course where " ++ searchAnds
          values     = concat $ map (\s -> map PersistText $ replicate 4 $ surr s) strns
          surr s     = "%" ++ s ++ "%"

numberForm :: Form Text
numberForm = renderBootstrap3 BootstrapBasicForm $ areq textField "Number:" Nothing

numberCourses :: MonadIO m => Text -> ReaderT SqlBackend m [Entity Course]
numberCourses strn = rawSql sqlStrn values
    where fields     = ["number", "number", "number", "number"]
          strns      = words strn
          numStrns   = length strns
          searchOrs  = intercalate " or " $ map (\f -> f ++ " like ?") fields
          searchAnds = intercalate " and " $ replicate numStrns ("(" ++ searchOrs ++ ")")
          sqlStrn    = "select ?? from course where " ++ searchAnds
          values     = concat $ map (\s -> map PersistText $ replicate 4 $ surr s) strns
          surr s     = "%" ++ s ++ "%"

nameForm :: Form Text
nameForm = renderBootstrap3 BootstrapBasicForm $ areq textField "Name:" Nothing

nameCourses :: MonadIO m => Text -> ReaderT SqlBackend m [Entity Course]
nameCourses strn = rawSql sqlStrn values
    where fields     = ["name", "name", "name", "name"]
          strns      = words strn
          numStrns   = length strns
          searchOrs  = intercalate " or " $ map (\f -> f ++ " like ?") fields
          searchAnds = intercalate " and " $ replicate numStrns ("(" ++ searchOrs ++ ")")
          sqlStrn    = "select ?? from course where " ++ searchAnds
          values     = concat $ map (\s -> map PersistText $ replicate 4 $ surr s) strns
          surr s     = "%" ++ s ++ "%"
