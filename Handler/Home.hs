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
    (widget, enctype) <- generateFormPost searchForm
    courses           <- runDB $ selectList [] [Asc CourseSubject, Asc CourseNumber]
    defaultLayout $ do
        [whamlet|
            <form method=post action=@{AllCoursesR} enctype=#{enctype}>
                ^{widget}|]
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")

getAllCourses2R :: Handler Html
getAllCourses2R = do
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses2")

-- getAcenR :: Handler Html
-- getAcenR = do
--     courses <- runDB $ selectList [CoursePrefix ==. "ACEN"] []
--     defaultLayout $ do
--         setTitle "SlugPlan: Browse Courses"
--         $(widgetFile "browsecourses")

-- getAnthR :: Handler Html
-- getAnthR = do
--     courses <- runDB $ selectList [CoursePrefix ==. "ANTH"] []
--     defaultLayout $ do
--         setTitle "SlugPlan: Browse Courses"
--         $(widgetFile "browsecourses")

-- getAplxR :: Handler Html
-- getAplxR = do
--     courses <- runDB $ selectList [CoursePrefix ==. "APLX"] []
--     defaultLayout $ do
--         setTitle "SlugPlan: Browse Courses"
--         $(widgetFile "browsecourses")


postAllCoursesR :: Handler Html
postAllCoursesR = do
    ((result, _), _)        <- runFormPost searchForm
    courses <- case result of
        FormSuccess numString -> runDB $ matchCourses numString
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

searchForm :: Form Text
searchForm = renderBootstrap3 BootstrapBasicForm $ areq textField "Search a class?" Nothing

matchCourses :: MonadIO m => Text -> ReaderT SqlBackend m [Entity Course]
matchCourses strn = rawSql sqlStrn values
    where fields     = ["subject", "prefix", "number", "name"]
          strns      = words strn
          numStrns   = length strns
          searchOrs  = intercalate " or " $ map (\f -> f ++ " like ?") fields
          searchAnds = intercalate " and " $ replicate numStrns ("(" ++ searchOrs ++ ")")
          sqlStrn    = "select ?? from course where " ++ searchAnds
          values     = concat $ map (\s -> map PersistText $ replicate 4 $ surr s) strns
          surr s     = "%" ++ s ++ "%"
