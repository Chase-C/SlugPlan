module Handler.Planner where

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
import Settings
import Settings.StaticFiles
import Text.Julius (RawJS (..))
import Data.Aeson.Types (Result (..))

import Database.Persist
import Database.Persist.Sqlite

--------------------------------------------------------------------------------

getPlannerR :: Handler Html
getPlannerR = do
    defaultLayout $ do
        let (courseFormId, courseListId) = courseIds
        setTitle "Course Planner"
        $(widgetFile "planner")

putNewCourseR :: Handler Value
putNewCourseR = do
    courseName <- (requireJsonBody :: Handler Text)
    mCourse    <- runDB $ selectFirst [CourseName ==. courseName] []
    case mCourse of
        (Just course) -> returnJson course
        _             -> notFound

postNewCourseR :: Handler Value
postNewCourseR =
    returnJson ("ok" :: Text)

getNewCourseR :: Handler Value
getNewCourseR =
    returnJson ("ok" :: Text)

postNewQuarterR :: Handler Value
postNewQuarterR =
    returnJson ("ok" :: Text)

getSearchCourseR :: Text -> Handler Value
getSearchCourseR searchStr = do
    courses <- runDB $ selectList [CourseNumber ==. toUpper(searchStr)]
                                  [Asc CourseSubject, Asc CourseNumber]
    returnJson $ take 7 courses

courseIds :: (Text, Text)
courseIds = ("js-courseForm", "js-courseList")
