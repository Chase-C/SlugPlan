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
import Database.Persist.Sql

--import Data.List (intercalate)

--------------------------------------------------------------------------------

getPlannerR :: Handler Html
getPlannerR = do
    defaultLayout $ do
        let (courseFormId, courseListId) = courseIds
        setTitle "Course Planner"
        $(widgetFile "planner")

deleteNewCourseR :: Handler Value
deleteNewCourseR =
    returnJson ("ok" :: Text)

putNewCourseR :: Handler Value
putNewCourseR = do
    courseName <- (requireJsonBody :: Handler Text)
    mCourse    <- runDB $ selectFirst [CourseName ==. courseName] []
    -- userCourses <- runDB $ selectList [UserCourseUserName ==. userName] []
    -- [UserCourse]
    -- courseId = map userCourseCourse userCourses
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
getSearchCourseR searchStrn = do
    courses <- runDB $ matchCourses searchStrn
    returnJson $ take 7 courses

courseIds :: (Text, Text)
courseIds = ("js-courseForm", "js-courseList")

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
