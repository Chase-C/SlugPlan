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

import Database.Persist
import Database.Persist.Sqlite

--------------------------------------------------------------------------------

getPlannerR :: Handler Html
getPlannerR = do
    defaultLayout $ do
        let (courseFormId, courseListId) = courseIds
        setTitle "Course Planner"
        $(widgetFile "planner")

postNewCourseR :: Handler Value
postNewCourseR =
    returnJson ("bueno" :: Text)

getNewCourseR :: Handler Value
getNewCourseR =
    returnJson ("bueno" :: Text)

postNewQuarterR :: Handler Value
postNewQuarterR =
    returnJson ("bueno" :: Text)

courseIds :: (Text, Text)
courseIds = ("js-courseForm", "js-courseList")
