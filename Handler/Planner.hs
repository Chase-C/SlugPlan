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
import Text.Julius (RawJS (..))

import Database.Persist
import Database.Persist.Sqlite

--------------------------------------------------------------------------------

getPlannerR :: Handler Html
getPlannerR = defaultLayout $ do
    let (courseFormId, courseListId) = courseIds
    setTitle "Course Planner"
    $(widgetFile "planner")

courseIds :: (Text, Text)
courseIds = ("js-courseForm", "js-courseList")
