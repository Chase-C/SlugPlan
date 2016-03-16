module Handler.Department where

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

getDepartmentR :: Text -> Handler Html
getDepartmentR dep = do
    courses <- runDB $ selectList [CoursePrefix ==. dep] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")

-- getAmsR :: Handler Html
-- getAmsR = do
--     courses <- runDB $ selectList [CoursePrefix ==. "AMS"] []
--     defaultLayout $ do
--         setTitle "SlugPlan: Browse Courses"
--         $(widgetFile "browsecourses")
-- getArtR :: Handler Html
-- getArtR = do
--     courses <- runDB $ selectList [CoursePrefix ==. "ART"] []
--     defaultLayout $ do
--         setTitle "SlugPlan: Browse Courses"
--         $(widgetFile "browsecourses")
-- getArtgR :: Handler Html
-- getArtgR = do
--     courses <- runDB $ selectList [CoursePrefix ==. "ARTG"] []
--     defaultLayout $ do
--         setTitle "SlugPlan: Browse Courses"
--         $(widgetFile "browsecourses")
-- getAstrR :: Handler Html
-- getAstrR = do
--     courses <- runDB $ selectList [CoursePrefix ==. "ASTR"] []
--     defaultLayout $ do
--         setTitle "SlugPlan: Browse Courses"
--         $(widgetFile "browsecourses")
-- getBiocR :: Handler Html
-- getBiocR = do
--     courses <- runDB $ selectList [CoursePrefix ==. "BIOC"] []
--     defaultLayout $ do
--         setTitle "SlugPlan: Browse Courses"
--         $(widgetFile "browsecourses")
-- getBiolR :: Handler Html
-- getBiolR = do
--     courses <- runDB $ selectList [CoursePrefix ==. "BIOL"] []
--     defaultLayout $ do
--         setTitle "SlugPlan: Browse Courses"
--         $(widgetFile "browsecourses")
-- getBmeR :: Handler Html
-- getBmeR = do
--     courses <- runDB $ selectList [CoursePrefix ==. "BME"] []
--     defaultLayout $ do
--         setTitle "SlugPlan: Browse Courses"
--         $(widgetFile "browsecourses")
-- getChemR :: Handler Html
-- getChemR = do
--     courses <- runDB $ selectList [CoursePrefix ==. "CHEM"] []
--     defaultLayout $ do
--         setTitle "SlugPlan: Browse Courses"
--         $(widgetFile "browsecourses")
-- getChinR :: Handler Html
-- getChinR = do
--     courses <- runDB $ selectList [CoursePrefix ==. "CHIN"] []
--     defaultLayout $ do
--         setTitle "SlugPlan: Browse Courses"
--         $(widgetFile "browsecourses")
