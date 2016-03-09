module Handler.Department2 where

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

getGreeR :: Handler Html
getGreeR = do
    courses <- runDB $ selectList [CoursePrefix ==. "GREE"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getLtgrR :: Handler Html
getLtgrR = do
    courses <- runDB $ selectList [CoursePrefix ==. "LTGR"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getHebrR :: Handler Html
getHebrR = do
    courses <- runDB $ selectList [CoursePrefix ==. "HEBR"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getHistR :: Handler Html
getHistR = do
    courses <- runDB $ selectList [CoursePrefix ==. "HIST"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getHavcR :: Handler Html
getHavcR = do
    courses <- runDB $ selectList [CoursePrefix ==. "Havc"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getHiscR :: Handler Html
getHiscR = do
    courses <- runDB $ selectList [CoursePrefix ==. "HISC"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getItalR :: Handler Html
getItalR = do
    courses <- runDB $ selectList [CoursePrefix ==. "ITAL"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getLtitR :: Handler Html
getLtitR = do
    courses <- runDB $ selectList [CoursePrefix ==. "LTIT"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getJapnR :: Handler Html
getJapnR = do
    courses <- runDB $ selectList [CoursePrefix ==. "JAPN"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getKrsgR :: Handler Html
getKrsgR = do
    courses <- runDB $ selectList [CoursePrefix ==. "KRSG"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getLaadR :: Handler Html
getLaadR = do
    courses <- runDB $ selectList [CoursePrefix ==. "LAAD"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getLatnR :: Handler Html
getLatnR = do
    courses <- runDB $ selectList [CoursePrefix ==. "LATN"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")