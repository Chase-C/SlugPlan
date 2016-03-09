module Handler.Department5 where

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

getEconR :: Handler Html
getEconR = do
    courses <- runDB $ selectList [CoursePrefix ==. "ECON"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getEducR :: Handler Html
getEducR = do
    courses <- runDB $ selectList [CoursePrefix ==. "EDUC"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getEtR :: Handler Html
getEtR = do
    courses <- runDB $ selectList [CoursePrefix ==. "EE"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getLtelR :: Handler Html
getLtelR = do
    courses <- runDB $ selectList [CoursePrefix ==. "LTEL"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getEnvsR :: Handler Html
getEnvsR = do
    courses <- runDB $ selectList [CoursePrefix ==. "ENVS"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getFmstR :: Handler Html
getFmstR = do
    courses <- runDB $ selectList [CoursePrefix ==. "FMST"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getFilmR :: Handler Html
getFilmR = do
    courses <- runDB $ selectList [CoursePrefix ==. "FILM"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getFrenR :: Handler Html
getFrenR = do
    courses <- runDB $ selectList [CoursePrefix ==. "FREN"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getLtfrR :: Handler Html
getLtfrR = do
    courses <- runDB $ selectList [CoursePrefix ==. "LTFR"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getGameR :: Handler Html
getGameR = do
    courses <- runDB $ selectList [CoursePrefix ==. "GAME"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getGermR :: Handler Html
getGermR = do
    courses <- runDB $ selectList [CoursePrefix ==. "GERM"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getLtgeR :: Handler Html
getLtgeR = do
    courses <- runDB $ selectList [CoursePrefix ==. "LTGE"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")