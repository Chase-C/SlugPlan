module Handler.Department3 where

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

getOceaR :: Handler Html
getOceaR = do
    courses <- runDB $ selectList [CoursePrefix ==. "OCEA"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getPhilR :: Handler Html
getPhilR = do
    courses <- runDB $ selectList [CoursePrefix ==. "PHIL"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getPhyeR :: Handler Html
getPhyeR = do
    courses <- runDB $ selectList [CoursePrefix ==. "PHYE"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getPhysR :: Handler Html
getPhysR = do
    courses <- runDB $ selectList [CoursePrefix ==. "PHYS"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getPoliR :: Handler Html
getPoliR = do
    courses <- runDB $ selectList [CoursePrefix ==. "POLI"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getPrtrR :: Handler Html
getPrtrR = do
    courses <- runDB $ selectList [CoursePrefix ==. "PRTR"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getPortR :: Handler Html
getPortR = do
    courses <- runDB $ selectList [CoursePrefix ==. "PORT"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getLtprR :: Handler Html
getLtprR = do
    courses <- runDB $ selectList [CoursePrefix ==. "LTPR"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getPsycR :: Handler Html
getPsycR = do
    courses <- runDB $ selectList [CoursePrefix ==. "PSYC"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getRussR :: Handler Html
getRussR = do
    courses <- runDB $ selectList [CoursePrefix ==. "RUSS"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getScicR :: Handler Html
getScicR = do
    courses <- runDB $ selectList [CoursePrefix ==. "SCIC"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getSocdR :: Handler Html
getSocdR = do
    courses <- runDB $ selectList [CoursePrefix ==. "SOCD"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getSocyR :: Handler Html
getSocyR = do
    courses <- runDB $ selectList [CoursePrefix ==. "SOCY"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")