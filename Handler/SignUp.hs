module Handler.SignUp where


{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Import
import Yesod
import Control.Applicative
import Data.Text (Text)
import Database.Persist

data Input = Input

-- Google client ID. 
clientId :: Text
clientId = "488381746948-hqnuulhpu1kotloi562d9pf7stkpk18h.apps.googleusercontent.com"

-- Google secret ID.
clientSecret :: Text
clientSecret = "eiOFqIa6Y4epKymLX8AccRFl"

--Routes added to 'routes'
{-}
mkYesod "Input" [parseRoutes|
/ SignUpR GET
/input InputR GET
|]
-}

--instance Yesod Input

instance RenderMessage Input FormMessage where
    renderMessage _ _ = defaultFormMessage

--data Person = Person { personName :: Text, personPassword :: Text }
  --  deriving Show

getRootR :: Handler Html
getRootR = do 
    defaultLayout $ do
        setTitle "Sign Up"
        $(widgetFile "signup")

postInputR :: Handler Html
postInputR = do
    person <- runInputGet $Person
                <$> ireq textField "name"
                <*> ireq textField "password"
    runDB $ insert person
    defaultLayout [whamlet|<p>#{show person}|]
