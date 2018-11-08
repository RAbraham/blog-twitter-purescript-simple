module Main where

import Control.Bind (pure)
import Data.Either (Either(..))
import Data.Semigroup ((<>))
import Data.String.Base64 as S
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff_)
import Effect.Class.Console (errorShow, log)
import Milkis as M
import Milkis.Impl.Node (nodeFetch)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Prelude (Unit, bind, show, ($))
import Simple.JSON as SimpleJSON

type Status =
  { created_at :: String
  , id_str :: String
  , text :: String
  }

type SearchResults =
  { statuses :: Array Status
  }

type TwitterCredentials =
  { consumer_key :: String
  , consumer_secret :: String
  , access_token :: String
  , access_token_secret :: String
  }

type BearerAuthorization =
  { token_type :: String
  , access_token :: String
  }

parseConfig :: String -> Either String TwitterCredentials
parseConfig s =
  case SimpleJSON.readJSON s of
    Left error -> Left (show error)
    Right (creds :: TwitterCredentials) -> Right creds



readConfigStr :: String -> Aff String
readConfigStr path =  readTextFile UTF8 path

readConfig :: String -> Aff (Either String TwitterCredentials)
readConfig path = do
  cStr <- readConfigStr path
  pure $ parseConfig cStr

fetch :: M.Fetch
fetch = M.fetch nodeFetch

authorizationStr :: TwitterCredentials -> String
authorizationStr credentials =
  S.encode $ credentials.consumer_key <> ":" <> credentials.consumer_secret

getTokenCredentialsStr :: String -> Aff (Either String String)
getTokenCredentialsStr basicAuthorizationStr = do
    let
      opts =
        { body: "grant_type=client_credentials"
        , method: M.postMethod
        , headers: M.makeHeaders { "Authorization": basicAuthorizationStr
                                 , "Content-Type": "application/x-www-form-urlencoded;charset=UTF-8"
                                 }

        }
    _response <- attempt $ fetch (M.URL "https://api.twitter.com/oauth2/token") opts
    case _response of
      Left e -> do
        pure (Left $ show e)
      Right response -> do
        theText <- M.text response
        pure (Right theText)

getTokenCredentials :: TwitterCredentials -> Aff (Either String BearerAuthorization)
getTokenCredentials credentials = do
  tokenCredentialsStrE <- getTokenCredentialsStr $ basicHeader $ authorizationStr credentials
  case tokenCredentialsStrE of
    Left error -> do
      pure (Left error)
    Right tokenCredentialsStr -> do
      let tokenCredentialsE = toBearerAuthorization(tokenCredentialsStr)
      case tokenCredentialsE of
        Left error -> do
          pure (Left error)
        Right authResult -> do
          pure (Right authResult)

basicHeader :: String -> String
basicHeader base64EncodedStr = "Basic " <> base64EncodedStr

toBearerAuthorization :: String -> Either String BearerAuthorization
toBearerAuthorization tokenString = do
  case SimpleJSON.readJSON tokenString of
    Left e -> do
      Left $ show e
    Right (result :: BearerAuthorization) -> do
      Right result

twitterURL :: String -> M.URL
twitterURL singleSearchTerm = M.URL $ "https://api.twitter.com/1.1/search/tweets.json?q=" <> singleSearchTerm

showResults :: BearerAuthorization -> String -> Aff (Either String SearchResults)
showResults credentials singleSearchTerm = do
  let
    opts =
      { method: M.getMethod
      , headers: M.makeHeaders { "Authorization": "Bearer " <> credentials.access_token}

      }
  _response <- attempt $ fetch (twitterURL singleSearchTerm) opts
  case _response of
    Left e -> do
      pure (Left $ show e)
    Right response -> do
      stuff <- M.text response
      let aJson = SimpleJSON.readJSON stuff
      case  aJson of
        Left e -> do
          pure $ Left $ show e
        Right (result :: SearchResults) -> do
          pure (Right result)



main :: Effect Unit
main = launchAff_ do
  let searchTerm = "Kim Kardashian"
  config <- readConfig "./config/twitter_credentials.json"
  case config of
    Left errorStr -> errorShow errorStr
    Right credentials -> do
      tokenCredentialsE <- getTokenCredentials credentials
      case tokenCredentialsE of
        Left error ->
          errorShow error
        Right tokenCredentials -> do
          resultsE <- showResults tokenCredentials searchTerm
          case resultsE of
            Left error ->
              errorShow error
            Right result ->
              log $ show $ "Response:" <> (show result.statuses)
