{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

module Main where

import Happstack.Server
import Control.Monad (msum)
import           System.Console.CmdArgs.Implicit ((&=))
import qualified System.Console.CmdArgs.Implicit as I

import Pages.Index (index)
import Pages.Exercise (exercise)
import Pages.Finished (finished)
import Model.KoanManager (koans, answeredKoan, getKoan)
import Helpers.RoutingHelper (splitOnKeyword)


main :: IO ()
main = do
  config <- I.cmdArgs aConfig
  simpleHTTP (hConf config) handlers

-- main :: IO ()
-- main = simpleHTTP nullConf $ handlers

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)


handlers :: ServerPart Response
handlers =
    do decodeBody myPolicy
       msum [
                dir "img" $ serveDirectory DisableBrowsing [] "img/",
                dir "Style" $ serveDirectory DisableBrowsing [] "Style/",
                dir "koans" $ msum[
                    Happstack.Server.method GET >> uriRest (\params -> paramsToServerPart params),
                    Happstack.Server.method POST >> answeredKoan
                ],
                dir "finished" $ finished,
                index
            ]


parseIndexes :: String -> (Int, Int)
parseIndexes url = (read $ fst indexes, read $ snd indexes)
    where indexes = splitOnKeyword url "" '/'

cleanURIParams :: String -> String
cleanURIParams uri = if last uri == '?'
                        then take (length uri - 1) uri
                        else uri

paramsToServerPart :: String -> ServerPart Response
paramsToServerPart params = exercise ((getKoan indexes), themeIndex, koanIndex, "")
    where indexes    = parseIndexes $ cleanURIParams $ drop 1 params
          themeIndex = fst indexes
          koanIndex  = snd indexes


-- Heroku Config
--------------------------------------------------------------------------------

data Config =
  Config { port :: Int, timeout :: Int } deriving ( Show, Eq, I.Data, I.Typeable )

hConf :: Config -> Conf
hConf (Config {..}) = nullConf { Happstack.Server.timeout = timeout, Happstack.Server.port = port }

aConfig :: Config
aConfig =
  Config { port    = 8000  &= I.help "Port number"
                           &= I.typ "INT"
         , timeout = 30    &= I.help "Timeout"
                           &= I.typ "SECONDS"
         }
    &= I.summary "HKTest server"
    &= I.program "server"
