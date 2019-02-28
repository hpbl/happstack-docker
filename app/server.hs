
module Main where

import Happstack.Server
import Control.Monad (msum)
import System.Environment

import Pages.Index (index)
import Pages.Exercise (exercise)
import Pages.Finished (finished)
import Model.KoanManager (koans, answeredKoan, getKoan)
import Helpers.RoutingHelper (splitOnKeyword)


main :: IO ()
main = do
    putStrLn "COMEÇOU"
    port <- getEnv "PORT"
    putStrLn port
    let conf = nullConf { port = read port }
    simpleHTTP conf $ handlers

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




