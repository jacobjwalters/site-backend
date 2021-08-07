{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import System.Directory (doesFileExist)
import System.Random

htmlHeader :: ActionM ()
htmlHeader = setHeader "Content-Type" "text/html;charset=utf-8"

homePage :: ActionM ()
homePage = do
    htmlHeader
    file "index.html"

css :: ActionM ()
css = do
    path <- param "path"
    file $ "css/" ++ path

wordlist = ["gamer", "test", "pog"]

wordPage :: ActionM ()
wordPage = do
    beam <- param "word"

    r <- randomRIO (0, length wordlist - 1)
    let a = wordlist !! r

    html $ mconcat [ "<h1>Scotty, ", beam, " me up!</h1>"
                   , "<p>", a, "</p>"]

postList :: ActionM ()
postList = do
    htmlHeader
    file "archive.html"

posts :: ActionM ()
posts = do
    htmlHeader
    postPath <- param "post"
    exists <- liftIO . doesFileExist $ "posts/" ++ postPath
    if exists
        then file $ "posts/" ++ postPath
        else pageNotFound

pageNotFound :: ActionM ()
pageNotFound = html "<h1>404</h1><p>Page not found!</p>"

main :: IO ()
main = scotty 3000 $ do
    middleware logStdoutDev
    get "/" homePage
    get "/css/:path" css
    get "/posts" $ postList
    get "/posts/:post" posts
    --get "/:word" wordPage
    notFound pageNotFound
