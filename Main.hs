{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import System.Directory (doesFileExist)
import System.Random

import Web.Scotty
import Network.Wai (Application, Middleware)
import Network.Wai.Handler.Warp (run, defaultSettings, setPort)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettingsChain)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

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

siteApp :: IO Application
siteApp = scottyApp $ do
    middleware logStdoutDev

    get "/" homePage
    get "/archive.html" postList
    get "/css/:path" css
    get "/posts" $ redirect "/archive.html"
    get "/posts/:post" posts
    --get "/:word" wordPage

    notFound pageNotFound

site :: IO ()
site = do
    app <- siteApp
    run 3080 app

siteTLS :: IO ()
siteTLS = do
    let certPath = "/etc/letsencrypt/live/jacobwalte.rs/"
    app <- siteApp
    runTLS
        (tlsSettingsChain
            (certPath ++ "cert.pem")
            [certPath ++ "chain.pem"]
            (certPath ++ "privkeycert.pem"))
        (setPort 3443 defaultSettings)
        app

main :: IO ()
main = do
    void $ forkIO siteTLS
    site
