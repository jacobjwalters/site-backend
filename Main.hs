{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import System.Directory (doesFileExist)
import System.Random

import Web.Scotty
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run, defaultSettings, setPort)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettingsChain)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

htmlHeader :: ActionM ()
htmlHeader = setHeader "Content-Type" "text/html;charset=utf-8"

homePage :: ActionM ()
homePage = do
    htmlHeader
    file "index.html"

acme :: ActionM ()
acme = do
    name <- param "name"
    file $ ".well-known/acme-challenge/" ++ name

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
    get "/css/:path" css
    get "/archive.html" $ file "archive.html"
    get "/about.html"   $ file "about.html"
    get "/contact.html" $ file "contact.html"
    get "/links.html"   $ file "links.html"
    get "/posts" $ redirect "/archive.html"
    get "/posts/:post" posts
    get "/.well-known/acme-challenge/:name" acme
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
            (certPath ++ "privkey.pem"))
        (setPort 3443 defaultSettings)
        app

main :: IO ()
main = do
    void $ forkIO siteTLS
    site
