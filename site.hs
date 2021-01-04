--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Text.Pandoc.Highlighting (styleToCss)
import           Data.Aeson (decode)
import           Data.String (fromString)
import           Data.Maybe (fromJust)

-- Configuration Rules ---------------------------------------------------------
main :: IO ()
main = hakyllWith deployConfig $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*.theme" $ do
        route (setExtension "css")
        compile kdeSyntaxJsonToCss

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "about.html" $ do
        route   $ setExtension "html"
        compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


-- Auxiliary Values ------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

deployConfig :: Configuration
deployConfig = defaultConfiguration 
  {
    deployCommand = "./deploy.sh"
  }

-- Decode a KDE syntax highlighting json file into a Pandoc style, and then
-- convert it to CSS
kdeSyntaxJsonToCss :: Compiler (Item String)
kdeSyntaxJsonToCss
  = fmap (styleToCss . fromJust . decode . fromString) <$> getResourceString
