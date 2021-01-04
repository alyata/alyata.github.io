--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson               (decode)
import           Data.Maybe               (fromJust)
import           Data.Monoid              (mappend)
import           Data.String              (fromString)

import           Text.Pandoc.Highlighting (styleToCss)
import           Text.Pandoc.Options

import           Hakyll

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
        compile $ pandocMathCompiler
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

-- Pandoc compiler but with MathJax support
-- modified from:
-- http://travis.athougies.net/posts/2013-08-13-using-math-on-your-hakyll-blog.html
pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler =
    let mathExtensions = extensionsFromList [Ext_tex_math_dollars,
                                             Ext_latex_macros]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions = defaultExtensions <> mathExtensions
        writerOptions = defaultHakyllWriterOptions {
                          writerExtensions = newExtensions,
                          writerHTMLMathMethod = MathJax ""
                        }
    in pandocCompilerWith defaultHakyllReaderOptions writerOptions

-- Decode a KDE syntax highlighting json file into a Pandoc style, and then
-- convert it to CSS
kdeSyntaxJsonToCss :: Compiler (Item String)
kdeSyntaxJsonToCss
  = fmap (styleToCss . fromJust . decode . fromString) <$> getResourceString
