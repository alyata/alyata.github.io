--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson               (decode)
import           Data.Maybe               (fromJust)
import           Data.Monoid              (mappend)
import           Data.String              (fromString)
import           Data.Time.Format         (formatTime, defaultTimeLocale)
import           Data.Time.Clock          (getCurrentTime)

import           Text.Pandoc.Highlighting (styleToCss)
import           Text.Pandoc.Options

import           Hakyll

-- Configuration Rules ---------------------------------------------------------
main :: IO ()
main = do
  currentTime <- getCurrentTime
  hakyllWith deployConfig $ do
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

    create ["sitemap.xml"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let matchedPageIdentifiers = ["about.html"]
        matchedPages <- loadAll $ fromList ["about.html"]
        createdPages <- (loadAll $ fromList ["archive.html"] :: Compiler [Item String])
        let pages = posts <> matchedPages
            pagesCtx = dateField "dateSitemap" "%Y-%m-%dT%H:%M:%S%z"
                    <> postCtx
            createdCtx = defaultContext
                      <> constField "root" root
                      <> (constField "modified" $
                                     formatTime defaultTimeLocale
                                                "%Y-%m-%dT%H:%M:%S%z"
                                                currentTime)
            sitemapCtx = constField "root" root
                      <> listField "pages" pagesCtx (return pages)
                      <> listField "createdPages" createdCtx (return createdPages)
        makeItem "" >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

-- Auxiliary Values ------------------------------------------------------------

-- url of this website
root :: String
root = "https://alyata.github.io"

-- context values for a post
postCtx :: Context String
postCtx = defaultContext
       <> constField "root" root
       <> dateField "date" "%B %e, %Y"
       <> modificationTimeField "modified" "%Y-%m-%dT%H:%M:%S%z"

-- config to deploy website to master branch
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
