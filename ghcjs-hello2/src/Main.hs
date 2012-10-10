{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Main (
    main
) where

import Control.Monad.Trans ( liftIO )
import System.IO (stdout, hFlush)
import Graphics.UI.Gtk.WebKit.GHCJS (runWebGUI)
import Graphics.UI.Gtk.WebKit.WebView (webViewGetDomDocument)
import Graphics.UI.Gtk.WebKit.DOM.Document
       (documentCreateElement, documentGetElementById, documentGetBody)
import Graphics.UI.Gtk.WebKit.DOM.HTMLElement
       (htmlElementSetInnerHTML)
import Data.Text.Lazy (Text, unpack)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet (shamlet)
import Text.Blaze.Html (Html)
import Graphics.UI.Gtk.WebKit.Types
       (castToHTMLElement, castToHTMLDivElement, castToHTMLInputElement)
import Control.Applicative ((<$>))
import Graphics.UI.Gtk.WebKit.DOM.Element
       (elementGetStyle, elementSetAttribute, elementOnclick,
        elementOnkeypress, elementOnkeyup, elementOnkeydown, elementFocus)
import Graphics.UI.Gtk.WebKit.DOM.HTMLInputElement
       (htmlInputElementGetValue)
import Control.Concurrent
       (tryTakeMVar, takeMVar, threadDelay, putMVar, forkIO, newEmptyMVar)
import Control.Monad (when, forever)
import Graphics.UI.Gtk.WebKit.DOM.EventM
       (mouseShiftKey, mouseCtrlKey)
import Graphics.UI.Gtk.WebKit.DOM.Node (nodeAppendChild)
import Graphics.UI.Gtk.WebKit.DOM.CSSStyleDeclaration
       (cssStyleDeclarationSetProperty)
       
import Graphics.UI.Gtk.WebKit.WebView        
import Graphics.UI.Gtk.WebKit.JavaScriptCore.WebFrame
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase 
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSStringRef
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSValueRef
import Foreign.Ptr 


main = do
  -- but just returns the browser window when compiled to JavaScript
  runWebGUI $ \ webView -> do

    -- WebKitGtk provides the normal W3C DOM functions
    doc <- webViewGetDomDocument webView
    Just body <- documentGetBody doc

    -- Lets use some Hamlet to replace HTerm with some HTML
    Just div <- fmap castToHTMLDivElement <$> documentCreateElement doc "div"
    htmlElementSetInnerHTML div . unpack $ renderHtml [shamlet|$newline always
      <body #slideBody>
        <h1 #heading>
            Hello and Welcome GHCJS
        <p>
            As you can see GHCJS apps can also use the DOM functions
          \ in the WebKitGtk Gtk2Hs package
        <p>
            Know any good prime numbers?
            <input #num size="8">
            <div #prime>
        <p>
            Thats it for our Hello World.  Here are some more fun GHCJS things to try
        <ul>
          <li>
            Check out the <a href="https://github.com/ghcjs/ghcjs-examples/blob/master/ghcjs-hello/src/Main.hs">Haskell source code</a>
            \ for this example.  (read it carefully to find the hidden FRP)
          <li>
            Try out the <a href="hterm.html">unminified version</a>
    |]
    nodeAppendChild body (Just div)

    Just heading  <- fmap castToHTMLElement      <$> documentGetElementById doc "heading"

    elementOnclick heading $ do
      shiftIsPressed <- mouseShiftKey
      when shiftIsPressed $ liftIO (shiftClickAction webView)
    return()

shiftClickAction :: WebViewClass self => self -> IO () 
shiftClickAction webview = do 
  putStrLn "key pressed"
  webframe <- webViewGetMainFrame webview 
  gctxt <- webFrameGetGlobalContext webframe 
  str <- jsstringcreatewithutf8cstring " { name : 'ian' } " 
  result <- jsevaluatescript gctxt nullPtr nullPtr nullPtr 1 nullPtr
  b <- jsvalueisundefined gctxt result
  putStrLn $ show b 
  putStrLn "done"
   {-
    -- We can get the elements by ID
    Just numInput <- fmap castToHTMLInputElement <$> documentGetElementById doc "num"
    Just prime    <- fmap castToHTMLDivElement   <$> documentGetElementById doc "prime"
    mbTerminal    <- fmap castToHTMLDivElement   <$> documentGetElementById doc "terminal"

    -- Set the input focus
    elementFocus numInput

    -- If we are in the browser let's shrink the terminal window to make room
    case mbTerminal of
      Just terminal -> do
        Just style <- elementGetStyle terminal
        cssStyleDeclarationSetProperty style "height" "200" ""
      _             -> return ()

    -- We don't want to work on more than on prime number test at a time.
    -- So we will have a single worker thread and a queue with just one value.
    next <- newEmptyMVar
    forkIO . forever $ do
      n <- takeMVar next
      htmlElementSetInnerHTML prime . unpack $ validatePrime n

    -- Something to set the next work item
    let setNext = do
                    n <- htmlInputElementGetValue numInput
                    tryTakeMVar next -- Discard existing next item
                    putMVar next n

    -- Lets wire up some events
    elementOnkeydown  numInput (liftIO setNext)
    elementOnkeyup    numInput (liftIO setNext)
    elementOnkeypress numInput (liftIO setNext)

    -- What is this?
    elementOnclick heading $ do
      shiftIsPressed <- mouseShiftKey
      when shiftIsPressed . liftIO $ lazyLoad_freecell doc body
    -}



{-
-- Integer uses goog.math.Integer compiled to Javascript
isPrime :: Integer -> Bool
isPrime p = p > 1 && (all (\n -> p `mod` n /= 0)
                     $ takeWhile (\n -> n*n <= p) [2..])

validatePrimeMessage :: Integer -> Html
validatePrimeMessage p | isPrime p = [shamlet|$newline always
                                        <b>Yes</b>, #{p} is a prime|]
                       | otherwise = [shamlet|$newline always
                                        <b>No</b>, #{p} is not a prime|]

validatePrime :: String -> Text
validatePrime s = renderHtml $
  case reads s of
    [(n, "")] -> validatePrimeMessage n
    _         -> [shamlet|$newline always
                    <b>No</b>, that is not a number|]

-- Sometimes you might have something that needs more JavaScript than everything else
-- you can tell the GHCJS linker to put its dependancies in a sparate file using
-- a lazyLoad_ prefix
{-# NOINLINE lazyLoad_freecell #-}
lazyLoad_freecell doc body = do
    htmlElementSetInnerHTML body $
      "<div style=\"position:relative;left:0px;top:0px;background-color:#e0d0ff;width:700px;height:500px\" "++
      "id=\"freecell\" draggable=\"false\"></div>"
    Just div <- fmap castToHTMLElement <$> documentGetElementById doc "freecell"
    unlisten <- engine doc div =<< mkFreecell
    -- Prevent finalizers running too soon
    forkIO $ forever (threadDelay 1000000000) >> unlisten
    return ()
-}