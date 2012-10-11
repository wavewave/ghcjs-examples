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
  str1 <- jsstringcreatewithutf8cstring " 3 + 4 + 5 " 
  str2 <- jsstringcreatewithutf8cstring " 3 + 4 + 6 "
  b1 <- jsstringisequal str1 str2   
  result <- jsevaluatescript gctxt str1 nullPtr nullPtr 1 nullPtr
  b2 <- jsvalueisundefined gctxt result
  b3 <- jsvalueisnumber gctxt result 
  b4 <- jsvalueisstring gctxt result 
  b5 <- jsvalueisobject gctxt result 
  typ <- jsvaluegettype gctxt result 
  num <- jsvaluetonumber gctxt result nullPtr 
  -- l <- jsstringgetlength str 

  putStrLn $ show (b1,b2,b3,b4,b5)
  print $ typ 
  print $ num 
  putStrLn "done"



