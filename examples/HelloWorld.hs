{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import System.Exit
  
import Control.Exception
import Control.Monad

import Graphics.Win32
import System.Win32.DLL (getModuleHandle)

main :: IO ()
main = do
  hWnd <- createDefaultWindow 600 600 wndProc
  messagePump hWnd

wndProc hwnd wmsg wParam lParam = defWindowProc (Just hwnd) (wmsg) (wParam) (lParam)

createDefaultWindow :: Int -> Int -> WindowClosure -> IO HWND
createDefaultWindow width height wndProc = do
  let winClass = mkClassName "HelloWorld"
  icon         <- loadIcon   Nothing iDI_APPLICATION
  cursor       <- loadCursor Nothing iDC_ARROW
  bgBrush      <- createSolidBrush (rgb 255 255 255)
  mainInstance <- getModuleHandle Nothing
  registerClass
    ( cS_VREDRAW + cS_HREDRAW
    , mainInstance
    , Just icon
    , Just cursor
    , Just bgBrush
    , Nothing
    , winClass )
  w <- createWindow
       winClass
       "Hello, World"
       wS_OVERLAPPEDWINDOW
       Nothing Nothing
       (Just width)
       (Just height)
       Nothing
       Nothing
       mainInstance
       wndProc
  showWindow w sW_SHOWNORMAL
  updateWindow w
  return w
  
messagePump :: HWND -> IO ()
messagePump hwnd = Graphics.Win32.allocaMessage $ \ msg ->
  let pump = do
        getMessage msg (Just hwnd) `catch` \ (_::SomeException) -> exitWith ExitSuccess
        translateMessage msg
        dispatchMessage msg
        pump
  in pump