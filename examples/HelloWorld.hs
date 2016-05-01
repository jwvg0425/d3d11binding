{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import System.Exit
  
import Control.Exception
import Control.Monad

import Foreign.Ptr

import Graphics.Win32
import System.Win32.DLL (getModuleHandle)

import Graphics.D3D11Binding

main :: IO ()
main = do
  hWnd <- createDefaultWindow 800 600 wndProc
  messagePump hWnd

wndProc hwnd wmsg wParam lParam = defWindowProc (Just hwnd) wmsg wParam lParam

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
  
  let bd = DxgiModeDesc 
            800 
            600 
            (DxgiRational 60 1) 
            DxgiFormatR8G8B8A8Unorm 
            DxgiModeScanlineOrderUnspecified 
            DxgiModeScalingUnspecified
  
  let sd = DxgiSwapChainDesc
            bd
            (DxgiSampleDesc 1 0)
            dxgiUsageRenderTargetOutput
            1
            w
            True
            DxgiSwapEffectDiscard
            0
  
  Right (swapChain, device, featureLevel, deviceContext) <- d3d11CreateDeviceAndSwapChain
          nullPtr 
          D3DDriverTypeHardware
          nullPtr
          [D3D11CreateDeviceDebug]
          [D3DFeatureLevel11_0, D3DFeatureLevel10_1, D3DFeatureLevel10_0]
          sd
  
  Right (backBuffer :: Ptr ID3D11Texture2D) <- getBuffer swapChain (fromIntegral 0)
  Right renderTargetView <- use backBuffer $ \b -> createRenderTargetView device b Nothing
  
  omSetRenderTargets deviceContext [renderTargetView] nullPtr
  rsSetViewports deviceContext [D3D11Viewport 0 0 800 600 0 1]
  
  showWindow w sW_SHOWNORMAL
  updateWindow w
  return w
    
messagePump :: HWND -> IO ()
messagePump hwnd = Graphics.Win32.allocaMessage $ \ msg ->
  let pump = do
        getMessage msg (Just hwnd) `catch` \ (_::SomeException) -> exitSuccess
        translateMessage msg
        dispatchMessage msg
        pump
  in pump