{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import System.Exit
  
import Data.Int

import Control.Exception
import Control.Monad

import Foreign (peekByteOff)
import Foreign.Ptr

import Graphics.Win32
import System.Win32.DLL (getModuleHandle)

import Graphics.D3D11Binding

foreign import stdcall "PostQuitMessage" postQuitMessage :: Int32 -> IO ()

main :: IO ()
main = do
  hWnd <- createDefaultWindow 800 600 wndProc
  (s, d, dc, r) <- initDevice hWnd
  use s $ \swapChain -> use d $ \device -> use dc $ \deviceContext -> use r $ \renderTargetView -> do
    messagePump hWnd deviceContext swapChain renderTargetView
  
wndProc :: WindowClosure
wndProc hWnd msg wParam lParam
  | msg == wM_DESTROY = postQuitMessage 0 >> return 0
  | otherwise = defWindowProc (Just hWnd) msg wParam lParam

initDevice :: HWND -> IO (Ptr IDxgiSwapChain, Ptr ID3D11Device, Ptr ID3D11DeviceContext, Ptr ID3D11RenderTargetView)
initDevice hWnd = do
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
            hWnd
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
  
  return (swapChain, device, deviceContext, renderTargetView)

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

pM_NOREMOVE, pM_REMOVE, pM_NOYIELD :: UINT
pM_NOREMOVE = 0x0000
pM_REMOVE = 0x0001
pM_NOYIELD = 0x0002

messagePump :: HWND -> Ptr ID3D11DeviceContext -> Ptr IDxgiSwapChain -> Ptr ID3D11RenderTargetView -> IO ()
messagePump hwnd deviceContext swapChain renderTargetView = Graphics.Win32.allocaMessage $ \ msg ->
  let pump = do
        m <- peekByteOff msg 4 :: IO WindowMessage
        when (m /= wM_QUIT) $ do
          r <- c_PeekMessage msg (maybePtr Nothing) 0 0 pM_REMOVE
          if r /= 0
          then do
            translateMessage msg
            dispatchMessage msg
            return ()
          else do
            render deviceContext swapChain renderTargetView
          pump
  in pump
  
render :: Ptr ID3D11DeviceContext -> Ptr IDxgiSwapChain -> Ptr ID3D11RenderTargetView -> IO ()
render deviceContext swapChain renderTargetView = do
  clearRenderTargetView deviceContext renderTargetView $ Color 0.0 0.125 0.3 1.0
  present swapChain 0 0
  return ()