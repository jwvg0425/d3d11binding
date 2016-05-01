module Graphics.D3D11Binding.Interface.D3D11DeviceContext where
import Data.Word

import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr

import Graphics.Win32
import Graphics.D3D11Binding.Types
import Graphics.D3D11Binding.Interface.Unknown
import Graphics.D3D11Binding.Interface.D3D11RenderTargetView
import Graphics.D3D11Binding.Interface.D3D11DepthStencilView

foreign import stdcall "OMSetRenderTargets" c_omSetRenderTargets
  :: Ptr ID3D11DeviceContext -> Word32 -> Ptr (Ptr ID3D11RenderTargetView) -> Ptr ID3D11DepthStencilView -> IO ()

foreign import stdcall "RSSetViewports" c_rsSetViewports
  :: Ptr ID3D11DeviceContext -> Word32 -> Ptr D3D11Viewport -> IO ()

foreign import stdcall "ClearRenderTargetView" c_clearRenderTargetView
  :: Ptr ID3D11DeviceContext -> Ptr ID3D11RenderTargetView -> Ptr Float -> IO ()

class (UnknownInterface interface) => D3D11DeviceContextInterface interface where
  omSetRenderTargets :: Ptr interface -> [Ptr ID3D11RenderTargetView] -> Ptr ID3D11DepthStencilView -> IO ()
  omSetRenderTargets ptr renderTargetViews depthStencilView = alloca $ \pRenderTargetViews -> do
    pokeArray pRenderTargetViews renderTargetViews
    c_omSetRenderTargets (castPtr ptr) (fromIntegral $ length renderTargetViews) pRenderTargetViews depthStencilView
  rsSetViewports :: Ptr interface -> [D3D11Viewport] -> IO ()
  rsSetViewports ptr viewports = alloca $ \pViewports -> do
    pokeArray pViewports viewports
    c_rsSetViewports (castPtr ptr) (fromIntegral $ length viewports) pViewports
  clearRenderTargetView :: Ptr interface -> Ptr ID3D11RenderTargetView -> Color -> IO ()
  clearRenderTargetView ptr renderTargetView color = alloca $ \pColor -> do
    poke pColor color
    c_clearRenderTargetView (castPtr ptr) renderTargetView (castPtr pColor)

data ID3D11DeviceContext = ID3D11DeviceContext

instance UnknownInterface ID3D11DeviceContext
instance D3D11DeviceContextInterface ID3D11DeviceContext