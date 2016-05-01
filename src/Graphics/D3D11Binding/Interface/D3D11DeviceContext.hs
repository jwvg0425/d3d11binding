module Graphics.D3D11Binding.Interface.D3D11DeviceContext where
import Data.Word

import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Ptr

import Graphics.Win32
import Graphics.D3D11Binding.Types
import Graphics.D3D11Binding.Interface.Unknown
import Graphics.D3D11Binding.Interface.D3D11RenderTargetView
import Graphics.D3D11Binding.Interface.D3D11DepthStencilView

foreign import stdcall "OMSetRenderTargets" c_omSetRenderTargets
  :: Ptr ID3D11DeviceContext -> Word32 -> Ptr (Ptr ID3D11RenderTargetView) -> Ptr ID3D11DepthStencilView -> IO ()

class (UnknownInterface interface) => D3D11DeviceContextInterface interface where
  omSetRenderTargets :: Ptr interface -> Word32 -> Ptr ID3D11RenderTargetView -> Ptr ID3D11DepthStencilView -> IO ()
  omSetRenderTargets ptr numViews renderTargetView depthStencilView = alloca $ \pRenderTargetView -> do
    poke pRenderTargetView renderTargetView
    c_omSetRenderTargets (castPtr ptr) numViews pRenderTargetView depthStencilView

data ID3D11DeviceContext = ID3D11DeviceContext

instance UnknownInterface ID3D11DeviceContext
instance D3D11DeviceContextInterface ID3D11DeviceContext