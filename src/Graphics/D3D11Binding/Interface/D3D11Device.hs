module Graphics.D3D11Binding.Interface.D3D11Device where
import Data.Word

import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Ptr

import Graphics.Win32
import Graphics.D3D11Binding.Types
import Graphics.D3D11Binding.Interface.Unknown
import Graphics.D3D11Binding.Interface.D3D11Resource
import Graphics.D3D11Binding.Interface.D3D11RenderTargetView

foreign import stdcall "CreateRenderTargetView" c_createRenderTargetView
  :: Ptr ID3D11Device -> Ptr ID3D11Resource -> Ptr D3D11RenderTargetViewDesc -> Ptr (Ptr ID3D11RenderTargetView) -> IO HRESULT
  
class (UnknownInterface interface) => D3D11DeviceInterface interface where
  createRenderTargetView 
    :: (D3D11ResourceInterface resource) => 
       Ptr interface -> Ptr resource -> Maybe (D3D11RenderTargetViewDesc) -> IO (Either HRESULT (Ptr ID3D11RenderTargetView))
  createRenderTargetView this pResource Nothing = alloca $ \renderTargetView -> do
    hr <- c_createRenderTargetView (castPtr this) (castPtr pResource) nullPtr renderTargetView
    if hr < 0 then return (Left hr) else Right <$> peek renderTargetView
  createRenderTargetView this pResource (Just desc) = alloca $ \renderTargetView -> alloca $ \pDesc -> do
    poke pDesc desc
    hr <- c_createRenderTargetView (castPtr this) (castPtr pResource) pDesc renderTargetView
    if hr < 0 then return (Left hr) else Right <$> peek renderTargetView
      
data ID3D11Device = ID3D11Device

instance UnknownInterface ID3D11Device
instance D3D11DeviceInterface ID3D11Device