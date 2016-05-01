module Graphics.D3D11Binding.Interface where

import Data.Word

import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Ptr

import Graphics.Win32
import Graphics.D3D11Binding.GUID
import Graphics.D3D11Binding.Types
  
foreign import stdcall "Present" c_present 
  :: Ptr IDxgiSwapChain -> Word32 -> Word32 -> IO HRESULT

foreign import stdcall "GetBuffer" c_getBuffer
  :: Ptr IDxgiSwapChain -> Word32 -> Ptr GUID -> Ptr (Ptr ()) -> IO HRESULT

class DxgiSwapChainInterface interface where
  present :: Ptr interface -> Word32 -> Word32 -> IO HRESULT 
  present this syncInterval flags = c_present (castPtr this) syncInterval flags
  getBuffer :: (HasGUID surface) => Ptr interface -> Word32 -> IO (Either HRESULT (Ptr surface))
  getBuffer this buffer = alloca $ \surface -> alloca $ \pGuid -> do
    let guid = getGUID surface
    poke pGuid guid
    hr <- c_getBuffer (castPtr this) buffer pGuid (castPtr surface)
    if hr < 0 then return (Left hr)
    else do
      peekSurface <- peek surface
      return (Right peekSurface)

foreign import stdcall "SetEvictionPriority" c_setEvictionPriority
  :: Ptr ID3D11Resource -> Word32 -> IO ()

class D3D11ResourceInterface interface where
  setEvictionPriority :: Ptr interface -> Word32 -> IO ()
  setEvictionPriority ptr w = c_setEvictionPriority (castPtr ptr) w

foreign import stdcall "CreateRenderTargetView" c_createRenderTargetView
  :: Ptr ID3D11Device -> Ptr ID3D11Resource -> Ptr D3D11RenderTargetViewDesc -> Ptr (Ptr ID3D11RenderTargetView) -> IO HRESULT
  
class D3D11DeviceInterface interface where
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

data ID3D11Texture2D = ID3D11Texture2D

instance HasGUID ID3D11Texture2D where
  getGUID _ = GUID 0x6f15aaf2 0xd208 0x4e89 [0x9a,0xb4,0x48,0x95,0x35,0xd3,0x4f,0x9c]

instance D3D11ResourceInterface ID3D11Texture2D

data IDxgiSwapChain = IDxgiSwapChain

instance DxgiSwapChainInterface IDxgiSwapChain

data ID3D11Device = ID3D11Device

instance D3D11DeviceInterface ID3D11Device

data ID3D11DeviceContext = ID3D11DeviceContext
data ID3D11RenderTargetView = ID3D11RenderTargetView
data IDxgiAdapter = IDxgiAdapter
data ID3D11Resource = ID3D11Resource