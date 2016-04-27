module Graphics.D3D11Binding.Interface where

import Data.Word

import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Ptr

import Graphics.Win32
import Graphics.D3D11Binding.GUID
  
foreign import stdcall "Present" c_present 
  :: Ptr interface -> Word32 -> Word32 -> IO HRESULT

foreign import stdcall "GetBuffer" c_getBuffer
  :: Ptr interface -> Word32 -> Ptr GUID -> Ptr (Ptr ()) -> IO HRESULT

class DxgiSwapChainInterface interface where
  present :: Ptr interface -> Word32 -> Word32 -> IO HRESULT 
  present = c_present
  getBuffer :: (HasGUID surface) => Ptr interface -> Word32 -> IO (Either HRESULT (Ptr surface))
  getBuffer this buffer = alloca $ \surface -> alloca $ \pGuid -> do
    let guid = getGUID surface
    poke pGuid guid
    hr <- c_getBuffer this buffer pGuid (castPtr surface)
    if hr < 0 then return (Left hr)
    else do
      peekSurface <- peek surface
      return (Right peekSurface)
    

data ID3D11Texture2D = ID3D11Texture2D

instance HasGUID ID3D11Texture2D where
  getGUID _ = GUID 0x6f15aaf2 0xd208 0x4e89 [0x9a,0xb4,0x48,0x95,0x35,0xd3,0x4f,0x9c]

data IDxgiSwapChain = IDxgiSwapChain

instance DxgiSwapChainInterface IDxgiSwapChain

data ID3D11Device = ID3D11Device
data ID3D11DeviceContext = ID3D11DeviceContext
data ID3D11RenderTargetView = ID3D11RenderTargetView
data IDxgiAdapter = IDxgiAdapter