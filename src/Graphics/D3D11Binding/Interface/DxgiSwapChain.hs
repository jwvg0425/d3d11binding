module Graphics.D3D11Binding.Interface.DxgiSwapChain where
import Data.Word

import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Ptr

import Graphics.Win32
import Graphics.D3D11Binding.GUID
import Graphics.D3D11Binding.Types

import Graphics.D3D11Binding.Interface.Unknown

foreign import stdcall "Present" c_present 
  :: Ptr IDxgiSwapChain -> Word32 -> Word32 -> IO HRESULT

foreign import stdcall "GetBuffer" c_getBuffer
  :: Ptr IDxgiSwapChain -> Word32 -> Ptr GUID -> Ptr (Ptr ()) -> IO HRESULT

class (UnknownInterface interface) => DxgiSwapChainInterface interface where
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

data IDxgiSwapChain = IDxgiSwapChain
instance UnknownInterface IDxgiSwapChain
instance DxgiSwapChainInterface IDxgiSwapChain