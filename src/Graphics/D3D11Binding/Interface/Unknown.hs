module Graphics.D3D11Binding.Interface.Unknown where
import Data.Word

import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Ptr

import Graphics.Win32
import Graphics.D3D11Binding.Types

foreign import stdcall "Release" c_release
 :: Ptr IUnknown -> IO Word32

class UnknownInterface interface where
  release :: Ptr interface -> IO Word32
  release ptr = c_release (castPtr ptr)
  use :: Ptr interface -> (Ptr interface -> IO result) -> IO result
  use i f = do
    res <- f i
    release i
    return res
    
data IUnknown = IUnknown
instance UnknownInterface IUnknown