module Graphics.D3D11Binding.Interface.D3D11Resource where
import Data.Word

import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Ptr

import Graphics.Win32
import Graphics.D3D11Binding.Types
import Graphics.D3D11Binding.Interface.Unknown

foreign import stdcall "SetEvictionPriority" c_setEvictionPriority
  :: Ptr ID3D11Resource -> Word32 -> IO ()

class (UnknownInterface interface) => D3D11ResourceInterface interface where
  setEvictionPriority :: Ptr interface -> Word32 -> IO ()
  setEvictionPriority ptr w = c_setEvictionPriority (castPtr ptr) w
  
data ID3D11Resource = ID3D11Resource
instance UnknownInterface ID3D11Resource
instance D3D11ResourceInterface ID3D11Resource