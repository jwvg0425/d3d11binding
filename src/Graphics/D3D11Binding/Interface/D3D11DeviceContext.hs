module Graphics.D3D11Binding.Interface.D3D11DeviceContext where
import Data.Word

import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Ptr

import Graphics.Win32
import Graphics.D3D11Binding.Types
import Graphics.D3D11Binding.Interface.Unknown

data ID3D11DeviceContext = ID3D11DeviceContext