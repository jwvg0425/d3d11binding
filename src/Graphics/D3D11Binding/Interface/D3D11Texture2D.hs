module Graphics.D3D11Binding.Interface.D3D11Texture2D where
  
import Foreign.Ptr

import Graphics.D3D11Binding.Types
import Graphics.D3D11Binding.GUID
import Graphics.D3D11Binding.Interface.Unknown
import Graphics.D3D11Binding.Interface.D3D11Resource
  
data ID3D11Texture2D = ID3D11Texture2D

instance HasGUID ID3D11Texture2D where
  getGUID _ = GUID 0x6f15aaf2 0xd208 0x4e89 [0x9a,0xb4,0x48,0x95,0x35,0xd3,0x4f,0x9c]

instance UnknownInterface ID3D11Texture2D
instance D3D11ResourceInterface ID3D11Texture2D