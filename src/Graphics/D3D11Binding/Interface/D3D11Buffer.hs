module Graphics.D3D11Binding.Interface.D3D11Buffer where

import Graphics.D3D11Binding.Interface.Unknown
import Graphics.D3D11Binding.Interface.D3D11Resource

data ID3D11Buffer = ID3D11Buffer

instance UnknownInterface ID3D11Buffer
instance D3D11ResourceInterface ID3D11Buffer