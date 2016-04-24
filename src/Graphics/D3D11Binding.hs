module Graphics.D3D11Binding 
( module Graphics.D3D11Binding.Enums
, module Graphics.D3D11Binding.Types
) where

import Data.Word

import Foreign.Ptr

import Graphics.Win32

import Graphics.D3D11Binding.Enums
import Graphics.D3D11Binding.Types

foreign import stdcall "D3D11CreateDeviceAndSwapChain" d3d11CreateDeviceAndSwapChain
  :: Ptr IDXGIAdapter -> HMODULE -> Word32 -> Word32 ->
     Ptr D3DFeatureLevel -> Word32 -> Word32 -> Ptr DxgiSwapChainDesc ->
     Ptr (Ptr IDXGISwapChain) -> Ptr (Ptr ID3D11Device) -> Ptr (D3DFeatureLevel) ->
     Ptr (Ptr ID3D11DeviceContext) -> IO HRESULT