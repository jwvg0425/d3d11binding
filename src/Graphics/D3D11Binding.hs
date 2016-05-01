module Graphics.D3D11Binding 
( module Graphics.D3D11Binding.Enums
, module Graphics.D3D11Binding.Types
, module Graphics.D3D11Binding.Interface
, module Graphics.D3D11Binding.Shader
, module Graphics.D3D11Binding.GUID
, module Graphics.D3D11Binding.Utils
, d3d11CreateDeviceAndSwapChain
) where

import Data.Word

import Foreign.Ptr

import Graphics.Win32

import Graphics.D3D11Binding.Enums
import Graphics.D3D11Binding.Types
import Graphics.D3D11Binding.Interface
import Graphics.D3D11Binding.Shader
import Graphics.D3D11Binding.GUID
import Graphics.D3D11Binding.Utils

import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Storable

type PIDxgiAdapter = Ptr IDxgiAdapter
type PD3DFeatureLevel = Ptr D3DFeatureLevel
type PDxgiSwapChainDesc = Ptr DxgiSwapChainDesc
type PIDxgiSwapChain = Ptr IDxgiSwapChain
type PID3D11Device = Ptr ID3D11Device
type PID3D11DeviceContext = Ptr ID3D11DeviceContext

d3d11SdkVersion :: Word32
d3d11SdkVersion = 7

foreign import stdcall "D3D11CreateDeviceAndSwapChain" c_d3d11CreateDeviceAndSwapChain
  :: PIDxgiAdapter -> Word32 -> HMODULE -> Word32 ->
     PD3DFeatureLevel -> Word32 -> Word32 -> PDxgiSwapChainDesc ->
     Ptr PIDxgiSwapChain -> Ptr PID3D11Device -> Ptr D3DFeatureLevel ->
     Ptr PID3D11DeviceContext -> IO HRESULT
     
d3d11CreateDeviceAndSwapChain 
  :: PIDxgiAdapter -> D3DDriverType -> HMODULE -> 
     [D3D11CreateDeviceFlag] -> [D3DFeatureLevel] -> DxgiSwapChainDesc -> 
     IO (Either HRESULT (PIDxgiSwapChain, PID3D11Device, D3DFeatureLevel, PID3D11DeviceContext))
d3d11CreateDeviceAndSwapChain adapter driverType software flags featureLevels swapChainDesc = do
  featureArray <- newArray featureLevels
  let flagSum = createDeviceFlag flags
  alloca $ \swapChain -> alloca $ \device -> alloca $ \feature -> alloca $ \context -> alloca $ \sd -> do
    poke sd swapChainDesc
    hr <- c_d3d11CreateDeviceAndSwapChain
            adapter
            (fromIntegral $ fromEnum driverType)
            software
            flagSum
            featureArray
            (fromIntegral $ length featureLevels)
            d3d11SdkVersion
            sd
            swapChain
            device
            feature
            context
    if hr < 0 then return (Left hr) 
    else do
      peekSwapChain <- peek swapChain
      peekDevice <- peek device
      peekFeature <- peek feature
      peekContext <- peek context
      return $ Right (peekSwapChain, peekDevice, peekFeature, peekContext)