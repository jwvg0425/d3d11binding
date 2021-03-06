{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.D3D11Binding.Types where
import GHC.Generics (Generic)
import Data.Word
import Data.Bits
import Control.Applicative

import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.CStorable

import Graphics.Win32

import Graphics.D3D11Binding.Enums

type DxgiUsage = Word32

dxgiUsageShaderInput :: DxgiUsage
dxgiUsageShaderInput = shift 1 4

dxgiUsageRenderTargetOutput :: DxgiUsage
dxgiUsageRenderTargetOutput = shift 1 5

dxgiUsageBackBuffer :: DxgiUsage
dxgiUsageBackBuffer = shift 1 6

dxgiUsageShared :: DxgiUsage
dxgiUsageShared = shift 1 7

dxgiUsageReadOnly :: DxgiUsage
dxgiUsageReadOnly = shift 1 8

dxgiUsageDiscardOnPresent :: DxgiUsage
dxgiUsageDiscardOnPresent = shift 1 9

dxgiUsageUnorderedAccess :: DxgiUsage
dxgiUsageUnorderedAccess = shift 1 10

data DxgiSwapChainDesc = DxgiSwapChainDesc
  { bufferDesc :: DxgiModeDesc
  , sampleDesc :: DxgiSampleDesc
  , bufferUsage :: DxgiUsage
  , bufferCount :: Word32
  , outputWindow :: HWND
  , windowed :: Bool
  , swapEffect :: DxgiSwapEffect
  , flags :: Word32 }
  deriving (Generic)

instance CStorable DxgiSwapChainDesc  
instance Storable DxgiSwapChainDesc where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek

data DxgiModeDesc = DxgiModeDesc
  { width :: Word32
  , height :: Word32
  , refreshRate :: DxgiRational
  , dxgiModeFormat :: DxgiFormat
  , dxgiScanlineOrdering :: DxgiModeScanlineOrder
  , dxgiScaling :: DxgiModeScaling }
  deriving (Generic)
  
instance CStorable DxgiModeDesc  
instance Storable DxgiModeDesc where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek

data DxgiRational = DxgiRational
  { numerator :: Word32
  , denominator :: Word32 }
  deriving (Generic)

instance CStorable DxgiRational  
instance Storable DxgiRational where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek

data DxgiSampleDesc = DxgiSampleDesc
  { count :: Word32
  , quality :: Word32 }
  deriving (Generic)

instance CStorable DxgiSampleDesc 
instance Storable DxgiSampleDesc where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek
  
data Rtv = Rtv Word32 Word32 Word32 deriving (Generic)

bufferRtv :: Word32 -> Word32 -> Rtv
bufferRtv d1 d2 = Rtv d1 d2 (fromIntegral 0)

tex1dRtv :: Word32 -> Rtv
tex1dRtv d1 = Rtv d1 (fromIntegral 0) (fromIntegral 0)

tex1dArrayRtv :: Word32 -> Word32 -> Word32 -> Rtv
tex1dArrayRtv d1 d2 d3 = Rtv d1 d2 d3

tex2dRtv :: Word32 -> Rtv
tex2dRtv d1 = Rtv d1 (fromIntegral 0) (fromIntegral 0)

tex2dArrayRtv :: Word32 -> Word32 -> Word32 -> Rtv
tex2dArrayRtv d1 d2 d3 = Rtv d1 d2 d3

tex2dMsRtv :: Rtv
tex2dMsRtv = Rtv (fromIntegral 0) (fromIntegral 0) (fromIntegral 0)

tex2dMsArrayRtv :: Word32 -> Word32 -> Rtv
tex2dMsArrayRtv d1 d2 = Rtv d1 d2 (fromIntegral 0)

tex3dArrayRtv :: Word32 -> Word32 -> Word32 -> Rtv
tex3dArrayRtv d1 d2 d3 = Rtv d1 d2 d3

instance CStorable Rtv
instance Storable Rtv where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek
  
data D3D11RenderTargetViewDesc = D3D11RenderTargetViewDesc
  { renderTargetViewFormat :: DxgiFormat
  , viewDimension :: D3D11RtvDimension
  , rtv :: Rtv } deriving (Generic)

instance CStorable D3D11RenderTargetViewDesc
instance Storable D3D11RenderTargetViewDesc where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek
  
data D3D11Viewport = D3D11Viewport
  { topLeftX :: Float
  , topLeftY :: Float
  , viewportWidth :: Float
  , viewportHeight :: Float
  , minDepth :: Float
  , maxDepth :: Float } deriving (Generic)

instance CStorable D3D11Viewport
instance Storable D3D11Viewport where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek
  
data Color = Color 
  { red :: Float
  , green :: Float
  , blue :: Float
  , alpha :: Float } deriving (Generic)

instance CStorable Color
instance Storable Color where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek
  
data D3D11InputElementDesc = D3D11InputElementDesc
  { semanticName :: String
  , semanticIndex :: Word32
  , inputElementFormat :: DxgiFormat
  , inputSlot :: Word32
  , alignedByteOffset :: Word32
  , inputSlotClass :: D3D11InputClassification
  , instanceDataStepRate :: Word32 } deriving (Generic)
  
instance CStorable D3D11InputElementDesc where
  cSizeOf = sizeOf
  cAlignment = alignment
  cPoke = poke
  cPeek = peek

pokeCString ptr str = go ptr str 0
  where go ptr [] byte = pokeByteOff ptr byte '\0'
        go ptr (x:xs) byte = do
          pokeByteOff ptr byte x
          go ptr xs (byte+1) 

instance Storable D3D11InputElementDesc where
  sizeOf _ = 32
  alignment _ = 8
  poke ptr desc = do
    -- TODO : free namePtr
    namePtr <- newCString (semanticName desc)
    pokeByteOff ptr 0 namePtr
    pokeByteOff ptr 8 (semanticIndex desc)
    pokeByteOff ptr 12 (inputElementFormat desc)
    pokeByteOff ptr 16 (inputSlot desc)
    pokeByteOff ptr 20 (alignedByteOffset desc)
    pokeByteOff ptr 24 (inputSlotClass desc)
    pokeByteOff ptr 28 (instanceDataStepRate desc)
  peek ptr = do
    namePtr <- peekByteOff ptr 0
    name <- peekCString namePtr
    index <- peekByteOff ptr 8
    elementFormat <- peekByteOff ptr 12
    slot <- peekByteOff ptr 16
    byteOffset <- peekByteOff ptr 20
    slotClass <- peekByteOff ptr 24
    dataStepRate <- peekByteOff ptr 28
    return $ D3D11InputElementDesc name index elementFormat slot byteOffset slotClass dataStepRate

data D3D11SubresourceData = D3D11SubresourceData
  { pSysMem :: Ptr ()
  , sysMemPitch :: Word32
  , sysMemSlicePitch :: Word32 } deriving (Generic)

instance CStorable D3D11SubresourceData
instance Storable D3D11SubresourceData where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek

class (Storable dataType) => HasSubresourceData dataType where
  getSubresourceData :: [dataType] -> IO D3D11SubresourceData
  getSubresourceData [] = return $ D3D11SubresourceData nullPtr (fromIntegral 0) (fromIntegral 0)
  getSubresourceData dat = alloca $ \pData -> do
    pokeArray pData dat
    return $ D3D11SubresourceData (castPtr pData) (fromIntegral 0) (fromIntegral 0)

data D3D11BufferDesc = D3D11BufferDesc
  { byteWidth :: Word32
  , usage :: D3D11Usage
  , bindFlags :: Word32
  , cpuAccessFlags :: Word32
  , miscFlags :: Word32
  , structureByteStride :: Word32 } deriving (Generic)
  
instance CStorable D3D11BufferDesc
instance Storable D3D11BufferDesc where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek

data D3D11Box = D3D11Box
  { left :: Word32
  , top :: Word32
  , front :: Word32
  , right :: Word32
  , bottom :: Word32
  , back :: Word32 } deriving (Generic)

instance CStorable D3D11Box
instance Storable D3D11Box where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek
  
data D3D11Texture2DDesc = D3D11Texture2DDesc
  { textureWidth :: Word32
  , textureHeight :: Word32
  , mipLevels :: Word32
  , arraySize :: Word32
  , textureFormat :: DxgiFormat
  , textureSampleDesc :: DxgiSampleDesc
  , textureUsage :: D3D11Usage
  , textureBindFlags :: Word32
  , textureCPUAccessFlags :: Word32
  , textureMiscFlags :: Word32 } deriving (Generic)
  
instance CStorable D3D11Texture2DDesc
instance Storable D3D11Texture2DDesc where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek
  
data D3D11DepthStencilViewDesc = D3D11DepthStencilViewDesc
  { dsvFormat :: DxgiFormat
  , dsvViewDimension :: D3D11DsvDimension
  , dsvFlags :: Word32
  , dsv :: Dsv } deriving (Generic)

instance CStorable D3D11DepthStencilViewDesc
instance Storable D3D11DepthStencilViewDesc where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek

data Dsv = Dsv Word32 Word32 Word32 deriving (Generic)

instance CStorable Dsv
instance Storable Dsv where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek

tex1dDsv :: Word32 -> Dsv
tex1dDsv mipSlice = Dsv mipSlice (fromIntegral 0) (fromIntegral 0)

tex1dArrayDsv :: Word32 -> Word32 -> Word32 -> Dsv
tex1dArrayDsv mipSlice firstArraySlice arraySize = Dsv mipSlice firstArraySlice arraySize

tex2dDsv :: Word32 -> Dsv
tex2dDsv mipSlice = Dsv mipSlice (fromIntegral 0) (fromIntegral 0)

tex2dArrayDsv :: Word32 -> Word32 -> Word32 -> Dsv
tex2dArrayDsv mipSlice firstArraySlice arraySize = Dsv mipSlice firstArraySlice arraySize

tex2dMsDsv :: Dsv
tex2dMsDsv = Dsv (fromIntegral 0) (fromIntegral 0) (fromIntegral 0)

tex2dMsArrayDsv :: Word32 -> Word32 -> Dsv
tex2dMsArrayDsv firstArraySlice arraySize = Dsv firstArraySlice arraySize (fromIntegral 0)

data D3D11SamplerDesc = D3D11SamplerDesc
  { samplerFilter :: D3D11Filter
  , samplerAddressU :: D3D11TextureAddressMode
  , samplerAddressV :: D3D11TextureAddressMode
  , samplerAddressW :: D3D11TextureAddressMode
  , samplerMipLODBias :: Float
  , samplerMaxAnisotropy :: Word32
  , samplerComparisonFunc :: D3D11ComparisonFunc
  , samplerBorderColor :: Color
  , samplerMinLOD :: Float
  , samplerMaxLOD :: Float
  } deriving (Generic)
  
instance CStorable D3D11SamplerDesc
instance Storable D3D11SamplerDesc where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek
  
d3d11Float32Max :: Float
d3d11Float32Max = 3402823466e+38