{-# LANGUAGE DeriveGeneric #-}
module Graphics.D3D11Binding.Types where
import GHC.Generics (Generic)
import Data.Word
import Data.Bits
import Control.Applicative
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
  , scanlineOrdering :: DxgiModeScanlineOrder
  , scaling :: DxgiModeScaling }
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