module Graphics.D3D11Binding.Types where
import Data.Word

import Graphics.Win32

import Graphics.D3D11Binding.Enums

type DxgiUsage = Word32

data DxgiSwapChainDesc = DxgiSwapChainDesc
  { bufferDesc :: DxgiModeDesc
  , sampleDesc :: DxgiSampleDesc
  , bufferUsage :: DxgiUsage
  , bufferCount :: Word32
  , outputWindow :: HWND
  , windowed :: Bool
  , swapEffect :: DxgiSwapEffect
  , flags :: Word32 }

data DxgiModeDesc = DxgiModeDesc
  { width :: Word32
  , height :: Word32
  , refreshRate :: DxgiRational
  , format :: DxgiFormat
  , scanlineOrdering :: DxgiModeScanlineOrder
  , scaling :: DxgiModeScaling }

data DxgiRational = DxgiRational
  { numerator :: Word32
  , denominator :: Word32 }

data DxgiSampleDesc = DxgiSampleDesc
  { count :: Word32
  , quality :: Word32 }