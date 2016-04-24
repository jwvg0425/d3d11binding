{-# LANGUAGE DeriveGeneric #-}
module Graphics.D3D11Binding.Types where
import GHC.Generics (Generic)
import Data.Word
import Control.Applicative
import Foreign.Storable
import Foreign.CStorable

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
  , format :: DxgiFormat
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