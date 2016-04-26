module Graphics.D3D11Binding.Enums where

import Data.Int
import Data.Word
import Data.Bits
import Foreign.Ptr
import Foreign.Storable
import Foreign.CStorable

data D3DDriverType = D3DDriverTypeUnknown
                   | D3DDriverTypeHardware
                   | D3DDriverTypeReference
                   | D3DDriverTypeNull
                   | D3DDriverTypeSoftware
                   | D3DDriverTypeWarp 
                   deriving (Eq,Show)

instance Enum D3DDriverType where
  fromEnum D3DDriverTypeUnknown = 0
  fromEnum D3DDriverTypeHardware = 1
  fromEnum D3DDriverTypeReference = 2
  fromEnum D3DDriverTypeNull = 3
  fromEnum D3DDriverTypeSoftware = 4
  fromEnum D3DDriverTypeWarp = 5
  toEnum 0 = D3DDriverTypeUnknown
  toEnum 1 = D3DDriverTypeHardware
  toEnum 2 = D3DDriverTypeReference
  toEnum 3 = D3DDriverTypeNull
  toEnum 4 = D3DDriverTypeSoftware
  toEnum 5 = D3DDriverTypeWarp
  toEnum unmatched = error ("D3DDriverType.toEnum: cannot match " ++ show unmatched)

instance Storable D3DDriverType where
  sizeOf e = sizeOf ((fromIntegral $ fromEnum e) :: Int32)
  alignment e = alignment ((fromIntegral $ fromEnum e) :: Int32)
  peek ptr = peekByteOff (castPtr ptr :: Ptr Int32) 0 >>= (return . toEnum)
  poke ptr val = pokeByteOff (castPtr ptr :: Ptr Int32) 0 (fromEnum val)
  
instance CStorable D3DDriverType where
  cSizeOf = sizeOf
  cAlignment = alignment
  cPeek = peek
  cPoke = poke

data D3DFeatureLevel = D3DFeatureLevel9_1
                     | D3DFeatureLevel9_2
                     | D3DFeatureLevel9_3
                     | D3DFeatureLevel10_0
                     | D3DFeatureLevel10_1
                     | D3DFeatureLevel11_0
                     deriving (Eq, Show)
                     
instance Enum D3DFeatureLevel where
  fromEnum D3DFeatureLevel9_1 = 0x9100
  fromEnum D3DFeatureLevel9_2 = 0x9200
  fromEnum D3DFeatureLevel9_3 = 0x9300
  fromEnum D3DFeatureLevel10_0 = 0xa000
  fromEnum D3DFeatureLevel10_1 = 0xa100
  fromEnum D3DFeatureLevel11_0 = 0xb000
  toEnum 0x9100 = D3DFeatureLevel9_1
  toEnum 0x9200 = D3DFeatureLevel9_2
  toEnum 0x9300 = D3DFeatureLevel9_3
  toEnum 0xa000 = D3DFeatureLevel10_0
  toEnum 0xa100 = D3DFeatureLevel10_1
  toEnum 0xb000 = D3DFeatureLevel11_0
  toEnum unmatched = error ("D3DFeatureLevel.toEnum: cannot match " ++ show unmatched)
  
instance Storable D3DFeatureLevel where
  sizeOf e = sizeOf ((fromIntegral $ fromEnum e) :: Int32)
  alignment e = alignment ((fromIntegral $ fromEnum e) :: Int32)
  peek ptr = peekByteOff (castPtr ptr :: Ptr Int32) 0 >>= (return . toEnum)
  poke ptr val = pokeByteOff (castPtr ptr :: Ptr Int32) 0 (fromEnum val)
  
instance CStorable D3DFeatureLevel where
  cSizeOf = sizeOf
  cAlignment = alignment
  cPeek = peek
  cPoke = poke
  
data DxgiFormat = DxgiFormatUnknown
                | DxgiFormatR32G32B32A32Typeless
                | DxgiFormatR32G32B32A32Float
                | DxgiFormatR32G32B32A32Uint
                | DxgiFormatR32G32B32A32Sint
                | DxgiFormatR32G32B32Typeless
                | DxgiFormatR32G32B32Float
                | DxgiFormatR32G32B32Uint
                | DxgiFormatR32G32B32Sint
                | DxgiFormatR16G16B16A16Typeless
                | DxgiFormatR16G16B16A16Float
                | DxgiFormatR16G16B16A16Unorm
                | DxgiFormatR16G16B16A16Uint
                | DxgiFormatR16G16B16A16Snorm
                | DxgiFormatR16G16B16A16Sint
                | DxgiFormatR32G32Typeless
                | DxgiFormatR32G32Float
                | DxgiFormatR32G32Uint
                | DxgiFormatR32G32Sint
                | DxgiFormatR32G8X24Typeless
                | DxgiFormatD32FloatS8X24Uint
                | DxgiFormatR32FloatX8X24Typeless
                | DxgiFormatX32TypelessG8X24Uint
                | DxgiFormatR10G10B10A2Typeless
                | DxgiFormatR10G10B10A2Unorm
                | DxgiFormatR10G10B10A2Uint
                | DxgiFormatR11G11B10Float
                | DxgiFormatR8G8B8A8Typeless
                | DxgiFormatR8G8B8A8Unorm
                | DxgiFormatR8G8B8A8UnormSRGB
                | DxgiFormatR8G8B8A8Uint
                | DxgiFormatR8G8B8A8Snorm
                | DxgiFormatR8G8B8A8Sint
                | DxgiFormatR16G16Typeless
                | DxgiFormatR16G16Float
                | DxgiFormatR16G16Unorm
                | DxgiFormatR16G16Uint
                | DxgiFormatR16G16Snorm
                | DxgiFormatR16G16Sint
                | DxgiFormatR32Typeless
                | DxgiFormatD32Float
                | DxgiFormatR32Float
                | DxgiFormatR32Uint
                | DxgiFormatR32Sint
                | DxgiFormatR24G8Typeless
                | DxgiFormatD24UnormS8Uint
                | DxgiFormatR24UnormX8Typeless
                | DxgiFormatX24TypelessG8Uint
                | DxgiFormatR8G8Typeless
                | DxgiFormatR8G8Unorm
                | DxgiFormatR8G8Uint
                | DxgiFormatR8G8Snorm
                | DxgiFormatR8G8Sint
                deriving (Eq, Show)

instance Enum DxgiFormat where
  fromEnum DxgiFormatUnknown = 0
  fromEnum DxgiFormatR8G8B8A8Unorm = 28
  toEnum 0 = DxgiFormatUnknown
  toEnum 28 = DxgiFormatR8G8B8A8Unorm
  toEnum unmatched = error ("DxgiFormat.toEnum: cannot match " ++ show unmatched)

instance Storable DxgiFormat where
  sizeOf e = sizeOf ((fromIntegral $ fromEnum e) :: Int32)
  alignment e = alignment ((fromIntegral $ fromEnum e) :: Int32)
  peek ptr = peekByteOff (castPtr ptr :: Ptr Int32) 0 >>= (return . toEnum)
  poke ptr val = pokeByteOff (castPtr ptr :: Ptr Int32) 0 (fromEnum val)

instance CStorable DxgiFormat where
  cSizeOf = sizeOf
  cAlignment = alignment
  cPeek = peek
  cPoke = poke

data DxgiModeScanlineOrder = DxgiModeScanlineOrderUnspecified
                           | DxgiModeScanlineOrderProgressive
                           | DxgiModeScanlineOrderUpperFieldFirst
                           | DxgiModeScanlineOrderLowerFieldFirst
                           deriving (Eq, Show)

instance Enum DxgiModeScanlineOrder where
  fromEnum DxgiModeScanlineOrderUnspecified = 0
  fromEnum DxgiModeScanlineOrderProgressive = 1
  fromEnum DxgiModeScanlineOrderUpperFieldFirst = 2
  fromEnum DxgiModeScanlineOrderLowerFieldFirst = 3
  toEnum 0 = DxgiModeScanlineOrderUnspecified
  toEnum 1 = DxgiModeScanlineOrderProgressive
  toEnum 2 = DxgiModeScanlineOrderUpperFieldFirst
  toEnum 3 = DxgiModeScanlineOrderLowerFieldFirst
  toEnum unmatched = error ("DxgiModeScanlineOrder.toEnum: cannot match " ++ show unmatched)
  
instance Storable DxgiModeScanlineOrder where
  sizeOf e = sizeOf ((fromIntegral $ fromEnum e) :: Int32)
  alignment e = alignment ((fromIntegral $ fromEnum e) :: Int32)
  peek ptr = peekByteOff (castPtr ptr :: Ptr Int32) 0 >>= (return . toEnum)
  poke ptr val = pokeByteOff (castPtr ptr :: Ptr Int32) 0 (fromEnum val)

instance CStorable DxgiModeScanlineOrder where
  cSizeOf = sizeOf
  cAlignment = alignment
  cPeek = peek
  cPoke = poke
   
data DxgiModeScaling = DxgiModeScalingUnspecified
                     | DxgiModeScalingCentered
                     | DxgiModeScalingStretched
                     deriving (Eq, Show)

instance Enum DxgiModeScaling where
  fromEnum DxgiModeScalingUnspecified = 0
  fromEnum DxgiModeScalingCentered = 1
  fromEnum DxgiModeScalingStretched = 2
  toEnum 0 = DxgiModeScalingUnspecified
  toEnum 1 = DxgiModeScalingCentered
  toEnum 2 = DxgiModeScalingStretched
  toEnum unmatched = error ("DxgiModeScaling.toEnum: cannot match " ++ show unmatched)
  
instance Storable DxgiModeScaling where
  sizeOf e = sizeOf ((fromIntegral $ fromEnum e) :: Int32)
  alignment e = alignment ((fromIntegral $ fromEnum e) :: Int32)
  peek ptr = peekByteOff (castPtr ptr :: Ptr Int32) 0 >>= (return . toEnum)
  poke ptr val = pokeByteOff (castPtr ptr :: Ptr Int32) 0 (fromEnum val)
  
instance CStorable DxgiModeScaling where
  cSizeOf = sizeOf
  cAlignment = alignment
  cPeek = peek
  cPoke = poke
  
data DxgiSwapEffect = DxgiSwapEffectDiscard
                    | DxgiSwapEffectSequential
                    deriving (Eq, Show)

instance Enum DxgiSwapEffect where
  fromEnum DxgiSwapEffectDiscard = 0
  fromEnum DxgiSwapEffectSequential = 1
  toEnum 0 = DxgiSwapEffectDiscard
  toEnum 1 = DxgiSwapEffectSequential
  toEnum unmatched = error ("DxgiSwapEffect.toEnum: cannot match " ++ show unmatched)
  
instance Storable DxgiSwapEffect where
  sizeOf e = sizeOf ((fromIntegral $ fromEnum e) :: Int32)
  alignment e = alignment ((fromIntegral $ fromEnum e) :: Int32)
  peek ptr = peekByteOff (castPtr ptr :: Ptr Int32) 0 >>= (return . toEnum)
  poke ptr val = pokeByteOff (castPtr ptr :: Ptr Int32) 0 (fromEnum val)
  
instance CStorable DxgiSwapEffect where
  cSizeOf = sizeOf
  cAlignment = alignment
  cPeek = peek
  cPoke = poke
  
data D3D11CreateDeviceFlag = D3D11CreateDeviceSinglethreaded
                           | D3D11CreateDeviceDebug
                           | D3D11CreateDeviceSwitchToRef
                           | D3D11CreateDevicePreventInternalThreadingOptimizations
                           | D3D11CreateDeviceBGRASupport
                           deriving (Eq, Show)
                           
instance Enum D3D11CreateDeviceFlag where
  fromEnum D3D11CreateDeviceSinglethreaded = 0x1
  fromEnum D3D11CreateDeviceDebug = 0x2
  fromEnum D3D11CreateDeviceSwitchToRef = 0x4
  fromEnum D3D11CreateDevicePreventInternalThreadingOptimizations = 0x8
  fromEnum D3D11CreateDeviceBGRASupport = 0x20
  toEnum 0x1 = D3D11CreateDeviceSinglethreaded
  toEnum 0x2 = D3D11CreateDeviceDebug
  toEnum 0x4 = D3D11CreateDeviceSwitchToRef
  toEnum 0x8 = D3D11CreateDevicePreventInternalThreadingOptimizations
  toEnum 0x20 = D3D11CreateDeviceBGRASupport
  toEnum unmatched = error ("D3D11CreateDeviceFlag.toEnum: cannot match " ++ show unmatched)
  
instance Storable D3D11CreateDeviceFlag where
  sizeOf e = sizeOf ((fromIntegral $ fromEnum e) :: Int32)
  alignment e = alignment ((fromIntegral $ fromEnum e) :: Int32)
  peek ptr = peekByteOff (castPtr ptr :: Ptr Int32) 0 >>= (return . toEnum)
  poke ptr val = pokeByteOff (castPtr ptr :: Ptr Int32) 0 (fromEnum val)
  
instance CStorable D3D11CreateDeviceFlag where
  cSizeOf = sizeOf
  cAlignment = alignment
  cPeek = peek
  cPoke = poke

createDeviceFlag :: [D3D11CreateDeviceFlag] -> Word32
createDeviceFlag = foldl (.|.) 0 . map (fromIntegral . fromEnum)