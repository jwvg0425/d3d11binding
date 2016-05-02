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
                | DxgiFormatR16Uint
                deriving (Eq, Show)

instance Enum DxgiFormat where
  fromEnum DxgiFormatUnknown = 0
  fromEnum DxgiFormatR32G32B32A32Float = 2
  fromEnum DxgiFormatR32G32B32Float = 6
  fromEnum DxgiFormatR8G8B8A8Unorm = 28
  fromEnum DxgiFormatR16Uint = 57
  toEnum 0 = DxgiFormatUnknown
  toEnum 2 = DxgiFormatR32G32B32A32Float
  toEnum 6 = DxgiFormatR32G32B32Float
  toEnum 28 = DxgiFormatR8G8B8A8Unorm
  toEnum 57 = DxgiFormatR16Uint
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

data D3D11RtvDimension = D3D11RtvDimensionUnknown
                       | D3D11RtvDimensionBuffer
                       | D3D11RtvDimensionTexture1D
                       | D3D11RtvDimensionTexture1DArray
                       | D3D11RtvDimensionTexture2D
                       | D3D11RtvDimensionTexture2DArray
                       | D3D11RtvDimensionTexture2DMs
                       | D3D11RtvDimensionTexture2DMsArray
                       | D3D11RtvDimensionTexture3D

instance Enum D3D11RtvDimension where
  fromEnum D3D11RtvDimensionUnknown = 0
  fromEnum D3D11RtvDimensionBuffer = 1
  fromEnum D3D11RtvDimensionTexture1D = 2
  fromEnum D3D11RtvDimensionTexture1DArray = 3
  fromEnum D3D11RtvDimensionTexture2D = 4
  fromEnum D3D11RtvDimensionTexture2DArray = 5
  fromEnum D3D11RtvDimensionTexture2DMs = 6
  fromEnum D3D11RtvDimensionTexture2DMsArray = 7
  fromEnum D3D11RtvDimensionTexture3D = 8
  toEnum 0 = D3D11RtvDimensionUnknown
  toEnum 1 = D3D11RtvDimensionBuffer
  toEnum 2 = D3D11RtvDimensionTexture1D
  toEnum 3 = D3D11RtvDimensionTexture1DArray
  toEnum 4 = D3D11RtvDimensionTexture2D
  toEnum 5 = D3D11RtvDimensionTexture2DArray
  toEnum 6 = D3D11RtvDimensionTexture2DMs
  toEnum 7 = D3D11RtvDimensionTexture2DMsArray
  toEnum 8 = D3D11RtvDimensionTexture3D
  toEnum unmatched = error ("D3D11RtvDimension.toEnum: cannot match " ++ show unmatched)
  
instance Storable D3D11RtvDimension where
  sizeOf e = sizeOf ((fromIntegral $ fromEnum e) :: Int32)
  alignment e = alignment ((fromIntegral $ fromEnum e) :: Int32)
  peek ptr = peekByteOff (castPtr ptr :: Ptr Int32) 0 >>= (return . toEnum)
  poke ptr val = pokeByteOff (castPtr ptr :: Ptr Int32) 0 (fromEnum val)
  
instance CStorable D3D11RtvDimension where
  cSizeOf = sizeOf
  cAlignment = alignment
  cPeek = peek
  cPoke = poke
  
data D3D11InputClassification = D3D11InputPerVertexData
                              | D3D11InputPerInstanceData
                              deriving (Eq, Show)

instance Enum D3D11InputClassification where
  fromEnum D3D11InputPerVertexData = 0
  fromEnum D3D11InputPerInstanceData = 1
  toEnum 0 = D3D11InputPerVertexData
  toEnum 1 = D3D11InputPerInstanceData
  toEnum unmatched = error ("D3D11InputClassification.toEnum: cannot match " ++ show unmatched)
  
instance Storable D3D11InputClassification where
  sizeOf e = sizeOf ((fromIntegral $ fromEnum e) :: Int32)
  alignment e = alignment ((fromIntegral $ fromEnum e) :: Int32)
  peek ptr = peekByteOff (castPtr ptr :: Ptr Int32) 0 >>= (return . toEnum)
  poke ptr val = pokeByteOff (castPtr ptr :: Ptr Int32) 0 (fromEnum val)
  
instance CStorable D3D11InputClassification where
  cSizeOf = sizeOf
  cAlignment = alignment
  cPeek = peek
  cPoke = poke
  
data D3D11Usage = D3D11UsageDefault
                | D3D11UsageImmutable
                | D3D11UsageDynamic
                | D3D11UsageStaging
                deriving (Eq, Show)

instance Enum D3D11Usage where
  fromEnum D3D11UsageDefault = 0
  fromEnum D3D11UsageImmutable = 1
  fromEnum D3D11UsageDynamic = 2
  fromEnum D3D11UsageStaging = 3
  toEnum 0 = D3D11UsageDefault
  toEnum 1 = D3D11UsageImmutable
  toEnum 2 = D3D11UsageDynamic
  toEnum 3 = D3D11UsageStaging
  toEnum unmatched = error ("D3D11Usage.toEnum: cannot match " ++ show unmatched)

instance Storable D3D11Usage where
  sizeOf e = sizeOf ((fromIntegral $ fromEnum e) :: Int32)
  alignment e = alignment ((fromIntegral $ fromEnum e) :: Int32)
  peek ptr = peekByteOff (castPtr ptr :: Ptr Int32) 0 >>= (return . toEnum)
  poke ptr val = pokeByteOff (castPtr ptr :: Ptr Int32) 0 (fromEnum val)
  
instance CStorable D3D11Usage where
  cSizeOf = sizeOf
  cAlignment = alignment
  cPeek = peek
  cPoke = poke

data D3D11BindFlag = D3D11BindVertexBuffer
                   | D3D11BindIndexBuffer
                   | D3D11BindConstantBuffer
                   | D3D11BindShaderResource
                   | D3D11BindStreamOutput
                   | D3D11BindRenderTarget
                   | D3D11BindDepthStencil
                   | D3D11BindUnorderedAccess
                   deriving (Eq, Show)
                   
instance Enum D3D11BindFlag where
  fromEnum D3D11BindVertexBuffer = 0x1
  fromEnum D3D11BindIndexBuffer = 0x2
  fromEnum D3D11BindConstantBuffer = 0x4
  fromEnum D3D11BindShaderResource = 0x8
  fromEnum D3D11BindStreamOutput = 0x10
  fromEnum D3D11BindRenderTarget = 0x20
  fromEnum D3D11BindDepthStencil = 0x40
  fromEnum D3D11BindUnorderedAccess = 0x80
  toEnum 0x1 = D3D11BindVertexBuffer
  toEnum 0x2 = D3D11BindIndexBuffer
  toEnum 0x4 = D3D11BindConstantBuffer
  toEnum 0x8 = D3D11BindShaderResource
  toEnum 0x10 = D3D11BindStreamOutput
  toEnum 0x20 = D3D11BindRenderTarget
  toEnum 0x40 = D3D11BindDepthStencil
  toEnum 0x80 = D3D11BindUnorderedAccess
  toEnum unmatched = error ("D3D11BindFlag.toEnum: cannot match " ++ show unmatched)

instance Storable D3D11BindFlag where
  sizeOf e = sizeOf ((fromIntegral $ fromEnum e) :: Int32)
  alignment e = alignment ((fromIntegral $ fromEnum e) :: Int32)
  peek ptr = peekByteOff (castPtr ptr :: Ptr Int32) 0 >>= (return . toEnum)
  poke ptr val = pokeByteOff (castPtr ptr :: Ptr Int32) 0 (fromEnum val)
  
instance CStorable D3D11BindFlag where
  cSizeOf = sizeOf
  cAlignment = alignment
  cPeek = peek
  cPoke = poke

d3d11BindFlags :: [D3D11BindFlag] -> Word32
d3d11BindFlags flags = fromIntegral $ foldl (\acc x -> acc .|. (fromEnum x)) 0 flags

data D3D11PrimitiveTopology = D3D11PrimitiveTopologyUndefined
                            | D3D11PrimitiveTopologyTrianglelist
                            deriving (Eq, Show)
                            
instance Enum D3D11PrimitiveTopology where
  fromEnum D3D11PrimitiveTopologyUndefined = 0
  fromEnum D3D11PrimitiveTopologyTrianglelist = 4
  toEnum 0 = D3D11PrimitiveTopologyUndefined
  toEnum 4 = D3D11PrimitiveTopologyTrianglelist
  toEnum unmatched = error ("D3D11PrimitiveTopology.toEnum: cannot match " ++ show unmatched)