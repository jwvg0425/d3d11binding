module Graphics.D3D11Binding.Enums where
  
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
                deriving (Eq, Show)

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
  
data DxgiSwapEffect = DxgiSwapEffectDiscard
                    | DxgiSwapEffectSequential
                    deriving (Eq, Show)

instance Enum DxgiSwapEffect where
  fromEnum DxgiSwapEffectDiscard = 0
  fromEnum DxgiSwapEffectSequential = 1
  toEnum 0 = DxgiSwapEffectDiscard
  toEnum 1 = DxgiSwapEffectSequential