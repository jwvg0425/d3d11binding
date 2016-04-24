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