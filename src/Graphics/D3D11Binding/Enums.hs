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

                  