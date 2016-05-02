{-# LANGUAGE DeriveGeneric #-}
module Graphics.D3D11Binding.Math.Vertex3 where
import GHC.Generics (Generic)

import Foreign.Storable
import Foreign.CStorable

import Graphics.D3D11Binding.Types

data Vertex3 = Vertex3
  { x :: Float
  , y :: Float
  , z :: Float } deriving (Generic)
  
instance CStorable Vertex3
instance Storable Vertex3 where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek

instance HasSubresourceData Vertex3