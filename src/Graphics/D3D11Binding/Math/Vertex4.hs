{-# LANGUAGE DeriveGeneric #-}
module Graphics.D3D11Binding.Math.Vertex4 where
import GHC.Generics (Generic)

import Foreign.Storable
import Foreign.CStorable

import Graphics.D3D11Binding.Types

data Vertex4 = Vertex4
  { x :: Float
  , y :: Float
  , z :: Float
  , w :: Float } deriving (Generic)
  
instance CStorable Vertex4
instance Storable Vertex4 where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek

instance HasSubresourceData Vertex4