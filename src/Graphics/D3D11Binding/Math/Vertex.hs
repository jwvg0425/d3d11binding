{-# LANGUAGE DeriveGeneric #-}
module Graphics.D3D11Binding.Math.Vertex where
import GHC.Generics (Generic)

import Foreign.Storable
import Foreign.CStorable

import Graphics.D3D11Binding.Types

class Vertex vertex where
  x :: vertex -> Float
  x _ = 0
  
  y :: vertex -> Float
  y _ = 0
  
  z :: vertex -> Float
  z _ = 0
  
  w :: vertex -> Float
  w _ = 0

data Vertex2 = Vertex2 Float Float deriving (Generic)
  
instance CStorable Vertex2
instance Storable Vertex2 where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek
  
instance HasSubresourceData Vertex2

instance Vertex Vertex2 where
  x (Vertex2 vx _) = vx
  y (Vertex2 _ vy) = vy

data Vertex3 = Vertex3 Float Float Float deriving (Generic)
  
instance CStorable Vertex3
instance Storable Vertex3 where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek

instance HasSubresourceData Vertex3

instance Vertex Vertex3 where
  x (Vertex3 vx _ _) = vx
  y (Vertex3 _ vy _) = vy
  z (Vertex3 _ _ vz) = vz

data Vertex4 = Vertex4 Float Float Float Float deriving (Generic)
  
instance CStorable Vertex4
instance Storable Vertex4 where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek

instance HasSubresourceData Vertex4

instance Vertex Vertex4 where
  x (Vertex4 vx _ _ _) = vx
  y (Vertex4 _ vy _ _) = vy
  z (Vertex4 _ _ vz _) = vz
  w (Vertex4 _ _ _ vw) = vw