{-# LANGUAGE DeriveGeneric #-}
module Graphics.D3D11Binding.GUID where

import GHC.Generics (Generic)

import Data.Word

import Foreign.Storable
import Foreign.CStorable
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc

data GUID = GUID Word32 Word16 Word16 [Word8] deriving (Generic)

instance Storable GUID where
  sizeOf _ = 16
  alignment _ = 8
  peek ptr = do 
    d1 <- peekByteOff ptr 0
    d2 <- peekByteOff ptr 4
    d3 <- peekByteOff ptr 6
    d4 <- peekByteOff ptr 8
    d4arr <- peekArray 8 d4
    return $ GUID d1 d2 d3 d4arr
  poke ptr (GUID d1 d2 d3 d4) = alloca $ \d4ptr -> do
    pokeArray d4ptr d4
    pokeByteOff ptr 0 d1
    pokeByteOff ptr 4 d2
    pokeByteOff ptr 6 d3
    pokeByteOff ptr 8 d4ptr

class HasGUID t where
  getGUID :: Ptr (Ptr t) -> GUID