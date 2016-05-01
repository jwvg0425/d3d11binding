module Graphics.D3D11Binding.GUID where

import Data.Word

import Foreign.Storable
import Foreign.CStorable
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc

data GUID = GUID Word32 Word16 Word16 [Word8] deriving (Show)

instance Storable GUID where
  sizeOf _ = 16
  alignment _ = 8
  peek ptr = do 
    d1 <- peekByteOff ptr 0
    d2 <- peekByteOff ptr 4
    d3 <- peekByteOff ptr 6
    d40 <- peekByteOff ptr 8
    d41 <- peekByteOff ptr 9
    d42 <- peekByteOff ptr 10
    d43 <- peekByteOff ptr 11
    d44 <- peekByteOff ptr 12
    d45 <- peekByteOff ptr 13
    d46 <- peekByteOff ptr 14
    d47 <- peekByteOff ptr 15
    return $ GUID d1 d2 d3 [d40,d41,d42,d43,d44,d45,d46,d47]
  poke ptr (GUID d1 d2 d3 d4) = do
    pokeByteOff ptr 0 d1
    pokeByteOff ptr 4 d2
    pokeByteOff ptr 6 d3
    pokeByteOff ptr 8 (d4 !! 0)
    pokeByteOff ptr 9 (d4 !! 1)
    pokeByteOff ptr 10 (d4 !! 2)
    pokeByteOff ptr 11 (d4 !! 3)
    pokeByteOff ptr 12 (d4 !! 4)
    pokeByteOff ptr 13 (d4 !! 5)
    pokeByteOff ptr 14 (d4 !! 6)
    pokeByteOff ptr 15 (d4 !! 7)

class HasGUID t where
  getGUID :: Ptr (Ptr t) -> GUID