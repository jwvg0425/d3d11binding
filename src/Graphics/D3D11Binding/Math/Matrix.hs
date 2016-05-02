module Graphics.D3D11Binding.Math.Matrix where
  
import Data.Matrix

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.CStorable

type Matrix4 = Matrix Float

matrix4 = matrix 4 4

matrix4FromList = fromList 4 4

instance (Storable m) => Storable (Matrix m) where
  sizeOf _ = 64
  alignment _ = 8
  peek ptr = do 
    list <- peekArray 16 (castPtr ptr)
    return $ matrix4FromList list
  poke ptr mat = do
    let list = toList mat
    pokeArray (castPtr ptr) list
    
instance (Storable m) => CStorable (Matrix m) where
  cSizeOf = sizeOf
  cAlignment = alignment
  cPeek = peek
  cPoke = poke