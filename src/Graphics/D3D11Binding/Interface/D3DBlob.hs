module Graphics.D3D11Binding.Interface.D3DBlob where

import Data.Word

import Foreign.Ptr

import Graphics.D3D11Binding.Interface.Unknown

data ID3DBlob = ID3DBlob

foreign import stdcall "GetBufferPointer" c_getBufferPointer
  :: Ptr ID3DBlob -> IO (Ptr ())

foreign import stdcall "GetBufferSize" c_getBufferSize
  :: Ptr ID3DBlob -> IO Word32

class (UnknownInterface interface) => D3DBlobInterface interface where
  getBufferPointer :: Ptr interface -> IO (Ptr ())
  getBufferPointer ptr = c_getBufferPointer (castPtr ptr)
  getBufferSize :: Ptr interface -> IO Word32
  getBufferSize ptr = c_getBufferSize (castPtr ptr)

instance UnknownInterface ID3DBlob
instance D3DBlobInterface ID3DBlob