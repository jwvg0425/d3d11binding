module Graphics.D3D11Binding.Shader.Compile where
import Data.Word

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String

import Graphics.Win32

import Graphics.D3D11Binding.Interface.D3DBlob
import Graphics.D3D11Binding.Interface.D3DInclude

data D3DShaderMacro = D3DShaderMacro
  { name :: String
  , definition :: String }
  
instance Storable D3DShaderMacro where
  sizeOf _ = 8
  alignment _ = 8
  peek ptr = do 
    n <- peekByteOff ptr 0
    d <- peekByteOff ptr 4
    n' <- peekCString n
    d' <- peekCString d
    return $ D3DShaderMacro n' d'
  poke ptr (D3DShaderMacro n d) = do
    withCString n $ \n' -> withCString d $ \d' -> do
      pokeByteOff ptr 0 n'
      pokeByteOff ptr 4 d'

foreign import stdcall "D3DCompile" c_d3dCompile
 :: Ptr () -> Word32 -> CString ->
    Ptr D3DShaderMacro -> Ptr ID3DInclude ->
    CString -> CString -> Word32 -> Word32 ->
    Ptr (Ptr ID3DBlob) -> Ptr (Ptr ID3DBlob) -> IO HRESULT