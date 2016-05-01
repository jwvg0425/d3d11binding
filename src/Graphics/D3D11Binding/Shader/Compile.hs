module Graphics.D3D11Binding.Shader.Compile where

import System.IO

import Data.Bits
import Data.Word
import Data.Maybe

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.C.String

import Graphics.Win32

import Graphics.D3D11Binding.Utils

import Graphics.D3D11Binding.Interface.D3DBlob
import Graphics.D3D11Binding.Interface.D3DInclude

import Graphics.D3D11Binding.Shader.Flags

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
    CString -> CString -> D3DCompileFlag -> D3DCompileEffectFlag ->
    Ptr (Ptr ID3DBlob) -> Ptr (Ptr ID3DBlob) -> IO HRESULT
    
d3dCompile 
  :: String -> Maybe String -> 
     Maybe D3DShaderMacro -> Ptr ID3DInclude -> 
     Maybe String -> String -> 
     [D3DCompileFlag] -> [D3DCompileEffectFlag] ->
     IO (Either (HRESULT, Ptr ID3DBlob) (Ptr ID3DBlob))
d3dCompile source sourceName defines pInclude entryPoint target compileFlags effectFlags = do
  withCStringLen source $ \(csource, len) -> withCString target $ \pTarget ->
    maybeWithCString sourceName $ \pSourceName -> maybePoke defines $ \pDefines ->
      maybeWithCString entryPoint $ \pEntryPoint -> alloca $ \ppCode -> alloca $ \ppErrorMsgs -> do
        let sFlag = foldl (.|.) 0 compileFlags
        let eFlag = foldl (.|.) 0 effectFlags
        hr <- c_d3dCompile 
                (castPtr csource) 
                (fromIntegral len) 
                pSourceName 
                pDefines 
                pInclude 
                pEntryPoint 
                pTarget 
                sFlag 
                eFlag 
                ppCode 
                ppErrorMsgs
        if hr < 0 
        then do
          pErrorMsgs <- peek ppErrorMsgs
          return $ Left (hr, pErrorMsgs)
        else do
          pCode <- peek ppCode
          return $ Right pCode

d3dCompileFromFile
  :: String -> Maybe String -> 
     Maybe D3DShaderMacro -> Ptr ID3DInclude -> 
     Maybe String -> String -> 
     [D3DCompileFlag] -> [D3DCompileEffectFlag] ->
     IO (Either (HRESULT, Ptr ID3DBlob) (Ptr ID3DBlob))
d3dCompileFromFile fileName sourceName defines pInclude entryPoint target compileFlags effectFlags =
  withFile fileName ReadMode $ \handle -> do
    contents <- hGetContents handle
    d3dCompile contents sourceName defines pInclude entryPoint target compileFlags effectFlags