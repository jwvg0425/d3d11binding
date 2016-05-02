module Graphics.D3D11Binding.Interface.D3D11Device where
import Data.Word

import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr

import Graphics.Win32

import Graphics.D3D11Binding.Types
import Graphics.D3D11Binding.Utils

import Graphics.D3D11Binding.Interface.Unknown
import Graphics.D3D11Binding.Interface.D3D11Resource
import Graphics.D3D11Binding.Interface.D3D11RenderTargetView
import Graphics.D3D11Binding.Interface.D3D11ClassLinkage
import Graphics.D3D11Binding.Interface.D3D11InputLayout

import Graphics.D3D11Binding.Shader.D3D11VertexShader
import Graphics.D3D11Binding.Shader.D3D11PixelShader

foreign import stdcall "CreateRenderTargetView" c_createRenderTargetView
  :: Ptr ID3D11Device -> Ptr ID3D11Resource -> Ptr D3D11RenderTargetViewDesc -> Ptr (Ptr ID3D11RenderTargetView) -> IO HRESULT

foreign import stdcall "CreateVertexShader" c_createVertexShader
  :: Ptr ID3D11Device -> Ptr () -> Word32 -> Ptr ID3D11ClassLinkage -> Ptr (Ptr ID3D11VertexShader) -> IO HRESULT
  
foreign import stdcall "CreatePixelShader" c_createPixelShader
  :: Ptr ID3D11Device -> Ptr () -> Word32 -> Ptr ID3D11ClassLinkage -> Ptr (Ptr ID3D11PixelShader) -> IO HRESULT

foreign import stdcall "CreateInputLayout" c_createInputLayout
  :: Ptr ID3D11Device -> Ptr D3D11InputElementDesc -> Word32 -> Ptr () -> Word32 -> Ptr (Ptr ID3D11InputLayout) -> IO HRESULT

class (UnknownInterface interface) => D3D11DeviceInterface interface where
  createRenderTargetView
    :: (D3D11ResourceInterface resource) => 
       Ptr interface -> Ptr resource -> Maybe (D3D11RenderTargetViewDesc) -> IO (Either HRESULT (Ptr ID3D11RenderTargetView))
  createRenderTargetView this pResource desc = alloca $ \renderTargetView -> maybePoke desc $ \pDesc -> do
    hr <- c_createRenderTargetView (castPtr this) (castPtr pResource) pDesc renderTargetView
    if hr < 0 then return (Left hr) else Right <$> peek renderTargetView    
  
  createVertexShader 
    :: Ptr interface -> Ptr () -> Word32 -> Ptr ID3D11ClassLinkage -> IO (Either HRESULT (Ptr ID3D11VertexShader))
  createVertexShader this shaderByteCode bytecodeLength pClassLinkage = alloca $ \ppVertexShader -> do
    hr <- c_createVertexShader (castPtr this) shaderByteCode bytecodeLength pClassLinkage ppVertexShader
    if hr < 0 then return (Left hr) else Right <$> peek ppVertexShader
  
  createPixelShader 
    :: Ptr interface -> Ptr () -> Word32 -> Ptr ID3D11ClassLinkage -> IO (Either HRESULT (Ptr ID3D11PixelShader))
  createPixelShader this shaderByteCode bytecodeLength pClassLinkage = alloca $ \ppPixelShader -> do
    hr <- c_createPixelShader (castPtr this) shaderByteCode bytecodeLength pClassLinkage ppPixelShader
    if hr < 0 then return (Left hr) else Right <$> peek ppPixelShader
  
  createInputLayout
    :: Ptr interface -> [D3D11InputElementDesc] -> Ptr () -> Word32 -> IO (Either HRESULT (Ptr ID3D11InputLayout))
  createInputLayout this inputElementDescs shaderByteCode bytecodeLength =
    alloca $ \ppInputLayout -> alloca $ \pElementDescs -> do
      pokeArray pElementDescs inputElementDescs
      hr <- c_createInputLayout
              (castPtr this)
              pElementDescs
              (fromIntegral $ length inputElementDescs)
              shaderByteCode
              bytecodeLength
              ppInputLayout
      if hr < 0 then return (Left hr) else Right <$> peek ppInputLayout
      
data ID3D11Device = ID3D11Device

instance UnknownInterface ID3D11Device
instance D3D11DeviceInterface ID3D11Device