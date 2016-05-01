module Graphics.D3D11Binding.Interface.D3D11Device where
import Data.Word

import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Ptr

import Graphics.Win32

import Graphics.D3D11Binding.Types
import Graphics.D3D11Binding.Utils

import Graphics.D3D11Binding.Interface.Unknown
import Graphics.D3D11Binding.Interface.D3D11Resource
import Graphics.D3D11Binding.Interface.D3D11RenderTargetView
import Graphics.D3D11Binding.Interface.D3D11ClassLinkage

import Graphics.D3D11Binding.Shader.D3D11VertexShader

foreign import stdcall "CreateRenderTargetView" c_createRenderTargetView
  :: Ptr ID3D11Device -> Ptr ID3D11Resource -> Ptr D3D11RenderTargetViewDesc -> Ptr (Ptr ID3D11RenderTargetView) -> IO HRESULT

foreign import stdcall "CreateVertexShader" c_createVertexShader
  :: Ptr ID3D11Device -> Ptr () -> Word32 -> Ptr ID3D11ClassLinkage -> Ptr (Ptr ID3D11VertexShader) -> IO HRESULT
  
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
      
data ID3D11Device = ID3D11Device

instance UnknownInterface ID3D11Device
instance D3D11DeviceInterface ID3D11Device