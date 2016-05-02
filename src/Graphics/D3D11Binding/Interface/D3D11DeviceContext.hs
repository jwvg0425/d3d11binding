module Graphics.D3D11Binding.Interface.D3D11DeviceContext where
import Data.Word

import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr

import Graphics.Win32

import Graphics.D3D11Binding.Enums
import Graphics.D3D11Binding.Types
import Graphics.D3D11Binding.Utils

import Graphics.D3D11Binding.Interface.Unknown
import Graphics.D3D11Binding.Interface.D3D11Buffer
import Graphics.D3D11Binding.Interface.D3D11RenderTargetView
import Graphics.D3D11Binding.Interface.D3D11DepthStencilView
import Graphics.D3D11Binding.Interface.D3D11InputLayout
import Graphics.D3D11Binding.Interface.D3D11ClassInstance

import Graphics.D3D11Binding.Shader.D3D11VertexShader
import Graphics.D3D11Binding.Shader.D3D11PixelShader

foreign import stdcall "OMSetRenderTargets" c_omSetRenderTargets
  :: Ptr ID3D11DeviceContext -> Word32 -> Ptr (Ptr ID3D11RenderTargetView) -> Ptr ID3D11DepthStencilView -> IO ()

foreign import stdcall "RSSetViewports" c_rsSetViewports
  :: Ptr ID3D11DeviceContext -> Word32 -> Ptr D3D11Viewport -> IO ()

foreign import stdcall "ClearRenderTargetView" c_clearRenderTargetView
  :: Ptr ID3D11DeviceContext -> Ptr ID3D11RenderTargetView -> Ptr Float -> IO ()
  
foreign import stdcall "IASetInputLayout" c_iaSetInputLayout
  :: Ptr ID3D11DeviceContext -> Ptr ID3D11InputLayout -> IO ()
  
foreign import stdcall "IASetVertexBuffers" c_iaSetVertexBuffers
  :: Ptr ID3D11DeviceContext -> Word32 -> Word32 -> Ptr (Ptr ID3D11Buffer) -> Ptr Word32 -> Ptr Word32 -> IO ()

foreign import stdcall "IASetIndexBuffer" c_iaSetIndexBuffer
  :: Ptr ID3D11DeviceContext -> Ptr ID3D11Buffer -> Word32 -> Word32 -> IO ()

foreign import stdcall "IASetPrimitiveTopology" c_iaSetPrimitiveTopology
  :: Ptr ID3D11DeviceContext -> Word32 -> IO ()
  
foreign import stdcall "VSSetShader" c_vsSetShader
  :: Ptr ID3D11DeviceContext -> Ptr ID3D11VertexShader -> Ptr (Ptr ID3D11ClassInstance) -> Word32 -> IO () 
  
foreign import stdcall "PSSetShader" c_psSetShader
  :: Ptr ID3D11DeviceContext -> Ptr ID3D11PixelShader -> Ptr (Ptr ID3D11ClassInstance) -> Word32 -> IO () 

foreign import stdcall "Draw" c_draw
  :: Ptr ID3D11DeviceContext -> Word32 -> Word32 -> IO ()

class (UnknownInterface interface) => D3D11DeviceContextInterface interface where
  omSetRenderTargets :: Ptr interface -> [Ptr ID3D11RenderTargetView] -> Ptr ID3D11DepthStencilView -> IO ()
  omSetRenderTargets ptr renderTargetViews depthStencilView = alloca $ \pRenderTargetViews -> do
    pokeArray pRenderTargetViews renderTargetViews
    c_omSetRenderTargets (castPtr ptr) (fromIntegral $ length renderTargetViews) pRenderTargetViews depthStencilView
  rsSetViewports :: Ptr interface -> [D3D11Viewport] -> IO ()
  rsSetViewports ptr viewports = alloca $ \pViewports -> do
    pokeArray pViewports viewports
    c_rsSetViewports (castPtr ptr) (fromIntegral $ length viewports) pViewports
  iaSetInputLayout :: Ptr interface -> Ptr ID3D11InputLayout -> IO ()
  iaSetInputLayout ptr inputLayout = c_iaSetInputLayout (castPtr ptr) inputLayout
  iaSetVertexBuffers :: Ptr interface -> Word32 -> [(Ptr ID3D11Buffer, Word32, Word32)] -> IO ()
  iaSetVertexBuffers ptr startSlot bufferData = do
    let buffer = map first bufferData
    let stride = map second bufferData
    let offset = map third bufferData
    alloca $ \pBuffer -> alloca $ \pStride -> alloca $ \pOffset -> do
      pokeArray pBuffer buffer
      pokeArray pStride stride
      pokeArray pOffset offset
      c_iaSetVertexBuffers (castPtr ptr) startSlot (fromIntegral $ length bufferData) pBuffer pStride pOffset
    where first (a,_,_) = a
          second (_,b,_) = b
          third (_,_,c) = c
  iaSetIndexBuffer :: Ptr interface -> Ptr ID3D11Buffer -> DxgiFormat -> Word32 -> IO ()
  iaSetIndexBuffer ptr buffer format offset = c_iaSetIndexBuffer (castPtr ptr) buffer (fromIntegral $ fromEnum format) offset
  iaSetPrimitiveTopology :: Ptr interface -> D3D11PrimitiveTopology -> IO ()
  iaSetPrimitiveTopology ptr topology = c_iaSetPrimitiveTopology (castPtr ptr) (fromIntegral $ fromEnum topology)
  clearRenderTargetView :: Ptr interface -> Ptr ID3D11RenderTargetView -> Color -> IO ()
  clearRenderTargetView ptr renderTargetView color = alloca $ \pColor -> do
    poke pColor color
    c_clearRenderTargetView (castPtr ptr) renderTargetView (castPtr pColor)
  clearState :: Ptr interface -> IO ()
  clearState ptr = c_clearState (castPtr ptr)
  vsSetShader :: Ptr interface -> Ptr ID3D11VertexShader -> [Ptr ID3D11ClassInstance] -> IO ()
  vsSetShader ptr vertexShader classInstances = maybePokeArray classInstances $ \pClassInstances -> do
    let classInstanceNum = fromIntegral $ length classInstances
    c_vsSetShader (castPtr ptr) vertexShader pClassInstances classInstanceNum
  psSetShader :: Ptr interface -> Ptr ID3D11PixelShader -> [Ptr ID3D11ClassInstance] -> IO ()
  psSetShader ptr pixelShader classInstances = maybePokeArray classInstances $ \pClassInstances -> do
    let classInstanceNum = fromIntegral $ length classInstances
    c_psSetShader (castPtr ptr) pixelShader pClassInstances classInstanceNum
  draw :: Ptr interface -> Word32 -> Word32 -> IO ()
  draw ptr vertexCount startVertexLocation = c_draw (castPtr ptr) vertexCount startVertexLocation

data ID3D11DeviceContext = ID3D11DeviceContext

foreign import stdcall "ClearState" c_clearState
 :: Ptr ID3D11DeviceContext -> IO ()

instance UnknownInterface ID3D11DeviceContext where
  use i f = do
    res <- f i
    clearState i
    release i
    return res
instance D3D11DeviceContextInterface ID3D11DeviceContext