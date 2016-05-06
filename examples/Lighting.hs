{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import GHC.Generics (Generic)
  
import System.Exit
  
import Data.Int
import Data.Word
import Data.Vect.Float
import System.CPUTime

import Control.Exception
import Control.Monad

import Foreign (peekByteOff)
import Foreign.CStorable
import Foreign.Storable
import Foreign.Ptr

import Graphics.Win32
import System.Win32.DLL (getModuleHandle)

import Graphics.D3D11Binding

foreign import stdcall "PostQuitMessage" postQuitMessage :: Int32 -> IO ()

data SimpleVertex = SimpleVertex Vertex3 Vertex3 deriving (Generic)

instance CStorable SimpleVertex
instance Storable SimpleVertex where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek
  
instance HasSubresourceData SimpleVertex

data ConstantBuffer = ConstantBuffer 
 { world :: Mat4
 , view :: Mat4
 , projection :: Mat4 
 , lightDir1 :: Vec4 
 , lightDir2 :: Vec4 
 , lightColor1 :: Color
 , lightColor2 :: Color 
 , outputColor :: Color } deriving (Generic)

instance CStorable ConstantBuffer
instance Storable ConstantBuffer where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek

instance HasSubresourceData ConstantBuffer

instance HasSubresourceData Word16


windowWidth :: (Num a) => a
windowWidth = 640

windowHeight :: (Num a) => a
windowHeight = 480

main :: IO ()
main = do
  hWnd <- createDefaultWindow windowWidth windowHeight wndProc
  useDevice hWnd $ \swapChain device deviceContext renderTargetView depthStencilView -> do
    vb <- compileShaderFromFile "fx/Lighting.fx" "VS" "vs_4_0"
    (il, vs) <- use vb $ \vsBlob -> do
      pointer <- getBufferPointer vsBlob
      size <- getBufferSize vsBlob
      Right vertexShader <- createVertexShader 
                              device
                              pointer
                              size
                              nullPtr
      
      Right inputLayout <- createInputLayout
                              device
                              [ D3D11InputElementDesc 
                                  "POSITION" 
                                  (fromIntegral 0)
                                  DxgiFormatR32G32B32Float
                                  (fromIntegral 0)
                                  (fromIntegral 0)
                                  D3D11InputPerVertexData
                                  (fromIntegral 0)
                              , D3D11InputElementDesc
                                  "NORMAL"
                                  (fromIntegral 0)
                                  DxgiFormatR32G32B32Float
                                  (fromIntegral 0)
                                  (fromIntegral 12)
                                  D3D11InputPerVertexData
                                  (fromIntegral 0) ]
                              pointer
                              size
      iaSetInputLayout deviceContext inputLayout
      return (inputLayout, vertexShader)
    
    pb <- compileShaderFromFile "fx/Lighting.fx" "PS" "ps_4_0"
    pbs <- compileShaderFromFile "fx/Lighting.fx" "PSSolid" "ps_4_0"
    (idb, ps, sps) <- use pb $ \psBlob -> use pbs $ \psSolidBlob -> do
      pointer <- getBufferPointer psBlob
      size <- getBufferSize psBlob
      Right pixelShader <- createPixelShader
                            device
                            pointer
                            size
                            nullPtr
      
      solidPointer <- getBufferPointer psSolidBlob
      solidSize <- getBufferSize psSolidBlob
      
      Right solidPixelShader <- createPixelShader
                                  device
                                  solidPointer
                                  solidSize
                                  nullPtr
      
      let bd = D3D11BufferDesc
                { byteWidth = fromIntegral $ 24 * sizeOf (undefined :: SimpleVertex)
                , usage = D3D11UsageDefault
                , bindFlags = d3d11BindFlags [D3D11BindVertexBuffer]
                , cpuAccessFlags = 0
                , miscFlags = 0
                , structureByteStride = 0 }
                
      Right buffer <- createBuffer
                        device
                        bd
                        [ SimpleVertex (Vertex3 (-1.0) 1.0 (-1.0)) (Vertex3 0.0 1.0 0.0)
                        , SimpleVertex (Vertex3 1.0 1.0 (-1.0)) (Vertex3 0.0 1.0 0.0)
                        , SimpleVertex (Vertex3 1.0 1.0 1.0) (Vertex3 0.0 1.0 0.0)
                        , SimpleVertex (Vertex3 (-1.0) 1.0 1.0) (Vertex3 0.0 1.0 0.0)
                        
                        , SimpleVertex (Vertex3 (-1.0) (-1.0) (-1.0)) (Vertex3 0.0 (-1.0) 0.0)
                        , SimpleVertex (Vertex3 1.0 (-1.0) (-1.0)) (Vertex3 0.0 (-1.0) 0.0)
                        , SimpleVertex (Vertex3 1.0 (-1.0) 1.0) (Vertex3 0.0 (-1.0) 0.0)
                        , SimpleVertex (Vertex3 (-1.0) (-1.0) 1.0) (Vertex3 0.0 (-1.0) 0.0)
                        
                        , SimpleVertex (Vertex3 (-1.0) (-1.0) 1.0) (Vertex3 (-1.0) 0.0 0.0)
                        , SimpleVertex (Vertex3 (-1.0) (-1.0) (-1.0)) (Vertex3 (-1.0) 0.0 0.0)
                        , SimpleVertex (Vertex3 (-1.0) 1.0 (-1.0)) (Vertex3 (-1.0) 0.0 0.0)
                        , SimpleVertex (Vertex3 (-1.0) 1.0 1.0) (Vertex3 (-1.0) 0.0 0.0)
                      
                        , SimpleVertex (Vertex3 1.0 (-1.0) 1.0) (Vertex3 1.0 0.0 0.0)
                        , SimpleVertex (Vertex3 1.0 (-1.0) (-1.0)) (Vertex3 1.0 0.0 0.0)
                        , SimpleVertex (Vertex3 1.0 1.0 (-1.0)) (Vertex3 1.0 0.0 0.0)
                        , SimpleVertex (Vertex3 1.0 1.0 1.0) (Vertex3 1.0 0.0 0.0)
                        
                        , SimpleVertex (Vertex3 (-1.0) (-1.0) (-1.0)) (Vertex3 0.0 0.0 (-1.0))
                        , SimpleVertex (Vertex3 1.0 (-1.0) (-1.0)) (Vertex3 0.0 0.0 (-1.0))
                        , SimpleVertex (Vertex3 1.0 1.0 (-1.0)) (Vertex3 0.0 0.0 (-1.0))
                        , SimpleVertex (Vertex3 (-1.0) 1.0 (-1.0)) (Vertex3 0.0 0.0 (-1.0))
                      
                        , SimpleVertex (Vertex3 (-1.0) (-1.0) 1.0) (Vertex3 0.0 0.0 1.0)
                        , SimpleVertex (Vertex3 1.0 (-1.0) 1.0) (Vertex3 0.0 0.0 1.0)
                        , SimpleVertex (Vertex3 1.0 1.0 1.0) (Vertex3 0.0 0.0 1.0)
                        , SimpleVertex (Vertex3 (-1.0) 1.0 1.0) (Vertex3 0.0 0.0 1.0) ]
                        
      iaSetVertexBuffers deviceContext 0 [(buffer, fromIntegral $ sizeOf (undefined :: SimpleVertex), 0)]
      
      let indexBd = D3D11BufferDesc
                { byteWidth = fromIntegral $ 36 * sizeOf (undefined :: Word16)
                , usage = D3D11UsageDefault
                , bindFlags = d3d11BindFlags [D3D11BindIndexBuffer]
                , cpuAccessFlags = 0
                , miscFlags = 0
                , structureByteStride = 0 }
      Right indexBuffer <- createBuffer
                             device
                             indexBd
                             ([ 3,1,0,
                                2,1,3,

                                6,4,5,
                                7,4,6,

                                11,9,8,
                                10,9,11,

                                14,12,13,
                                15,12,14,

                                19,17,16,
                                18,17,19,

                                22,20,21,
                                23,20,22 ] :: [Word16])
      
      iaSetIndexBuffer deviceContext indexBuffer DxgiFormatR16Uint 0
      iaSetPrimitiveTopology deviceContext D3D11PrimitiveTopologyTrianglelist
      return (indexBuffer, pixelShader, solidPixelShader)
    
    let constantBd = D3D11BufferDesc
                { byteWidth = fromIntegral $ sizeOf (undefined :: ConstantBuffer)
                , usage = D3D11UsageDefault
                , bindFlags = d3d11BindFlags [D3D11BindConstantBuffer]
                , cpuAccessFlags = 0
                , miscFlags = 0
                , structureByteStride = 0 }
    
    Right cb <- createBuffer device constantBd ([] :: [ConstantBuffer])
    
    use cb $ \constantBuffer -> use il $ \inputLayout -> use vs $ \vertexShader -> use idb $ \ indexBuffer ->
      use ps $ \pixelShader -> use sps $ \solidPixelShader -> do
        messagePump 
          hWnd 
          deviceContext 
          swapChain 
          renderTargetView 
          depthStencilView 
          vertexShader 
          pixelShader 
          solidPixelShader
          constantBuffer

useDevice hWnd proc = do
  (s, d, dc, r, ds) <- initDevice hWnd
  use s $ \swapChain -> use d $ \device -> use dc $ \deviceContext -> use r $ \renderTargetView -> use ds $ \depthStencilView ->
    proc swapChain device deviceContext renderTargetView depthStencilView
  
wndProc :: WindowClosure
wndProc hWnd msg wParam lParam
  | msg == wM_DESTROY = postQuitMessage 0 >> return 0
  | otherwise = defWindowProc (Just hWnd) msg wParam lParam

initDevice :: HWND ->
   IO (Ptr IDxgiSwapChain, Ptr ID3D11Device, Ptr ID3D11DeviceContext, Ptr ID3D11RenderTargetView, Ptr ID3D11DepthStencilView)
initDevice hWnd = do
  let bd = DxgiModeDesc 
            windowWidth 
            windowHeight 
            (DxgiRational 60 1) 
            DxgiFormatR8G8B8A8Unorm 
            DxgiModeScanlineOrderUnspecified 
            DxgiModeScalingUnspecified
  
  let sd = DxgiSwapChainDesc
            bd
            (DxgiSampleDesc 1 0)
            dxgiUsageRenderTargetOutput
            1
            hWnd
            True
            DxgiSwapEffectDiscard
            0
  
  Right (swapChain, device, featureLevel, deviceContext) <- d3d11CreateDeviceAndSwapChain
          nullPtr 
          D3DDriverTypeHardware
          nullPtr
          [D3D11CreateDeviceDebug]
          [D3DFeatureLevel11_0, D3DFeatureLevel10_1, D3DFeatureLevel10_0]
          sd
  
  Right (backBuffer :: Ptr ID3D11Texture2D) <- getBuffer swapChain (fromIntegral 0)
  Right renderTargetView <- use backBuffer $ \b -> createRenderTargetView device b Nothing
  
  let td = D3D11Texture2DDesc
            windowWidth
            windowHeight
            1
            1
            DxgiFormatD24UnormS8Uint
            (DxgiSampleDesc 1 0)
            D3D11UsageDefault
            (d3d11BindFlags [D3D11BindDepthStencil])
            0
            0
  Right depthStencil <- createTexture2D device td ([]::[Word16])
  
  let descDsv = D3D11DepthStencilViewDesc
                  DxgiFormatD24UnormS8Uint
                  D3D11DsvDimensionTexture2D
                  0
                  (tex2dDsv 0)
  
  Right depthStencilView <- createDepthStencilView device depthStencil (Just descDsv)
            
  omSetRenderTargets deviceContext [renderTargetView] depthStencilView
  rsSetViewports deviceContext [D3D11Viewport 0 0 windowWidth windowHeight 0 1]
  
  return (swapChain, device, deviceContext, renderTargetView, depthStencilView)

compileShaderFromFile :: String -> String -> String -> IO (Ptr ID3DBlob)
compileShaderFromFile fileName entryPoint shaderModel = do
  Right res <- d3dCompileFromFile 
      fileName
      Nothing
      Nothing
      nullPtr
      (Just entryPoint)
      shaderModel
      [d3dCompileEnableStrictness]
      []
  return res

createDefaultWindow :: Int -> Int -> WindowClosure -> IO HWND
createDefaultWindow width height wndProc = do
  let winClass = mkClassName "Cube Window"
  icon         <- loadIcon   Nothing iDI_APPLICATION
  cursor       <- loadCursor Nothing iDC_ARROW
  bgBrush      <- createSolidBrush (rgb 255 255 255)
  mainInstance <- getModuleHandle Nothing
  registerClass
    ( cS_VREDRAW + cS_HREDRAW
    , mainInstance
    , Just icon
    , Just cursor
    , Just bgBrush
    , Nothing
    , winClass )
  w <- createWindow
       winClass
       "Cube"
       wS_OVERLAPPEDWINDOW
       Nothing Nothing
       (Just width)
       (Just height)
       Nothing
       Nothing
       mainInstance
       wndProc
  
  showWindow w sW_SHOWNORMAL
  updateWindow w
  return w

pM_NOREMOVE, pM_REMOVE, pM_NOYIELD :: UINT
pM_NOREMOVE = 0x0000
pM_REMOVE = 0x0001
pM_NOYIELD = 0x0002

messagePump 
  :: HWND -> Ptr ID3D11DeviceContext -> 
     Ptr IDxgiSwapChain -> Ptr ID3D11RenderTargetView -> Ptr ID3D11DepthStencilView ->
     Ptr ID3D11VertexShader -> Ptr ID3D11PixelShader -> Ptr ID3D11PixelShader -> Ptr ID3D11Buffer -> IO ()
messagePump hwnd deviceContext swapChain renderTargetView depthStencilView vs ps sps cb = 
  Graphics.Win32.allocaMessage $ \ msg ->
    let pump = do
          m <- peekByteOff msg 4 :: IO WindowMessage
          when (m /= wM_QUIT) $ do
            r <- c_PeekMessage msg (maybePtr Nothing) 0 0 pM_REMOVE
            if r /= 0
            then do
              translateMessage msg
              dispatchMessage msg
              return ()
            else do
              render deviceContext swapChain renderTargetView depthStencilView vs ps sps cb
            pump
    in pump
  
render 
  :: Ptr ID3D11DeviceContext -> Ptr IDxgiSwapChain -> Ptr ID3D11RenderTargetView -> Ptr ID3D11DepthStencilView -> 
     Ptr ID3D11VertexShader -> Ptr ID3D11PixelShader -> Ptr ID3D11PixelShader -> Ptr ID3D11Buffer -> IO ()
render deviceContext swapChain renderTargetView depthStencilView vs ps sps cb = do
  clearRenderTargetView deviceContext renderTargetView $ Color 0.0 0.125 0.3 1.0
  clearDepthStencilView deviceContext depthStencilView [D3D11ClearDepth] 1 0
  
  t <- getCPUTime
  let d = (fromIntegral t) / 1000000000000
  
  let eye = Vec3 0.0 4.0 (-10.0)
  let at = Vec3 0.0 1.0 0.0
  let up = Vec3 0.0 1.0 0.0
  let world = rotationY d
  let view = lookAtLH eye at up
  let projection = perspectiveFovLH (pi / 4) (windowWidth / windowHeight) 0.01 100
  let preDir = Vec4 0.0 0.0 (-1.0) 1.0
  let rotate = rotationY $ (-2.0) * d 
  let lights =
       [ (Vec4 (-0.577) 0.577 (-0.577) 1.0, Color 0.5 0.5 0.5 1.0)
       , (preDir .* rotate, Color 0.5 0.0 0.0 1.0) ]  
  
  let cbData = ConstantBuffer 
                (transpose world) 
                (transpose view) 
                (transpose projection)
                (fst (lights !! 0))
                (fst (lights !! 1))
                (snd (lights !! 0))
                (snd (lights !! 1))
                (Color 0 0 0 0)
  
  updateSubresource deviceContext cb 0 Nothing cbData 0 0
  
  vsSetShader deviceContext vs []
  vsSetConstantBuffers deviceContext 0 [cb]
  psSetShader deviceContext ps []
  psSetConstantBuffers deviceContext 0 [cb]
  
  drawIndexed deviceContext 36 0 0
  
  psSetShader deviceContext sps []
  
  forM_ lights $ \(dir@(Vec4 vx vy vz vw), color) -> do
    let trans = fromProjective $ translation (5 *& (Vec3 vx vy vz))
    let scale = fromProjective $ scaling (Vec3 0.2 0.2 0.2)
    let light = scale .*. trans;
    
    let cbData2 = cbData { world = transpose light, outputColor = color }
    
    updateSubresource deviceContext cb 0 Nothing cbData2 0 0
  
    drawIndexed deviceContext 36 0 0
  
  present swapChain 0 0
  return ()