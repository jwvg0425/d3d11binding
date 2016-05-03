module Graphics.D3D11Binding.Math.Matrix where
  
import Data.Vect.Float

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.CStorable
    
instance CStorable Mat4 where
  cSizeOf = sizeOf
  cAlignment = alignment
  cPeek = peek
  cPoke = poke

lookAtLH :: Vec3 -> Vec3 -> Vec3 -> Mat4
lookAtLH eye focus up = lookToLH eye eyeDir up
  where eyeDir = focus &- eye
 
lookToLH :: Vec3 -> Vec3 -> Vec3 -> Mat4
lookToLH eye eyeDir up = transpose m
  where r2@(Vec3 r2x r2y r2z) = normalize eyeDir
        r0@(Vec3 r0x r0y r0z) = normalize (up &^ r2)
        r1@(Vec3 r1x r1y r1z) = r2 &^ r0
        negEye = neg eye
        d0 = r0 &. negEye
        d1 = r1 &. negEye
        d2 = r2 &. negEye
        m = Mat4 (Vec4 r0x r0y r0z d0)
                 (Vec4 r1x r1y r1z d1)
                 (Vec4 r2x r2y r2z d2)
                 (Vec4   0   0   0  1)
                 
perspectiveFovLH :: Float -> Float -> Float -> Float -> Mat4
perspectiveFovLH fovAngleY aspectRatio nearZ farZ = m
  where sinFov = sin (fovAngleY * 0.5)
        cosFov = cos (fovAngleY * 0.5)
        height = cosFov / sinFov
        width = height / aspectRatio
        rate = farZ / (farZ - nearZ)
        m = Mat4 (Vec4 width 0 0 0)
                 (Vec4 0 height 0 0)
                 (Vec4 0 0 rate 1.0)
                 (Vec4 0 0 ((-rate)*nearZ) 0)
                 
rotationY :: Float -> Mat4
rotationY angle = m
  where sinAngle = sin angle
        cosAngle = cos angle
        m = Mat4 (Vec4 cosAngle 0 (-sinAngle) 0)
                 (Vec4 0 1 0 0)
                 (Vec4 sinAngle 0 cosAngle 0)
                 (Vec4 0 0 0 1)

rotationZ :: Float -> Mat4
rotationZ angle = m
  where sinAngle = sin angle
        cosAngle = cos angle
        m = Mat4 (Vec4 cosAngle sinAngle 0 0)
                 (Vec4 (-sinAngle) cosAngle 0 0)
                 (Vec4 0 0 1 0)
                 (Vec4 0 0 0 1)