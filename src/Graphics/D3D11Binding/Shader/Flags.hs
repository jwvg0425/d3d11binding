module Graphics.D3D11Binding.Shader.Flags where
import Data.Bits
import Data.Word

type D3DCompileFlag = Word32

d3dCompileDebug :: D3DCompileFlag
d3dCompileDebug = shift 1 0

d3dCompileSkipValidation :: D3DCompileFlag
d3dCompileSkipValidation = shift 1 1

d3dCompileSkipOptimization :: D3DCompileFlag
d3dCompileSkipOptimization = shift 1 2

d3dCompilePackMatrixRowMajor :: D3DCompileFlag
d3dCompilePackMatrixRowMajor = shift 1 3

d3dCompilePackMatrixColumnMajor :: D3DCompileFlag
d3dCompilePackMatrixColumnMajor = shift 1 4

d3dCompilePartialPrecision :: D3DCompileFlag
d3dCompilePartialPrecision = shift 1 5

d3dCompileForceVSSoftwareNoOpt :: D3DCompileFlag
d3dCompileForceVSSoftwareNoOpt = shift 1 6

d3dCompileForcePSSoftwareNoOpt :: D3DCompileFlag
d3dCompileForcePSSoftwareNoOpt = shift 1 7

d3dCompileNoPreshader :: D3DCompileFlag
d3dCompileNoPreshader = shift 1 8

d3dCompileAvoidFlowControl :: D3DCompileFlag
d3dCompileAvoidFlowControl = shift 1 9

d3dCompilePreferFlowControl :: D3DCompileFlag
d3dCompilePreferFlowControl = shift 1 10

d3dCompileEnableStrictness :: D3DCompileFlag
d3dCompileEnableStrictness = shift 1 11

d3dCompileEnableBackwardsCompatibility :: D3DCompileFlag
d3dCompileEnableBackwardsCompatibility = shift 1 12

d3dCompileIEEEStrictness :: D3DCompileFlag
d3dCompileIEEEStrictness = shift 1 13

d3dCompileOptimizationLevel0 :: D3DCompileFlag
d3dCompileOptimizationLevel0 = shift 1 14

d3dCompileOptimizationLevel1 :: D3DCompileFlag
d3dCompileOptimizationLevel1 = 0

d3dCompileOptimizationLevel2 :: D3DCompileFlag
d3dCompileOptimizationLevel2 = shift 1 14 .|. shift 1 15

d3dCompileOptimizationLevel3 :: D3DCompileFlag
d3dCompileOptimizationLevel3 = shift 1 15

d3dCompileReserved16 :: D3DCompileFlag
d3dCompileReserved16 = shift 1 16

d3dCompileReserved17 :: D3DCompileFlag
d3dCompileReserved17 = shift 1 17

d3dCompileWarningsAreErrors :: D3DCompileFlag
d3dCompileWarningsAreErrors = shift 1 18

type D3DCompileEffectFlag = Word32

d3dCompileEffectChildEffect :: D3DCompileEffectFlag
d3dCompileEffectChildEffect = shift 1 0

d3dCompileEffectAllowSlowOps :: D3DCompileEffectFlag
d3dCompileEffectAllowSlowOps = shift 1 1
