#include <d3d11_1.h>
#include <stdint.h>

typedef enum DDS_ALPHA_MODE
{
  DDS_ALPHA_MODE_UNKNOWN       = 0,
  DDS_ALPHA_MODE_STRAIGHT      = 1,
  DDS_ALPHA_MODE_PREMULTIPLIED = 2,
  DDS_ALPHA_MODE_OPAQUE        = 3,
  DDS_ALPHA_MODE_CUSTOM        = 4,
} DDS_ALPHA_MODE;

// Standard version
HRESULT CreateDDSTextureFromMemory( ID3D11Device* d3dDevice,
                                    const uint8_t* ddsData,
                                    size_t ddsDataSize,
                                    ID3D11Resource** texture,
                                    ID3D11ShaderResourceView** textureView,
                                    size_t maxsize,
                                    DDS_ALPHA_MODE* alphaMode
                                      );

HRESULT CreateDDSTextureFromFile( ID3D11Device* d3dDevice,
                                  const wchar_t* szFileName,
                                  ID3D11Resource** texture,
                                  ID3D11ShaderResourceView** textureView,
                                  size_t maxsize,
                                  DDS_ALPHA_MODE* alphaMode
                                    );

// Standard version with optional auto-gen mipmap support
HRESULT CreateDDSTextureFromMemoryMipMap( ID3D11Device* d3dDevice,
                                    ID3D11DeviceContext* d3dContext,
                                    const uint8_t* ddsData,
                                    size_t ddsDataSize,
                                    ID3D11Resource** texture,
                                    ID3D11ShaderResourceView** textureView,
                                    size_t maxsize,
                                    DDS_ALPHA_MODE* alphaMode
                                      );

HRESULT CreateDDSTextureFromFileMipMap( ID3D11Device* d3dDevice,
                                  ID3D11DeviceContext* d3dContext,
                                  const wchar_t* szFileName,
                                  ID3D11Resource** texture,
                                  ID3D11ShaderResourceView** textureView,
                                  size_t maxsize,
                                  DDS_ALPHA_MODE* alphaMode
                                    );

// Extended version
HRESULT CreateDDSTextureFromMemoryEx( ID3D11Device* d3dDevice,
                                      const uint8_t* ddsData,
                                      size_t ddsDataSize,
                                      size_t maxsize,
                                      D3D11_USAGE usage,
                                      unsigned int bindFlags,
                                      unsigned int cpuAccessFlags,
                                      unsigned int miscFlags,
                                      BOOL forceSRGB,
                                      ID3D11Resource** texture,
                                      ID3D11ShaderResourceView** textureView,
                                      DDS_ALPHA_MODE* alphaMode
                                      );

HRESULT CreateDDSTextureFromFileEx( ID3D11Device* d3dDevice,
                                    const wchar_t* szFileName,
                                    size_t maxsize,
                                    D3D11_USAGE usage,
                                    unsigned int bindFlags,
                                    unsigned int cpuAccessFlags,
                                    unsigned int miscFlags,
                                    BOOL forceSRGB,
                                    ID3D11Resource** texture,
                                    ID3D11ShaderResourceView** textureView,
                                    DDS_ALPHA_MODE* alphaMode
                                    );

// Extended version with optional auto-gen mipmap support
HRESULT CreateDDSTextureFromMemoryExMipMap( ID3D11Device* d3dDevice,
                                      ID3D11DeviceContext* d3dContext,
                                      const uint8_t* ddsData,
                                      size_t ddsDataSize,
                                      size_t maxsize,
                                      D3D11_USAGE usage,
                                      unsigned int bindFlags,
                                      unsigned int cpuAccessFlags,
                                      unsigned int miscFlags,
                                      BOOL forceSRGB,
                                      ID3D11Resource** texture,
                                      ID3D11ShaderResourceView** textureView,
                                      DDS_ALPHA_MODE* alphaMode
                                      );

HRESULT CreateDDSTextureFromFileExMipMap( ID3D11Device* d3dDevice,
                                    ID3D11DeviceContext* d3dContext,
                                    const wchar_t* szFileName,
                                    size_t maxsize,
                                    D3D11_USAGE usage,
                                    unsigned int bindFlags,
                                    unsigned int cpuAccessFlags,
                                    unsigned int miscFlags,
                                    BOOL forceSRGB,
                                    ID3D11Resource** texture,
                                    ID3D11ShaderResourceView** textureView,
                                    DDS_ALPHA_MODE* alphaMode
                                    );