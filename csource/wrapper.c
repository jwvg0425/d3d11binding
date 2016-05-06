#include "dxgi.h"
#include "d3d11.h"
#include <stdio.h>

HRESULT Present(IDXGISwapChain* This, UINT sync_interval, UINT flags)
{
  return This->lpVtbl->Present(This, sync_interval, flags);
}

HRESULT GetBuffer(IDXGISwapChain* This,UINT buffer_idx,REFIID riid,void **surface)
{
  return This->lpVtbl->GetBuffer(This, buffer_idx,riid,surface);
}

 void SetEvictionPriority(ID3D11Texture3D* This, UINT EvictionPriority)
 {
   This->lpVtbl->SetEvictionPriority(This,EvictionPriority);
 }
 
 HRESULT CreateRenderTargetView(
        ID3D11Device* This,
        ID3D11Resource *pResource,
        const D3D11_RENDER_TARGET_VIEW_DESC *pDesc,
        ID3D11RenderTargetView **ppRTView)
{
  return This->lpVtbl->CreateRenderTargetView(This, pResource, pDesc, ppRTView);
}

ULONG Release(IUnknown* This)
{
  return This->lpVtbl->Release(This);
}

void OMSetRenderTargets( 
  ID3D11DeviceContext* This,
  UINT NumViews, 
  ID3D11RenderTargetView *const *ppRenderTargetViews, 
  ID3D11DepthStencilView *pDepthStencilView)
{
  This->lpVtbl->OMSetRenderTargets(This, NumViews, ppRenderTargetViews, pDepthStencilView);
}

void RSSetViewports(
  ID3D11DeviceContext* This,
  UINT NumViewports,
  const D3D11_VIEWPORT *pViewports)
{
  This->lpVtbl->RSSetViewports(This, NumViewports, pViewports);
}

void ClearRenderTargetView(
  ID3D11DeviceContext* This,
  ID3D11RenderTargetView *pRenderTargetView, 
  const FLOAT ColorRGBA[ 4 ])
{
  This->lpVtbl->ClearRenderTargetView(This, pRenderTargetView, ColorRGBA);
}

void ClearState(ID3D11DeviceContext* This)
{
  This->lpVtbl->ClearState(This);
}

void* GetBufferPointer(ID3DBlob* This)
{
  return This->lpVtbl->GetBufferPointer(This);
}

void GetBufferSize(ID3DBlob* This)
{
  This->lpVtbl->GetBufferSize(This);
}

HRESULT CreateVertexShader(
  ID3D11Device* This,
  const void *pShaderBytecode,
  SIZE_T BytecodeLength,
  ID3D11ClassLinkage *pClassLinkage,
  ID3D11VertexShader **ppVertexShader)
{
    return This->lpVtbl->CreateVertexShader(This, pShaderBytecode, BytecodeLength, pClassLinkage, ppVertexShader);
}

HRESULT CreatePixelShader( 
  ID3D11Device* This,
  const void *pShaderBytecode,
  SIZE_T BytecodeLength,
  ID3D11ClassLinkage *pClassLinkage,
  ID3D11PixelShader **ppPixelShader)
{
    return This->lpVtbl->CreatePixelShader(This, pShaderBytecode, BytecodeLength, pClassLinkage, ppPixelShader);
}

HRESULT CreateInputLayout( 
  ID3D11Device* This,
  const D3D11_INPUT_ELEMENT_DESC* pInputElementDescs,
  UINT NumElements,
  const void* pShaderBytecodeWithInputSignature,
  SIZE_T BytecodeLength,
  ID3D11InputLayout** ppInputLayout)
{
  return This->lpVtbl->CreateInputLayout(This, pInputElementDescs, NumElements, pShaderBytecodeWithInputSignature, BytecodeLength, ppInputLayout);
}

void IASetInputLayout( 
  ID3D11DeviceContext* This,
  ID3D11InputLayout *pInputLayout)
{
  return This->lpVtbl->IASetInputLayout(This, pInputLayout);
}

HRESULT CreateBuffer( 
  ID3D11Device* This,
  const D3D11_BUFFER_DESC *pDesc,
  const D3D11_SUBRESOURCE_DATA *pInitialData,
  ID3D11Buffer **ppBuffer)
{
  return This->lpVtbl->CreateBuffer(This, pDesc, pInitialData, ppBuffer);
}

void IASetVertexBuffers( 
  ID3D11DeviceContext* This,
  UINT StartSlot,
  UINT NumBuffers,
  ID3D11Buffer *const *ppVertexBuffers,
  const UINT *pStrides,
  const UINT *pOffsets)
{
  This->lpVtbl->IASetVertexBuffers(This, StartSlot, NumBuffers, ppVertexBuffers, pStrides, pOffsets);
}

void IASetPrimitiveTopology( 
  ID3D11DeviceContext* This,
  D3D11_PRIMITIVE_TOPOLOGY Topology)
{
  This->lpVtbl->IASetPrimitiveTopology(This, Topology);
}

void VSSetShader(
  ID3D11DeviceContext* This,
  ID3D11VertexShader *pVertexShader,
  ID3D11ClassInstance *const *ppClassInstances,
  UINT NumClassInstances)
{
  return This->lpVtbl->VSSetShader(This, pVertexShader, ppClassInstances, NumClassInstances);
}

void PSSetShader(
  ID3D11DeviceContext* This,
  ID3D11PixelShader *pPixelShader,
  ID3D11ClassInstance *const *ppClassInstances,
  UINT NumClassInstances)
{
  return This->lpVtbl->PSSetShader(This, pPixelShader, ppClassInstances, NumClassInstances);
}

void Draw( 
  ID3D11DeviceContext* This,
  UINT VertexCount,
  UINT StartVertexLocation)
{
  This->lpVtbl->Draw(This, VertexCount, StartVertexLocation); 
}

void IASetIndexBuffer( 
  ID3D11DeviceContext* This,
  ID3D11Buffer *pIndexBuffer,
  DXGI_FORMAT Format,
  UINT Offset)
{
  This->lpVtbl->IASetIndexBuffer(This, pIndexBuffer, Format, Offset);  
}

void UpdateSubresource(
  ID3D11DeviceContext* This,
  ID3D11Resource *pDstResource, 
  UINT DstSubresource,
  const D3D11_BOX *pDstBox,
  const void *pSrcData,
  UINT SrcRowPitch,
  UINT SrcDepthPitch)
{
  This->lpVtbl->UpdateSubresource(This, pDstResource, DstSubresource, pDstBox, pSrcData, SrcRowPitch, SrcDepthPitch);
}

void VSSetConstantBuffers(
  ID3D11DeviceContext* This,
  UINT StartSlot,
  UINT NumBuffers,
  ID3D11Buffer *const *ppConstantBuffers)
{
  This->lpVtbl->VSSetConstantBuffers(This, StartSlot, NumBuffers, ppConstantBuffers);
}

void PSSetConstantBuffers(
  ID3D11DeviceContext* This,
  UINT StartSlot,
  UINT NumBuffers,
  ID3D11Buffer *const *ppConstantBuffers)
{
  This->lpVtbl->PSSetConstantBuffers(This, StartSlot, NumBuffers, ppConstantBuffers);
}

void DrawIndexed(
  ID3D11DeviceContext* This,
  UINT IndexCount,
  UINT StartIndexLocation,
  INT BaseVertexLocation)
{
  This->lpVtbl->DrawIndexed(This, IndexCount, StartIndexLocation, BaseVertexLocation);
}

HRESULT CreateTexture2D( 
  ID3D11Device* This,
  const D3D11_TEXTURE2D_DESC *pDesc,
  const D3D11_SUBRESOURCE_DATA *pInitialData,
  ID3D11Texture2D **ppTexture2D)
{
  return This->lpVtbl->CreateTexture2D(This, pDesc, pInitialData, ppTexture2D);
}

HRESULT CreateDepthStencilView(
  ID3D11Device* This,
  ID3D11Resource *pResource,
  const D3D11_DEPTH_STENCIL_VIEW_DESC *pDesc,
  ID3D11DepthStencilView **ppDepthStencilView)
{
  return This->lpVtbl->CreateDepthStencilView(This, pResource, pDesc, ppDepthStencilView);
}

void ClearDepthStencilView(
  ID3D11DeviceContext* This,
  ID3D11DepthStencilView *pDepthStencilView,
  UINT ClearFlags,
  FLOAT Depth,
  UINT8 Stencil)
{
  return This->lpVtbl->ClearDepthStencilView(This, pDepthStencilView, ClearFlags, Depth, Stencil);
}