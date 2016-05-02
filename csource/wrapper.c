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

HRESULT CreateInputLayout( 
  ID3D11Device* This,
  const D3D11_INPUT_ELEMENT_DESC* pInputElementDescs,
  UINT NumElements,
  const void* pShaderBytecodeWithInputSignature,
  SIZE_T BytecodeLength,
  ID3D11InputLayout** ppInputLayout)
{
  return This -> lpVtbl->CreateInputLayout(This, pInputElementDescs, NumElements, pShaderBytecodeWithInputSignature, BytecodeLength, ppInputLayout);
}