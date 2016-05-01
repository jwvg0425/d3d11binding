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