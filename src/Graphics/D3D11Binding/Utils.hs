module Graphics.D3D11Binding.Utils where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.C.String


maybePoke :: (Storable a) => Maybe a -> (Ptr a -> IO b) -> IO b
maybePoke Nothing proc = proc nullPtr
maybePoke (Just m) proc = alloca $ \ptr -> do
  poke ptr m
  proc ptr

maybeWithCString :: Maybe String -> (CString -> IO a) -> IO a
maybeWithCString Nothing proc = proc nullPtr
maybeWithCString (Just m) proc = withCString m proc

maybePokeArray :: (Storable a) => [a] -> (Ptr a -> IO b) -> IO b
maybePokeArray [] proc = proc nullPtr
maybePokeArray xs proc = alloca $ \ptr -> do
  pokeArray ptr xs
  proc ptr