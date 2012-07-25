module Fdd where

{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.C.String(CString,withCString)
import Foreign.C.Error(throwErrnoIfNull)
import IO

import Bdd

foreign import ccall "fdd.h fdd_ithset"
  c_fdd_ithset :: CInt -> IO CInt

foreign import ccall "fdd.h fdd_ithvar"
  c_fdd_ithvar :: CInt -> CInt -> IO CInt
                  
foreign import ccall "fdd.h fdd_equals"
  c_fdd_equals :: CInt -> CInt -> IO CInt
                  
foreign import ccall "fdd.h fdd_extdomain"
  c_fdd_extdomain :: Ptr CInt -> CInt -> IO CInt

foreign import ccall "fdd.h fdd_printset"
  c_fdd_printset :: CInt -> IO ()

foreign import ccall "fdd.h fdd_fprintset"
  c_fdd_fprintset :: Ptr CFile -> CInt -> IO ()

fdd_ithset x = c_fdd_ithset x >>= c_bdd_addref
fdd_equals x y = c_fdd_equals x y >>= c_bdd_addref
fdd_ithvar x y = c_fdd_ithvar x y >>= c_bdd_addref
fdd_extdomain xs = withArrayLen xs (\len ptr -> c_fdd_extdomain ptr (fromIntegral len))
fdd_printset = c_fdd_printset

fdd_fprintset path bdd = do
  withCString path $ \cpath -> do
    withCString "w" $ \cmode -> do
      file <- throwErrnoIfNull "fopen: " (fopen cpath cmode)
      c_fdd_fprintset file bdd
      fclose file