module Bdd where

{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign
import Foreign.C.Types
import Foreign.C.String(CString)
import IO

import Control.DeepSeq(NFData,rnf)

foreign import ccall unsafe "stdio.h fopen"
  fopen :: CString -> CString -> IO (Ptr CFile)

foreign import ccall unsafe "stdio.h fclose"
  fclose :: Ptr CFile -> IO CInt

foreign import ccall "bdd.h bdd_init"
  c_bdd_init :: CInt -> CInt -> IO CInt

foreign import ccall "bdd.h bdd_done"
  c_bdd_done :: IO ()

foreign import ccall "bdd.h bdd_addref"
  c_bdd_addref :: CInt -> IO CInt

foreign import ccall "bdd.h bdd_delref"
  c_bdd_delref :: CInt -> IO CInt

foreign import ccall "bdd.h bdd_not"
  c_bdd_not :: CInt -> IO CInt

foreign import ccall "bdd.h bdd_and"
  c_bdd_and :: CInt -> CInt -> IO CInt

foreign import ccall "bdd.h bdd_or"
  c_bdd_or :: CInt -> CInt -> IO CInt

foreign import ccall "bdd.h bdd_exist"
  c_bdd_exist :: CInt -> CInt -> IO CInt

foreign import ccall "bdd.h bdd_forall"
  c_bdd_forall :: CInt -> CInt -> IO CInt
                 
foreign import ccall "bdd.h bdd_apply"
  c_bdd_apply :: CInt -> CInt -> CInt -> IO CInt
{-
foreign import ccall "kernel.c bddtrue"
  bddtrue :: IO CInt

foreign import ccall "kernel.c bddfalse"
  bddfalse :: IO CInt
-}

bddfalse = c_bdd_addref 0
bddtrue = c_bdd_addref 1

bdd_init x y = c_bdd_init x y
bdd_done = c_bdd_done
bdd_addref x = c_bdd_addref x
bdd_delref x = c_bdd_delref x

bdd_not x =  (c_bdd_not x) >>= c_bdd_addref
bdd_and x y = (c_bdd_and x y) >>= c_bdd_addref
bdd_or x y = (c_bdd_or x y) >>= c_bdd_addref
bdd_exist x y = (c_bdd_exist x y) >>= c_bdd_addref
bdd_forall x y = (c_bdd_forall x y) >>= c_bdd_addref

--bdd_apply x y op = c_bdd_apply x y op
--bdd_printset x = c_bdd_printset x

--create_bdd x = c_bdd_addref x
--fastsin :: Double -> Double
--fastsin x = realToFrac (c_sin (realToFrac x))

instance NFData CInt where
  rnf n = rnf n `seq`()