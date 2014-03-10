{-# LANGUAGE ForeignFunctionInterface #-}

-- | Provides Win32-compatible version of some system-level functions
module Win32(getLoginName) where
import Foreign
import Foreign.C
import System.Win32.Types

foreign import stdcall unsafe "windows.h GetUserNameW" getUserNameWin32 :: LPTSTR -> LPDWORD -> IO Bool

getLoginName :: IO String
getLoginName = do
  allocaArray 512 $ \ buf -> do
    with 512 $ \ len -> do
      failIfFalse_ "GetUserName" $ getUserNameWin32 buf len
      peekTString buf
