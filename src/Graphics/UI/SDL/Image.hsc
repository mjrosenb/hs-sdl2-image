module Graphics.UI.SDL.Image
    ( InitFlag(..),
      imgInit,
      imgQuit,
      withImgInit,
      imgLoadTexture
    ) where

import Data.Word (Word32)
import Prelude hiding (init, Enum(..))
import Graphics.UI.SDL.Utilities (Enum(..), toBitmask)
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Foreign.C (withCString, peekCString, CString)
import Foreign.Ptr (nullPtr)
import Control.Exception (bracket_)
import Graphics.UI.SDL (Renderer, Texture)

#include "SDL_image.h"

data InitFlag = InitJPG
              | InitPNG
              | InitTIF
              | InitWEBP
    deriving(Eq, Ord, Show, Read)

instance Bounded InitFlag where
    minBound = InitJPG
    maxBound = InitWEBP

instance Enum InitFlag Word32 where
    fromEnum InitJPG = #{const IMG_INIT_JPG}
    fromEnum InitPNG = #{const IMG_INIT_PNG}
    fromEnum InitTIF = #{const IMG_INIT_TIF}
    fromEnum InitWEBP = #{const IMG_INIT_WEBP}
    toEnum #{const IMG_INIT_JPG} = InitJPG
    toEnum #{const IMG_INIT_PNG} = InitPNG
    toEnum #{const IMG_INIT_TIF} = InitTIF
    toEnum #{const IMG_INIT_WEBP} = InitWEBP
    toEnum _ = error "Graphics.UI.SDL.General.toEnum: bad argument"
    succ InitJPG = InitPNG
    succ InitPNG = InitTIF
    succ InitTIF = InitWEBP
    succ _ = error "Graphics.UI.SDL.General.succ: bad argument"
    pred InitPNG = InitJPG
    pred InitTIF = InitPNG
    pred InitWEBP = InitTIF
    pred _ = error "Graphics.UI.SDL.General.pred: bad argument"
    enumFromTo x y | x > y = []
                     | x == y = [y]
                     | True = x : enumFromTo (succ x) y


-- | Initializes SDL2-image. This should be called before all other SDL functions.
imgInit :: [InitFlag] -> IO ()
imgInit flags
    = do ret <- _imgInit (fromIntegral (toBitmask flags))
         when (ret == (-1)) (failWithError "SDL_Init")
foreign import ccall unsafe "IMG_Init" _imgInit :: Word32 -> IO Int

withImgInit :: [InitFlag] -> IO a -> IO a
withImgInit flags action
    = bracket_ (imgInit flags) imgQuit action


imgQuit :: IO ()
imgQuit = _imgQuit
foreign import ccall unsafe "IMG_Quit" _imgQuit :: IO ()


-- | Load image file to a texture.
imgLoadTexture :: Renderer -> String -> IO (Either String Texture)
imgLoadTexture rend file
    = withCString file $ \cFile -> do
        tex <- _imgLoadTexture rend cFile
        err <- getError
        if tex == nullPtr
            then return (Left (fromMaybe "IMG_LoadTexture(): Unknown error!" err))
            else return (Right tex)
foreign import ccall unsafe "IMG_LoadTexture" _imgLoadTexture :: Renderer -> CString -> IO Texture


-- | Returns a string containing the last error. Nothing if no error.
getError :: IO (Maybe String)
getError
    = do str <- peekCString =<< _sdlGetError
         if null str
            then return Nothing
            else return (Just str)
foreign import ccall unsafe "SDL_GetError" _sdlGetError :: IO CString


failWithError :: String -> IO a
failWithError msg
    = do err <- fmap (fromMaybe "No SDL error") getError
         ioError $ userError $ msg ++ "\nSDL message: " ++ err
