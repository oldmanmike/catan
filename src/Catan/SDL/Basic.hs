{-# LANGUAGE PatternSynonyms #-}
-------------------------------------------------------------------------------
-- |
-- Copyright        : (c) 2015 Michael Carpenter
-- License          : GPL3
-- Maintainer       : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability        : provisional
-- Portability      : portable
--
-------------------------------------------------------------------------------
module Catan.SDL.Basic (
    initializeSDL,
    createWindow,
    loadBitmap,
    resetSDL,
    closeSDL,
    renderBitmap,
    loadSurface,
    pollEvent,
    convertEvent,
    throwSDLError,
    applyToPointer
) where


-- * Internal Imports
import Catan.SDL.Enum

-- * External Imports
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import qualified Graphics.UI.SDL as SDL

-------------------------------------------------------------------------------
------------------------------------ Basic ------------------------------------
-------------------------------------------------------------------------------


-- | Initialize SDL with flags.
initializeSDL :: [SDL.InitFlag] -> IO (Either String ())
initializeSDL flags = do
    status <- SDL.init $ foldl (.|.) 0 flags
    return $ if status < 0
                then Left "SDL could not initialize!"
                else Right ()
            

-- | Creates a window using a title, width, and height.
createWindow :: String -> Int -> Int -> IO (Either String SDL.Window)
createWindow title width height = withCAString title $ \winTitle -> do
    window <- SDL.createWindow
        winTitle
        SDL.SDL_WINDOWPOS_UNDEFINED 
        SDL.SDL_WINDOWPOS_UNDEFINED
        (toEnum width :: CInt)
        (toEnum height :: CInt)
        SDL.SDL_WINDOW_SHOWN
    return $ if window == nullPtr 
                then Left "Window could not be created!" 
                else Right window


resetSDL :: SDL.Window -> Ptr SDL.Surface -> IO ()
resetSDL win surface = do
    pxlfmt <- peek surface
    colorscreen <- SDL.mapRGB (SDL.surfaceFormat pxlfmt) 0 0 0
    _ <- SDL.fillRect surface nullPtr colorscreen
    _ <- SDL.updateWindowSurface win
    return ()


closeSDL :: SDL.Window -> Ptr SDL.Surface -> IO ()
closeSDL win surface = do
    SDL.freeSurface surface
    SDL.destroyWindow win
    SDL.quit


-------------------------------------------------------------------------------
------------------------------------ Video ------------------------------------
-------------------------------------------------------------------------------


-- | Loads a bitmap into a surface based on a provided filepath.
loadBitmap :: String -> IO (Either String (Ptr SDL.Surface))
loadBitmap path = do
    surface <- withCAString path SDL.loadBMP
    return $ if surface == nullPtr 
                then Left ("Unable to load image " ++ path ++ "!") 
                else Right surface


renderBitmap :: SDL.Window -> Ptr SDL.Surface -> Ptr SDL.Surface -> IO (Either String ())
renderBitmap win surface bitmap = do
    _ <- SDL.blitSurface bitmap nullPtr surface nullPtr
    status <- SDL.updateWindowSurface win
    return $ if status < 0
                then Left "SDL could not render bitmap!"
                else Right ()


loadSurface :: FilePath -> IO (Ptr SDL.Surface)
loadSurface path = do
    cPath <- newCString path
    loadedSurfacePtr <- SDL.loadBMP cPath
    loadedSurface <- peek loadedSurfacePtr
    optimizedSurfacePtr <- SDL.convertSurface loadedSurfacePtr (SDL.surfaceFormat loadedSurface) 0
    SDL.freeSurface loadedSurfacePtr
    return optimizedSurfacePtr


-------------------------------------------------------------------------------
----------------------------------- Events ------------------------------------
-------------------------------------------------------------------------------


pollEvent :: IO (Maybe SDL.Event)
pollEvent = liftIO $ alloca $ poll
    where 
        poll eventPtr = do result <- SDL.pollEvent eventPtr
                           case result of
                                0 -> return Nothing
                                _ -> do Just <$> peek eventPtr


convertEvent :: Maybe SDL.Event -> Maybe Event
convertEvent (Just (SDL.KeyboardEvent SDL.SDL_KEYDOWN _ _ _ _ (SDL.Keysym s _ _))) = Just $ Keyboard (toEnum . fromIntegral $ s)
convertEvent (Just (SDL.MouseMotionEvent SDL.SDL_MOUSEMOTION _ _ _ _ x y _ _)) = Just $ Mouse (fromIntegral x) (fromIntegral y)
convertEvent (Just _) = Just Something
convertEvent Nothing = Nothing


-------------------------------------------------------------------------------
------------------------------------ Utils ------------------------------------ 
-------------------------------------------------------------------------------


throwSDLError :: String -> IO a
throwSDLError message = do
    errorString <- SDL.getError >>= peekCString
    fail (message ++ " SDL_Error: " ++ errorString)


applyToPointer :: (Storable a) => (a -> b) -> Ptr a -> IO b
applyToPointer f ptr = liftM f $ peek ptr


