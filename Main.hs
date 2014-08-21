{-# LANGUAGE LambdaCase #-}

-- | Inspired by http://hdante.wordpress.com/2014/07/08/the-hello-wayland-tutorial/
module Main where

import Control.Applicative
import Control.Concurrent (threadWaitRead)
import Data.Function (fix)
import Data.IORef
import Data.Maybe
import Graphics.Wayland.Client
import System.IO
import System.Posix.Files
import System.Posix.IO

main :: IO ()
main = do
  display <- displayConnect >>= \case
    Just x -> return x
    Nothing -> error "couldn't connect to a wayland server"

  displayFd <- displayGetFd display

  putStrLn $ "Using file descriptor " ++ show displayFd
  putStrLn $ "Display at " ++ show display

  registry <- displayGetRegistry display

  putStrLn $ "Registry at "++ show registry

  compositorRef <- newIORef Nothing
  shmRef <- newIORef Nothing
  seatRef <- newIORef Nothing
  shellRef <- newIORef Nothing
  --pointerRef <- newIORef Nothing

  let listener = RegistryListener
        { registryGlobal = \reg iD ifacename version -> do
            putStrLn $ "Received global " ++ show iD ++ " (" ++ ifacename ++ ") version " ++ show version
            case ifacename of
              "wl_compositor" -> registryBindCompositor reg iD ifacename version >>= writeIORef compositorRef . Just
              "wl_shm"        -> registryBindShm        reg iD ifacename version >>= writeIORef shmRef . Just
              "wl_seat"       -> registryBindSeat       reg iD ifacename version >>= writeIORef seatRef . Just
              "wl_shell"      -> registryBindShell      reg iD ifacename version >>= writeIORef shellRef . Just
              _               -> return ()

        , registryGlobalRemove = \_reg iD -> do
            putStrLn $ "Received global remove " ++ show iD
        }

  errorCode <- registrySetListener registry listener

  putStrLn $ "Setting registry listener... " ++ show errorCode

  res <- displayPrepareRead display

  putStrLn $ "Preparing read... " ++ show res

  flushed <- displayFlush display

  putStrLn $ "Flushed " ++ show flushed
  putStrLn "polling"

  threadWaitRead displayFd

  putStrLn $ "Ready to read."

  events <- displayReadEvents display

  putStrLn $ "Read display events: " ++ show events

  dispatched <- displayDispatchPending display

  putStrLn $ "Dispatched events: " ++ show dispatched


  putStrLn "Trying to create window"

  comp  <- fromMaybe (error "got no compositor") <$> readIORef compositorRef
  shell <- fromMaybe (error "got no shell")      <$> readIORef shellRef
  shm   <- fromMaybe (error "got no shm")        <$> readIORef shmRef
  seat  <- fromMaybe (error "got no seat")       <$> readIORef seatRef

  putStrLn $ "Compositor: " ++ show comp
  putStrLn "Trying to create surface"

  surf <- compositorCreateSurface comp

  putStrLn $ "Surface: " ++ show surf

  shellSurf <- shellGetShellSurface shell surf

  putStrLn $ "Shell surface: " ++ show shellSurf

  shellSurfaceSetListener shellSurf ShellSurfaceListener
    { shellSurfacePing = shellSurfacePong
    , shellSurfaceConfigure = \_ss iD sizeX sizeY -> putStrLn $ "shellSurfaceConfigure " ++ show (iD, sizeX, sizeY)
    , shellSurfacePopupDone = \_ss -> return ()
    }
  shellSurfaceSetToplevel shellSurf

  putStrLn $ "Trying to create buffer"

  let path = "images.bin"
  imageSize <- fileSize <$> getFileStatus path
  let bytesPerPixel = 4
      nBytes = fromIntegral imageSize * bytesPerPixel
      width  = 320
      height = 200
      formatWord = let ShmFormat i = shmFormatArgb8888 in fromIntegral i
  -- This needs to be `ReadWrite` (with `ReadOnly` `displayGetError` returns `-12`)
  imageFd <- openFd path ReadWrite Nothing defaultFileFlags
  pool <- shmCreatePool shm imageFd nBytes
  buffer <- shmPoolCreateBuffer pool 0 width height (width * bytesPerPixel) formatWord

  putStrLn $ "Trying to attach surface"

  surfaceAttach surf (Just buffer) 0 0
  surfaceCommit surf

  putStrLn $ "Setting up pointer listener"

  pointer <- seatGetPointer seat
  pointerSetListener pointer PointerListener
    { pointerEnter  = \_ptr _serial _surf x y          -> putStrLn $ "pointer enter " ++ show (x, y)
    , pointerLeave  = \_ptr _serial _surf              -> putStrLn $ "pointer leave"
    , pointerMotion = \_ptr _time x y                  -> putStrLn $ "pointer motion " ++ show (x, y)
    , pointerButton = \_ptr _serial _time button state -> putStrLn $ "pointer button " ++ show (button, state)
    , pointerAxis   = \_ptr _time axis value           -> putStrLn $ "pointer axis " ++ show (axis, value)
    } >>= \case
      Success -> return ()
      Failure -> error "could not set up PointerListener"

  -- Keyboard listeners segfault at the moment (probably when the call returns)
  --putStrLn $ "Setting up keyboard listener"

  --keyboard <- seatGetKeyboard seat
  --keyboardSetListener keyboard KeyboardListener
  --  { keyboardKeymap    = \_kb _format fd size         -> return () -- putStrLn $ "keyboard keymap " ++ show (fd, size)
  --  , keyboardLeave     = \_kb _serial _surface        -> putStrLn $ "keyboard leave"
  --  , keyboardKey       = \_kb _serial _time key state -> putStrLn $ "keyboard key" ++ show (key, state)
  --  , keyboardModifiers = \_kb _serial mods_depressed mods_latched mods_locked group ->
  --                          putStrLn $ "keyboard modifiers " ++ show (mods_depressed, mods_latched, mods_locked, group)
  --  } >>= \case
  --    Success -> return ()
  --    Failure -> error "could not set up KeyboardListener"

  putStrLn $ "Running main event loop"

  fix $ \loop -> displayDispatch display >>= \case
    Just _  -> loop
    Nothing -> do
      err <- displayGetError display
      hPutStrLn stderr $ "main loop error: " ++ show err


  displayDisconnect display
