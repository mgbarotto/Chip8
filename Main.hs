--TODO: Mover el codigo a diferentes modulos
module Main (main) where
import Data.Function
import System.Exit
import System.Environment (getArgs)
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL as SDL
import EmuData
import EmuProcessor

main :: IO()
main = do
  args <- getArgs
  if null args then
    putStrLn "usage: Chip8 rom"
  else do
    putStrLn $ show args
    (rom:_) <- getArgs
    SDL.init [SDL.InitEverything]
    scr <- SDL.setVideoMode screenW screenH 32 [SDL.SWSurface]
    let initialState =  State {
      screen = scr,
      mem = (replicate 4096 0),
      vx = (replicate 16 0),
      i = 0,
      pc = 0x200, --El program counter empieza en 0x200 ya que el comienzo de la memoria esta reservada para el interprete
      sp = 0,
      stack = (replicate 16 0),
      dt=0,
      st=0,
      pixels = replicate pixelsH (replicate pixelsW False),
      keyboard = replicate 16 False
    }
    state <- return (loadToMem charMap 0x0 initialState)
    inst <- fileOpen rom
    state <- return(loadToMem inst 0x200 state)
    loop state
    SDL.quit


dbgloop :: State -> IO (State)
dbgloop s= do
    event <- getEvent
    case event of
      SDL.Quit -> return s
      SDL.KeyDown k -> do
        if (SDL.symKey k) == SDL.SDLK_SPACE
          then  do
            drawScreen (screen s) (pixels s)
            st <- return (tickDT s)
            putStrLn $ "------------------------------------------------"
            putStrLn $ describe $ getInst (pc s) (mem s)
            st <- execute (getInst (pc st) (mem st)) st
            putStrLn $ show $ st
            dbgloop st
        else if (SDL.symKey k) == SDL.SDLK_ESCAPE
          then return s
          else dbgloop $ keyDown s keyMap (SDL.symKey k)
      SDL.KeyUp k -> do
        st <- return (keyUp s keyMap (SDL.symKey k))
        dbgloop st
      otherwise -> dbgloop s
loop :: State -> IO (State)
loop s= do
  SDL.delay 1
  event <- getEvent
  case event of
    SDL.Quit -> return s
    SDL.KeyDown k -> do
      if (SDL.symKey k) == SDL.SDLK_ESCAPE
        then return s
        else do
          st <- return (keyDown s keyMap (SDL.symKey k))
          drawScreen (screen s) (pixels s)
          st <- return (tickDT st)
          st <- execute (getInst (pc st) (mem st)) st
          loop st
    SDL.KeyUp k -> do
      st <- return (keyUp s keyMap (SDL.symKey k))
      drawScreen (screen s) (pixels s)
      st <- return (tickDT st)
      st <- execute (getInst (pc st) (mem st)) st
      loop st
    otherwise -> do
      drawScreen (screen s) (pixels s)
      st <- return (tickDT s)
      st <- execute (getInst (pc st) (mem st)) st
      loop st
