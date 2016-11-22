module Main (main) where
import System.Exit
import System.Environment (getArgs)
import System.FilePath (takeBaseName)
import qualified Graphics.UI.SDL as SDL
import EmuData
import EmuProcessor

main :: IO()
main = do
  args <- getArgs
  if null args then
    putStrLn "usage: Chip8 rom [debug]"
  else do
    (rom:_) <- getArgs
    SDL.init [SDL.InitEverything]
    SDL.setCaption (takeBaseName rom) ""
    scr <- SDL.setVideoMode screenW screenH 32 [SDL.SWSurface]
    let initialState =  State {
      screen = scr,
      mem = replicate 4096 0,
      vx = replicate 16 0,
      i = 0,
      pc = 0x200, --El program counter empieza en 0x200 ya que el comienzo de la memoria esta reservada para el interprete
      sp = 0,
      stack = replicate 16 0,
      dt=0,
      st=0,
      pixels = replicate pixelsH (replicate pixelsW False),
      keyboard = replicate 16 False
    }
    state <- return (loadToMem charMap 0x0 initialState)
    inst <- fileOpen rom
    state <- return(loadToMem inst 0x200 state)
    if "debug" `elem` args
      then dbgloop state 
    else
      loop state
    SDL.quit


dbgloop :: State -> IO State
dbgloop s = do
    event <- getEvent
    case event of
      SDL.Quit -> return s
      SDL.KeyDown k  
        | SDL.symKey k == SDL.SDLK_SPACE -> 
          do drawScreen (screen s) (pixels s)
             st <- return (tickDT s)
             putStrLn "------------------------------------------------"
             putStrLn $ describe $ getInst (pc s) (mem s)
             st <- execute (getInst (pc st) (mem st)) st
             print st
             dbgloop st
        | SDL.symKey k == SDL.SDLK_ESCAPE -> return s
        | otherwise -> dbgloop $ keyDown s keyMap (SDL.symKey k)
      SDL.KeyUp k -> do
        let st = keyUp s keyMap (SDL.symKey k)
        dbgloop st
      _ -> dbgloop s
loop :: State -> IO State
loop s= do
  drawScreen (screen s) (pixels s)
  st <- return (tickDT s)
  st <- execute (getInst (pc st) (mem st)) st
  SDL.delay 1
  event <- getEvent
  case event of
    SDL.Quit -> return st
    SDL.KeyDown k -> 
      if SDL.symKey k == SDL.SDLK_ESCAPE
        then return st
        else loop (keyDown st keyMap (SDL.symKey k))
    SDL.KeyUp k -> loop (keyUp s keyMap (SDL.symKey k))
    _ -> loop st
