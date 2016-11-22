module EmuData where
import qualified Graphics.UI.SDL as SDL
import Data.Word(Word8, Word32)
import Data.Map(Map, fromList)
--Color de fondo de la pantalla
fgColor   = 0xFFFFFFFF :: Word32
--Color de dibujo en pantalla
bgColor   = 0xFF000000 :: Word32
--Tamaño en pixeles reales de cada pixel emulado
pixelSize = 8::Int
--Cantidad de pixeles que contiene la pantalla
--Deberia ser de 64x32
pixelsW   = 64::Int
pixelsH   = 32::Int

screenW   = pixelsW*pixelSize
screenH   = pixelsH*pixelSize


  --Teclado
  --1  2  3  C
  --4  5  6  D
  --7  8  9  E
  --A  0  B  F
keyMap = fromList [(SDL.SDLK_1,1),  (SDL.SDLK_2,2),(SDL.SDLK_3,3),   (SDL.SDLK_4,12),
                    (SDL.SDLK_q,4),  (SDL.SDLK_w,5),(SDL.SDLK_e,6),   (SDL.SDLK_r,13),
                    (SDL.SDLK_a,7),  (SDL.SDLK_s,8),(SDL.SDLK_d,9),   (SDL.SDLK_f,14),
                    (SDL.SDLK_z,10),(SDL.SDLK_x,0),(SDL.SDLK_c,11), (SDL.SDLK_v,15)] :: Map SDL.SDLKey Int 

charMap = [0xF0, 0x90, 0x90, 0x90, 0xF0,  -- 0
             0x20, 0x60, 0x20, 0x20, 0x70,  -- 1
             0xF0, 0x10, 0xF0, 0x80, 0xF0,  -- 2
             0xF0, 0x10, 0xF0, 0x10, 0xF0,  -- 3
             0x90, 0x90, 0xF0, 0x10, 0x10,  -- 4
             0xF0, 0x80, 0xF0, 0x10, 0xF0,  -- 5
             0xF0, 0x80, 0xF0, 0x90, 0xF0,  -- 6
             0xF0, 0x10, 0x20, 0x40, 0x40,  -- 7
             0xF0, 0x90, 0xF0, 0x90, 0xF0,  -- 8
             0xF0, 0x90, 0xF0, 0x10, 0xF0,  -- 9
             0xF0, 0x90, 0xF0, 0x90, 0x90,  -- A
             0xE0, 0x90, 0xE0, 0x90, 0xE0,  -- B
             0xF0, 0x80, 0x80, 0x80, 0xF0,  -- C
             0xE0, 0x90, 0x90, 0x90, 0xE0,  -- D
             0xF0, 0x80, 0xF0, 0x80, 0xF0,  -- E
             0xF0, 0x80, 0xF0, 0x80, 0x80]::[Word8]  -- F


