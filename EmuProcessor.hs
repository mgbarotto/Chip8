module EmuProcessor where
import Numeric (showHex, readHex)
import qualified Graphics.UI.SDL as SDL
import Data.Bits (Bits, shiftL, shiftR, xor, (.&.), (.|.), testBit)
import Data.Word (Word8, Word16)
import Data.Maybe (fromMaybe)
import System.Random (randomIO)
import Data.List(genericTake, genericDrop)
import qualified Data.ByteString as B
import qualified Data.Map as Map
import EmuData
data State = State  {
    screen :: SDL.Surface,
    pixels :: [[Bool]],
    pc :: Word16,     -- Program counter
    sp :: Word8,      -- Stack pointer
    stack :: [Word16],     
    vx :: [Word8],    -- Registros de procesador
    i :: Word16,      -- Registro especial I
    dt :: Word8,      --Delay timer, su valor disminuye con
                      -- cada instruccion hasta llegar a 0
    st :: Word8,      -- Sound timer, su valor disminuye con
                      -- cada instruccion, y el sonido suena
                      -- mientras no sea 0
    mem :: [Word8],
    keyboard :: [Bool]
}

instance Show State where
  show s = "I:"++show (i s)++
    "\n      0,1,2,3,4,5,6,7,8,9,A,B,C,D,E,F"++
    "\nReg: "++show(vx s)++"\nPC: "++show(pc s)++"\nSP: "++show(sp s)++"\nStack: "++show(stack s)++"\nDT: "++show(dt s)++"\nST: "++show(st s)
    ++ "\nKeyboard:\n   "++ show (take 4 (keyboard s)) ++ "\n   "++ show (take 4 (drop 4 (keyboard s))) ++ "\n   "++ show (take 4 (drop 8 (keyboard s)))


waitForKey :: State -> IO(State, SDL.SDLKey)
waitForKey s = do
  event <- getEvent
  case event of
    SDL.Quit -> return (s, SDL.SDLK_ESCAPE)
    SDL.KeyDown k -> return (keyDown s keyMap (SDL.symKey k), SDL.symKey k)
    SDL.KeyUp k -> waitForKey (keyUp s keyMap (SDL.symKey k))
    _ -> do
      let st = tickDT s
      waitForKey st


getEvent :: IO SDL.Event
getEvent = do
  e <- SDL.pollEvent
  case e of
    SDL.Quit -> return e
    (SDL.KeyDown _)-> return e
    (SDL.KeyUp _)-> return e
    SDL.NoEvent -> return e
    _ -> getEvent
-- Manejo de instrucciones
execute :: Word16 -> State -> IO State
execute ins s=
  case showHex ins "" of
    --0NNN  Calls RCA 1802 program at address NNN. Not necessary for most ROMs.
    nnn@[_, _, _] -> return (advancePC s) --Esta instruccion se ignora y es innecesario especificarla, la dejo por completitud
   --00E0  Clears the screen.
    "e0"          -> return (advancePC (s{pixels =
                      replicate pixelsH (replicate pixelsW False)}))
   --00EE  Returns from a subroutine.
    "ee"          -> return (advancePC (removeFromStack s))
   --1NNN  Jumps to address NNN.
    ('1':_)       -> return (s{pc = n })
   --2NNN  Calls subroutine at NNN.
    ('2':_)       -> return (addToStack n s)
   --3XKK  Skips the next instruction if VX equals KK.
    ('3':_)       -> return (skipOn (==) (getReg s x) k s)
   --4XKK  Skips the next instruction if VX doesn't equal KK.
    ('4':_)       -> return (skipOn (/=) (getReg s x) k s)
   --5XY0  Skips the next instruction if VX equals VY.
    ('5':_)       -> return (skipOn (==) (getReg s x) (getReg s y) s)
   --6XNN  Sets VX to KK.
    ('6':_)       -> return (advancePC (setReg s x k))
   --7XKK  Adds KK to VX.
    ('7':_)       -> return (advancePC ( addToReg s x k))
   --8XY0  Sets VX to the value of VY.
    ['8',_,_,'0'] -> return (advancePC (setReg s x (getReg s  y)))
   --8XY1  Sets VX to VX or VY.
    ['8',_,_,'1'] -> return (advancePC (
                      setReg s x (getReg s  y .|. getReg s  x)))
   --8XY2  Sets VX to VX and VY.
    ('8':_:_:"2") -> return (advancePC (
                      setReg s x (getReg s  y.&. getReg s  x)))
     --8XY3  Sets VX to VX xor VY.
    ('8':_:_:"3") -> return (advancePC (
                      setReg s x (xor (getReg s y) (getReg s  x))))
     --8XY4  Adds VY to VX. VF is set to 1 when there's a carry, and to 0 when there isn't.
    ('8':_:_:"4") -> return (advancePC (sumReg s x y))
     --8XY5  VY is subtracted from VX. VF is set to 0 when there's a borrow, and 1 when there isn't.
    ('8':_:_:"5") -> return (advancePC (subReg s x y x))
     --8XY6  Shifts VX right by one. VF is set to the value of the least --significant bit of VX before the shift.[2]
    ('8':_:_:"6") -> return (advancePC (shiftRReg s x))
     --8XY7  Sets VX to VY minus VX. VF is set to 0 when there's a borrow, and 1 --when there isn't.
    ('8':_:_:"7") -> return (advancePC (subReg s y x x))
     --8XYE  Shifts VX left by one. VF is set to the value of the most --significant bit of VX before the shift.[2]
    ('8':_:_:"e") -> return (advancePC (shiftLReg s x))
     --9XY0  Skips the next instruction if VX doesn't equal VY.
    ('9':_:_:"0") -> return (skipOn (/=) (getReg s x) (getReg s y) s)
     --ANNN  Sets I to the address NNN.
    ('a':_)       -> return (advancePC (setI s n))
     --BNNN  Jumps to the address NNN plus V0.
    ('b':_)       -> return (setPC s (n+ asWord16(getReg s 0)))
     --CXNN  Sets VX to the result of a bitwise and operation on a random number --and NN.
    ('c':_)       -> do
                     ran <- randomIO :: IO Word8
                     return (advancePC (setReg s x (ran.&.k)))
     --DXYN  Draws a sprite at coordinate (VX, VY) that has a width of 8 pixels 
     --and a height of N pixels. Each row of 8 pixels is read as bit-coded 
     --starting from memory location I; I value doesn’t change after the execution 
     --of this instruction. As described above, VF is set to 1 if any screen 
     --pixels are flipped from set to unset when the sprite is drawn, and to 0 if 
     --that doesn’t happen
    ('d':_)       -> do
                     let sprite =  map toBitsArray (loadFromMem (asInt (i s)) (asInt h) s)
                     (st, coll) <- drawSprite s (getReg s x) (getReg s y) sprite
                     if coll
                       then return (advancePC (setReg st 0xf 1))
                       else return (advancePC (setReg st 0xf 0))
     --EX9E  Skips the next instruction if the key stored in VX is pressed.
    ('e':_:"9e") -> return (skipIf (getAt (getReg s x) (keyboard s)) s)
     --EXA1  Skips the next instruction if the key stored in VX isn't pressed.
    ('e':_:"a1") -> return (skipIf (not (getAt (getReg s x) (keyboard s))) s)
     --FX07  Sets VX to the value of the delay timer.
    ('f':_:"07") -> return (advancePC (setReg s x (dt s)))
     --FX0A  A key press is awaited, and then stored in VX.
    ('f':_:"0a") -> do
                    (st, key) <- waitForKey s
                    if key == SDL.SDLK_ESCAPE
                      then do
                        SDL.quit
                        return s
                      else do
                        let keyPos = asWord8 (fromMaybe 0 (Map.lookup key keyMap))
                        return (advancePC (setReg s x keyPos))
     --FX15  Sets the delay timer to VX.
    ('f':_:"15") -> return (advancePC (setDT s (getReg s x)))
     --FX18  Sets the sound timer to VX. --TODO: Implementar sonido
    ('f':_:"18") -> return (advancePC s)
     --FX1E  Adds VX to I.[3]
    ('f':_:"1e") -> return (advancePC (setI s (i s +asWord16 (getReg s x))))
     --FX29  Sets I to the location of the sprite for the character in VX. --Characters 0-F (in hexadecimal) are represented by a 4x5 font.
    ('f':_:"29") -> return (advancePC (setI s (asWord16 (getReg s x * 5))))
     --FX33  Stores the binary-coded decimal representation of VX, with the most 
     --significant of three digits at the address in I, the middle digit at I plus 
     --1, and the least significant digit at I plus 2. (In other words, genericTake the 
     --decimal representation of VX, place the hundreds digit in memory at 
     --location in I, the tens digit at location I+1, and the ones digit at location I+2.)
    ('f':_:"33") -> do
                    let v = getReg s x
                    let bcd = [div v 100, div (mod v 100) 10, mod v 10]
                    return (advancePC (loadToMem bcd (asInt (i s)) s))
     --FX55  Stores V0 to VX (including VX) in memory starting at address I.
    ('f':_:"55") -> return (advancePC (
                    loadToMem (genericTake (x+1) (vx s)) (asInt (i s)) s))
     --FX65  Fills V0 to VX (including VX) with values from memory starting at address I.
    ('f':_:"65") -> return (advancePC (
                    s{vx=loadFromMem (asInt (i s)) (asInt x+1) s++
                     drop (asInt x+1)  (vx s)}))
    _ -> return (advancePC s)
  where x = getX ins
        y = getY ins
        n = getN ins
        k = getK ins
        h = getH ins


--

describe :: Word16 -> String
describe ins =
  case showHex ins "" of
    nnn@[_, _, _] -> "Cargar RCA 1802 en "++nnn
    "e0" -> "Limpiar pantalla"
    "ee" -> "Regresar subrutina"
    ('1':n)       -> "["++showHex ins ""++"] Saltar a la direccion "++ nn
    ('2':n)       -> "["++showHex ins ""++"] Llamar subrutina en "++ nn
    ('3':x:n)     -> "["++showHex ins ""++"] Saltear si V"++[x]++" == "++ kk
    ('4':x:n)     -> "["++showHex ins ""++"] Saltear si V"++[x]++" /= "++ kk
    ('5':x:y:_)   -> "["++showHex ins ""++"] Saltear si V"++[x]++" == V"++ [y]
    ('6':x:n)     -> "["++showHex ins ""++"] Setea V"++[x]++" a "++ kk
    ('7':x:n)     -> "["++showHex ins ""++"] Suma "++kk++" a V"++[x]
    ['8',x,y,'0'] -> "["++showHex ins ""++"] Copia V"++[y]++" a V"++[x]
    ['8',x,y,'1'] -> "["++showHex ins ""++"] V"++[x]++" = V"++[x]++"|V"++[y]
    ['8',x,y,'2'] -> "["++showHex ins ""++"] V"++[x]++" = V"++[x]++"&V"++[y]
    ['8',x,y,'3'] -> "["++showHex ins ""++"] V"++[x]++" = V"++[x]++"xor V"++[y]
    ['8',x,y,'4'] -> "["++showHex ins ""++"] V"++[x]++"+=V"++[y]
    ['8',x,y,'5'] -> "["++showHex ins ""++"] V"++[x]++"-=V"++[y]
    ['8',x,y,'6'] -> "["++showHex ins ""++"] Shift V"++[x]++" a la derecha"
    ['8',x,y,'7'] -> "["++showHex ins ""++"] V"++[x]++"=V"++[y]++"-V"++[x]
    ['8',x,y,'e'] -> "["++showHex ins ""++"] Shift V"++[x]++" a la izquierda"
    ['9',x,y,'0'] -> "["++showHex ins ""++"] Salta instruccion si V"++[x]++"/=V"++[y]
    ('a':n)       -> "["++showHex ins ""++"] Settea I a "++nn
    ('b':n)       -> "["++showHex ins ""++"] Salta a "++nn++" + V0"
    ('c':x:n)     -> "["++showHex ins ""++"] V"++[x]++"="++kk++" & (random)"
    ('d':x:y:n)   -> "["++showHex ins ""++"] Dibuja un sprite en V"++[x]++", V"++[y]++" de altura "++hh
    ('e':x:"9e")  -> "["++showHex ins ""++"] Saltea si la tecla en V"++[x]++" esta apretada"
    ('e':x:"a1")  -> "["++showHex ins ""++"] Saltea si la tecla en V"++[x]++" no esta apretada"
    ('f':x:"07")  -> "["++showHex ins ""++"] V"++[x]++"= delay timer"
    ('f':x:"0a")  -> "["++showHex ins ""++"] Espera una tecla y la guarda en V"++[x]
    ('f':x:"15")  -> "["++showHex ins ""++"] delay timer = V"++[x]
    ('f':x:"18")  -> "["++showHex ins ""++"] sound timer = V"++[x]
    ('f':x:"1e")  -> "["++showHex ins ""++"] Agrega V"++[x]++" a I"
    ('f':x:"29")  -> "["++showHex ins ""++"] I =  posicion del caracter V"++[x]++" (font)"
    ('f':x:"33")  -> "["++showHex ins ""++"] Guarda centenas de V"++[x]++" (decimal) en I, decenas en I+1, unidades en I+2"
    ('f':x:"55")  -> "["++showHex ins ""++"] Guarda valores de V0 a V"++[x]++" inclusive en memoria, empezando en I"
    ('f':x:"65")  -> "["++showHex ins ""++"] Settea valores de V0 a V"++[x]++" con los valores en memoria empezando en I"
    _            -> "["++showHex ins ""++"] Instruccion invalida (dec: "++ show ins++", hex:)"
    where xx =show $ getX ins
          yy =show $ getY ins
          nn =show $ getN ins
          kk =show $ getK ins
          hh =show $ getH ins

--Manejo de registros
getReg :: State -> Int -> Word8
getReg State{ vx = vx } x = getAt x vx
setReg :: State -> Int -> Word8 -> State
setReg s@State{ vx = vx } x v = s { vx = replaceAt x vx v }
addToReg :: State -> Int -> Word8 -> State
addToReg s x v = setReg s x (getReg s x + v)
subFromReg :: State -> Int -> Word8 -> State
subFromReg s x v = addToReg s x (-v)

shiftRReg :: State -> Int -> State
shiftRReg s x = setReg (setReg s x (shiftR vx 1)) 0xf (vx.&.1)
  where vx = getReg s x
shiftLReg :: State -> Int -> State
shiftLReg s x = setReg (setReg s x (shiftL vx 1)) 0xf (shiftR (vx.&.128) 7)
  where vx = getReg s x

setI :: State -> Word16 -> State
setI s v = s{i=v}

sumReg :: State -> Int -> Int -> State
sumReg s x y
  |v > 255 = setReg (setReg s x (asWord8 v)) 0xf 1
  |otherwise =  setReg (setReg s x (asWord8 v)) 0xf 0
  where v = asInt (getReg s x) + asInt (getReg s y)

subReg :: State -> Int -> Int -> Int -> State
subReg s x y d
  |vx > vy = setReg (setReg s d v) 0xf 1
  |otherwise = setReg (setReg s d (v-255)) 0xf 0
  where vx = getReg s x
        vy = getReg s y
        v = vx-vy



--Manejo de program counter
advancePC :: State -> State
advancePC  s@State{pc = pc} = s{pc = pc+2}
setPC s v = s{pc=v}

--Manejo de pantalla
clearScreen :: SDL.Surface -> IO ()
clearScreen s = do
  screenRect <- SDL.getClipRect s
  SDL.fillRect  s (Just screenRect) (SDL.Pixel bgColor)
  return()

drawScreen :: SDL.Surface -> [[Bool]] -> IO()
drawScreen s d = do
  clearScreen s
  go 0 0 d
  where
    drawPixel s x y = SDL.fillRect s (Just (SDL.Rect (x*pixelSize) (y*pixelSize) pixelSize pixelSize)) (SDL.Pixel fgColor)
    go _ _ [] = SDL.flip s
    go x y ([]:r) = go 0 (y+1) r
    go x y ((True:ps):r) = do
      drawPixel s x y
      go (x+1) y (ps:r)
    go x y ((False:ps):r) = go (x+1) y (ps:r)


togglePixel p x y = replaceAt y p (replaceAt x (getAt y p) (not (getPixel p x y)))

getPixel :: [[Bool]] -> Word8 -> Word8 -> Bool
getPixel p x y = getAt x (getAt y p)


drawSprite :: State -> Word8 -> Word8 -> [[Bool]] -> IO (State, Bool)
drawSprite state x y sprite = go (pixels state) 0 0 sprite False
  where
    go p _ _ [] c = return (state {pixels = p},c)
    go p _ dy ([]:r) c = go p 0 (dy+1) r c
    go p dx dy ((False:ps):r) c = go p (dx+1) dy (ps:r) c
    go p dx dy ((True:ps):r) c = do
      let px = mod (x+dx) (asWord8 pixelsW)
      let py = mod (y+dy) (asWord8 pixelsH)
      go (togglePixel p px py ) (dx+1) dy (ps:r) (c || getPixel p px py)



--Manejo de memoria
loadToMem :: [Word8] -> Int -> State -> State
loadToMem [] _ s = s
loadToMem (x:xs) p s@State{mem = m} = loadToMem xs (p+1) (s{ mem = replaceAt p m x })

loadFromMem :: Int -> Int -> State -> [Word8]
loadFromMem i r s = genericTake r $drop i m
  where m = mem s


fileOpen :: FilePath -> IO [Word8]
fileOpen fp = do
  bs <- B.readFile fp
  return (B.unpack bs)

getX i = fromIntegral $ shiftR (i.&.0x0F00) 8
getY i = fromIntegral $ shiftR (i.&.0x00F0) 4
getN :: Word16 -> Word16
getN i = i.&.0x0FFF
getK :: Word16 -> Word8
getK i = fromIntegral (i.&.0xFF)
getH :: Word16 -> Word8
getH i = fromIntegral (i.&.0xF)

getInst :: Word16 -> [Word8] -> Word16
getInst addr mem = (shiftL (fromIntegral l) 8 + fromIntegral r)::Word16
  where l = mem !! fromIntegral addr
        r = mem !! fromIntegral (addr+1)

addToStack :: Word16 -> State -> State
addToStack addr state@State{pc=pc, sp=sp, stack=stack} = state {pc=addr,sp=sp+1, stack = replaceAt sp stack pc}

removeFromStack state@State{sp=sp, stack=stack} = state {pc = getAt (sp-1) stack, sp=sp-1}

skipOn :: (a->a->Bool)->a->a->State->State
skipOn f a b s@State{pc=pc}
  | f a b = s{pc=pc+4}
  | otherwise = s{pc=pc+2}

skipIf :: Bool->State->State
skipIf True s@State{pc=pc} = s{pc=pc+4}
skipIf _ s@State{pc=pc}   = s{pc=pc+2}

-- Manejo de timers

tickDT :: State -> State
tickDT s@State{dt=0} = s
tickDT s@State{dt=dt} = s{dt=dt-1}

setDT s v = s{dt=v}

-- Manejo de teclado
keyDown :: State -> Map.Map SDL.SDLKey Int -> SDL.SDLKey -> State
keyDown s km k
  | pos == -1 = s
  | otherwise = s{keyboard = replaceAt pos (keyboard s) True}
  where pos = fromMaybe 0 (Map.lookup k km)

keyUp :: State -> Map.Map SDL.SDLKey Int -> SDL.SDLKey -> State
keyUp s km k
  | pos == -1 = s
  | otherwise = s{keyboard = replaceAt pos (keyboard s) False}
  where pos = fromMaybe 0 (Map.lookup k km)

-- Utilidad
getAt x l = l !! fromIntegral x

replaceAt x l v = y ++ [v] ++ z
  where ix = fromIntegral x
        (y,z) = (genericTake ix l, drop (ix+1) l)
addAt x l v = replaceAt x l (v + getAt x l)
toBitsArray :: Word8 -> [Bool]
toBitsArray x = map (testBit x) [7,6..0]
asInt x = fromIntegral x::Int
asWord8 x = fromIntegral x::Word8
asWord16 x = fromIntegral x::Word16
