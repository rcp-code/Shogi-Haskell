module Reglas where

    {----------------------------------------------------------------------
            Reglas del juego relacionadas con la promoción de fichas
                y los movimientos de estas, entre otras cosas
    ----------------------------------------------------------------------}

import Prelude as P
import Data.Matrix as M
import Data.Vector as V

-- ---------------------------------------------------------------------------------------

-- Tipos definidos (con type y data)
type Ficha = String     -- Identifica si es Rey, General de Oro, General de Plata, etc.
type World = M.Matrix Ficha
type Fila = V.Vector Ficha

-- Tipos de movimientos que pueden hacer las fichas
data Movimiento = HaciaAdelante Int |
                  HaciaAtras Int |
                  HaciaDerecha Int |
                  HaciaIzquierda Int |
                  EnDiagonalP Int |
                  EnDiagonalS Int |
                  Especial Int Int  --Solo para el Caballo
            deriving (Eq)

-- ---------------------------------------------------------------------------------------

-- ---------------------------------------------------------------------------------------

--               Funciones básicas de comprobación y promoción de fichas

-- ---------------------------------------------------------------------------------------

-- Promocionan todas las fichas, salvo el Rey y el General Dorado (uso de case of)
esPromocionable :: Ficha -> Bool
esPromocionable ficha = case ficha of
                        "BRey" -> False
                        "BG.Oro" -> False
                        "NRey" -> False
                        "NG.Oro" -> False
                        _ -> True

{- Una ficha puede promocionar solo si se encuentra en las tres filas donde se encuentran las fichas del rival
al inicio del juego (o "arriba" o "abajo"). Además hay que comprobar si la ficha es promocionable/coronable
o no, es decir, si la ficha no es Rey/General Dorado sí podrá promocionar -}
puedePromocionar :: (Float, Float) -> Ficha -> Bool
puedePromocionar p ficha = esPromocionable ficha && compruebaPosTablero p ficha

-- Comprueba la posición para saber si la ficha puede o no promocionar (uso de case of)
compruebaPosTablero :: (Float, Float) -> Ficha -> Bool
compruebaPosTablero p ficha = case p of
                                    (_,-263) -> 'B' `P.elem` ficha --solo pueden promocionar fichas blancas
                                    (_,-197) -> 'B' `P.elem` ficha --solo pueden promocionar fichas blancas
                                    (_,-131) -> 'B' `P.elem` ficha --solo pueden promocionar fichas blancas
                                    (_,133) -> 'N' `P.elem` ficha  --solo pueden promocionar fichas negras
                                    (_,199) -> 'N' `P.elem` ficha  --solo pueden promocionar fichas negras
                                    (_,265) -> 'N' `P.elem` ficha  --solo pueden promocionar fichas negras
                                    _ -> False


-- Una vez que la ficha promociona o es coronada, no puede volver a ser coronada 
-- (es decir, al transformarse en Rey o General Dorado, no puede promocionar más)
promocionFicha :: (Float, Float) -> Ficha -> Ficha
promocionFicha p ficha
    | puedePromocionar p ficha = promociona ficha --si puede promocionar, se intercambian los elementos del par
    | otherwise = ficha        --si es Rey o General Dorado, la ficha se queda igual

-- Uso de case of:
promociona :: Ficha -> Ficha
promociona ficha = case ficha of
                    "BLancero" -> "BG.Oro"
                    "BCaballo" -> "BG.Oro"
                    "BTorre" -> "BRey"
                    "BAlfil" -> "BRey"
                    "BG.Plata" -> "BG.Oro"
                    "BPeon" -> "BG.Oro"
                    "NLancero" -> "NG.Oro"
                    "NCaballo" -> "NG.Oro"
                    "NTorre" -> "NRey"
                    "NAlfil" -> "NRey"
                    "NG.Plata" ->"NG.Oro"
                    "NPeon" -> "NG.Oro"

--Asigna movimiento a una ficha según sus restricciones (patrones y guardas)
asignaMovimiento :: Int -> Int -> Int -> Ficha -> Movimiento -> (Int, Int)
asignaMovimiento _ x y "Rey" mov       -- Se mueve una casilla en cualquier dirección
    | mov==HaciaAdelante 1 = (x,y+1)
    | mov==HaciaAtras 1 = (x,y-1)
    | mov==HaciaDerecha 1 = (x+1,y)
    | mov==HaciaIzquierda 1 = (x-1,y)
    | mov==EnDiagonalP 1 = (x+1,y-1)
    | mov==EnDiagonalS 1 = (x+1,y+1)
    | mov==EnDiagonalP (-1) = (x-1,y+1)
    | mov==EnDiagonalS (-1) = (x-1,y-1)
    | otherwise = (0,0)
asignaMovimiento n x y "Torre" mov     -- Puede desplazarse cualquier número de casillas vertical u horizontalmente
    | n>=1 && n<=9 && mov==HaciaAdelante n = (x,y+n)
    | n>=1 && n<=9 && mov==HaciaAtras n = (x,y-n)
    | n>=1 && n<=9 && mov==HaciaDerecha n = (x+n,y)
    | n>=1 && n<=9 && mov==HaciaIzquierda n = (x-n,y)
    | otherwise = (0,0)
asignaMovimiento n x y "Alfil" mov     -- Puede desplazarse cualquier número de casillas en diagonal
    | n>=1 && n<=9 && mov==EnDiagonalP n = (x-n,y+n)
    | n>=1 && n<=9 && mov==EnDiagonalS n = (x+n,y+n)
    | n>=1 && n<=9 && mov==EnDiagonalP (-n) = (x+n,y-n)
    | n>=1 && n<=9 && mov==EnDiagonalS (-n) = (x-n,y-n)
    | otherwise = (0,0)
asignaMovimiento _ x y "G.Oro" mov     -- Una casilla en vertical, horizontal o diagonalmente hacia adelante
    | mov==HaciaAdelante 1 = (x,y+1)
    | mov==HaciaAtras 1 = (x,y-1)
    | mov==HaciaDerecha 1 = (x+1,y)
    | mov==HaciaIzquierda 1 = (x-1,y)
    | mov==EnDiagonalP 1 = (x+1,y-1)
    | mov==EnDiagonalS 1 = (x+1,y+1)
    | otherwise = (0,0)
asignaMovimiento _ x y "G.Plata" mov       -- Una casilla en diagonal, o bien vertical hacia adelante
    | mov==HaciaAdelante 1 = (x,y+1)
    | mov==EnDiagonalP 1 = (x+1,y-1)
    | mov==EnDiagonalS 1 = (x+1,y+1)
    | mov==EnDiagonalP (-1) = (x-1,y+1)
    | mov==EnDiagonalS (-1) = (x-1,y-1)
    | otherwise = (0,0)
asignaMovimiento _ x y "Caballo" mov   -- El caballo NO puede desplazarse hacia atrás. Su movimiento consiste en dos casillas hacia adelante y una hacia la izquierda o la derecha
    | mov==Especial 2 (-1) = (x-1,y+2)
    | mov==Especial 2 1 = (x+1,y+2)
    | otherwise = (0,0)
asignaMovimiento n x y "Lancero" mov   -- Se mueve cualquier número de casillas verticalmente hacia adelante
    | n>=1 && n<=9 && mov==HaciaAdelante n = (x,y+n)
    | otherwise = (0,0)
asignaMovimiento _ x y "Peon" mov      -- Solo se mueve una casilla adelante
    | mov==HaciaAdelante 1 = (x,y+1)
    | otherwise = (0,0)

-- Comprueba que sólo haya un peón propio por columna (uso de guardas, orden superior, Data.Prelude y Data.Vector)
compruebaNifu :: World -> Bool
compruebaNifu mundo
    | maxPeonesNegros>1 = True      --hay peones negros en la misma columna
    | maxPeonesBlancos>1 = True     --hay peones blancos en la misma columna
    | otherwise = False
    where cols = obtieneColumnas mundo
          peonesNegros = [V.length (V.filter (=="NPeon") c) | c<-cols]
          peonesBlancos = [V.length (V.filter (=="BPeon") c) | c<-cols]
          maxPeonesNegros = P.maximum peonesNegros
          maxPeonesBlancos = P.maximum peonesBlancos

-- Funciones auxiliares recursivas
obtieneColumnas :: World -> [V.Vector Ficha]
obtieneColumnas = obtieneCols 9

obtieneCols :: Int -> World -> [V.Vector Ficha]
obtieneCols 0 _ = []
obtieneCols i mundo
    | i>M.ncols mundo = obtieneCols (i-1) mundo
    | otherwise = M.getCol i mundo:obtieneCols (i-1) mundo

obtieneFilas :: World -> [Fila]
obtieneFilas = obtieneFilas' 9

obtieneFilas' :: Int -> World -> [Fila]
obtieneFilas' 0 _ = []
obtieneFilas' i mundo
    | i>M.nrows mundo = obtieneFilas' (i-1) mundo
    | otherwise = M.getRow i mundo:obtieneFilas' (i-1) mundo

-- Comprueba si se ha hecho Jaque al Rey (guardas, Data.Vector, orden superior y uso de let)
-- (es decir, si alguna de las fichas rivales está en posición de capturar al Rey)
enJaque :: World -> Bool
enJaque mundo
    | V.length rnf==0 || V.length rbf==0 || V.length rnc==0 || V.length rbc==0 = False
    | otherwise = True
    where cols = obtieneColumnas mundo
          filas = obtieneFilas mundo
          rnf = V.concat [V.filter (=="NRey") f | f<-filas]
          rbf = V.concat [V.filter (=="BRey") f | f<-filas]
          rnc = V.concat [V.filter (=="NRey") c | c<-cols]
          rbc = V.concat [V.filter (=="BRey") c | c<-cols]

-- enJaque :: World -> Bool
-- enJaque mundo
--     | jaqueNegroPorC == False || jaqueNegroPorF == False = False --comprueba si hay jaque al rey de las negras
--     | jaqueBlancoPorC == False || jaqueBlancoPorF == False = False  --comprueba si hay jaque al rey de las blancas
--     | otherwise = True
--     where cols = obtieneColumnas mundo
--           filas = obtieneFilas mundo
--           rnf = V.concat [V.filter (=="NRey") f | f<-filas]
--           rbf = V.concat [V.filter (=="BRey") f | f<-filas]
--           rnc = V.concat [V.filter (=="NRey") c | c<-cols]
--           rbc = V.concat [V.filter (=="BRey") c | c<-cols]
--           jaqueNegro f1 f2 = if (f1=="NRey" && f2=="") || (f1=="" && f2=="NRey") then False else True
--           jaqueBlanco f1 f2 = if (f1=="BRey" && f2=="") || (f1=="" && f2=="BRey") then False else True
--           jaqueNegroPorC = V.any (==True) [if (jaqueNegro f1 f2) then True else False | (f1,f2)<-V.zip rnc (V.tail rnc)]
--           jaqueBlancoPorC = V.any (==True) [if (jaqueBlanco f1 f2) then True else False | (f1,f2)<-V.zip rbc (V.tail rbc)]
--           jaqueNegroPorF = V.any (==True) [if (jaqueNegro f1 f2) then True else False | (f1,f2)<-V.zip rnf (V.tail rnf)]
--           jaqueBlancoPorF = V.any (==True) [if (jaqueBlanco f1 f2) then True else False | (f1,f2)<-V.zip rbf (V.tail rbf)]

-- Comprueba si se ha llegado al Jaque Mate 
-- (si alguna de las fichas rivales está en posición de capturar al Rey y este no tiene escapatoria)
-- enJaqueMate :: World -> Bool
-- enJaqueMate mundo = undefined
--     where cols = obtieneColumnas mundo
--           filas = obtieneFilas mundo
