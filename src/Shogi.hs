{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Shogi(main) where

    {----------------------------------------------------------------------------
        Fichero en el que se incluirá la implementación en Gloss del juego 
        y de las reglas, así como el main en el que se ejecutará el juego.
    ----------------------------------------------------------------------------}

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Prelude as P
import Data.Matrix as M
import Data.Vector as V
import Reglas


-- ---------------------------------------------------------------------------------------

-- Variables globales
limiteTablero :: Float
limiteTablero = 265 -- Punto recomendado para el límite del tablero (positivo o negativo)
listaPosicionamientoX :: [Float]
listaPosicionamientoX = [-265, -199 .. 265] -- Lista de los puntos en que se ponen como string en la métrica del eje X
listaPosicionamientoY :: [Float]
listaPosicionamientoY = [265, 199 .. -265] -- Lista de los puntos en que se ponen como string en la métrica del eje Y
listaPeonesB :: [Ficha]
listaPeonesB = ["BPeon" | _<-[0..8]] -- Lista con los string para los peones blancos
listaPeonesN :: [Ficha]
listaPeonesN = ["NPeon" | _<-[0..8]] -- Lista con los string para los peones negros
-- Lista con los string para las figuras grandes blancas
ordenGrandesB :: [Ficha]
ordenGrandesB = ["BLancero", "BCaballo", "BG.Plata", "BG.Oro", "BRey", "BG.Oro", "BG.Plata", "BCaballo", "BLancero"]
-- Lista con los string para las figuras grandes negras
ordenGrandesN :: [Ficha]
ordenGrandesN = ["NLancero", "NCaballo", "NG.Plata", "NG.Oro", "NRey", "NG.Oro", "NG.Plata", "NCaballo", "NLancero"]
nombresAyTLB :: [Ficha]
nombresAyTLB = ["", "BAlfil", "", "", "", "", "", "BTorre", ""] -- Lista con los string de los alfiles y las torres blancas
nombresAyTLN :: [Ficha]
nombresAyTLN = ["", "NAlfil", "", "", "", "", "", "NTorre", ""] -- Lista con los string de los alfiles y las torres negras
listaStringVacios :: [String]
listaStringVacios = ["" | _<-[0..8]] -- Lista para las filas vacías iniciales
-- Lista que auna todas las listas de nombres anteriores en el orden correcto
listaDeListasConNombres :: [[Ficha]]
listaDeListasConNombres = [ordenGrandesB, nombresAyTLB, listaPeonesB, listaStringVacios, listaStringVacios, listaStringVacios, listaPeonesN, nombresAyTLN, ordenGrandesN]
-- Lista con las posiciones posibles del tablero (aún no ordenadas de forma correcta)
listaDePosiciones :: [(Float,Float)]
listaDePosiciones = [(x,y) | x<-listaPosicionamientoX, y<-listaPosicionamientoY]
-- Creamos la matriz de posiciones y la transponemos para conseguir el orden correcto
matrizPosiciones :: M.Matrix (Float, Float)
matrizPosiciones = transpose $ M.fromList 9 9 listaDePosiciones
-- Creamos la matriz de nombres
mundoInicial :: World
mundoInicial = M.fromLists listaDeListasConNombres


    {----------------------------------------------------------------------------

                           Implementación gráfica en Gloss

    ----------------------------------------------------------------------------}

-- ---------------------------------------------------------------------------------------

--                          Funciones básicas del programa

-- ---------------------------------------------------------------------------------------

-- Main del programa
main :: IO ()
main = play ventanaJuego fondo 2 inicial dibujaMundo manejaEntrada actualiza

-- Ventana en la que se mostrará el juego
ventanaJuego :: Display
ventanaJuego = InWindow "Shogi" (1400, 700) (50, 40)

-- Color del fondo de la ventana
fondo :: Color
fondo = white

-- Matriz inicial con el orden de las fichas dispuestas para comenzar una partida nueva
inicial :: World
inicial = mundoInicial

-- Función principal que dibuja la matriz que se le pase. Con esta función es como realmente se actualiza el juego
dibujaMundo :: World -> Picture
dibujaMundo mundo = pictures [cuadricula, fichas]
    where fichas = pictures [fichasJuego i (M.getRow i mundo) | i<-[1..9]]

-- Función para manejar las interacciones juego-usuario (uso de guardas)
manejaEntrada :: Event -> World -> World
manejaEntrada evento mundo
    | EventKey (MouseButton LeftButton) _ _ _ <- evento = manejaEvento evento mundo
    | otherwise = mundo

manejaEvento :: Event -> World -> World
manejaEvento (EventKey (MouseButton LeftButton) Down _ raton@(x,y)) mundo
    | existeCasilla raton && hayFicha raton mundo = pintaMovimientos raton mundo
    | otherwise = mundo
manejaEvento (EventKey (MouseButton LeftButton) Up _ raton@(x,y)) mundo
    | existeCasilla raton && movimientoPosible raton mundo && (encuentraS mundo /= (0,0)) = mundoTrasMovimiento raton mundo
    | otherwise = mundo

{- Función de actualización del mundo
No hace mucho porque el mundo no cambia con el tiempo, sino con los movimientos de las fichas -}
actualiza :: Float -> World -> World
actualiza _ = id

-- ---------------------------------------------------------------------------------------

--                      Funciones relacionadas con dibujaMundo

-- ---------------------------------------------------------------------------------------

-- Dibuja las fichas a partir de una fila dada (Data.Prelude, Data.Vector y Data.Matrix)
fichasJuego :: Int -> Fila -> Picture
fichasJuego f fila = pictures [gestionaFicha s x y | (s,(x,y))<-P.zip ss ps, s/=""]
    where ss = V.toList fila
          ps = V.toList $ M.getRow f matrizPosiciones

-- Realiza la toma de decisiones dependiendo del tipo de ficha que se vaya a dibujar (uso de guardas y patrones)
gestionaFicha :: Ficha -> Float -> Float -> Picture
gestionaFicha "" _ _ = translate 900 900 $ color green $ circleSolid 0.1
gestionaFicha "P" x y = translate x y puntoVerde
gestionaFicha s x y
    | primera == 'S' = gestionaFicha resto x y
    | primera == 'B' = translate x y $ rotate 180 $ ficha resto
    | primera == 'P' = pictures [gestionaFicha resto x y,translate x y puntoVerde]
    | otherwise = translate x y $ ficha resto
        where primera = P.head s
              resto = P.tail s

-- Dibuja un punto verde
puntoVerde :: Picture
puntoVerde = color green $ circleSolid 0.1

-- Dibuja una ficha de shogi con su nombre
ficha :: Ficha -> Picture
ficha s = pictures [coloreada,nombrada]
    where forma = polygon [(0,25),(20,15),(20,-25),(-20,-25),(-20,15)] -- Para las fichas del shogi creamos nuestro propio polígono
          coloreada = color black forma
          nombrada = nombra s

-- Función auxiliar para escribir los nombres de las fichas (uso de composición de funciones)
nombra :: String -> Picture
nombra = translate (-16) 0 . scale 0.08 0.08 . color white . text

-- ---------------------------------------------------------------------------------------

--          Funciones para manejar los inputs, relacionadas con manejaEntrada

-- ---------------------------------------------------------------------------------------

-- Comprueba si se ha presionado un lugar dentro del juego (uso de guardas)
existeCasilla :: (Float, Float) -> Bool
existeCasilla (x,y)
    | x > 300 || x < -300 || y > 300 || y < -300 = False
    | otherwise = True

-- Comprueba si hay una ficha de shogi en el lugar clickeado (uso de guardas)
hayFicha :: (Float, Float) -> World -> Bool
hayFicha raton mundo
    | pos == (0,0) = False
    | nombreFicha == "" = False
    | nombreFicha == "P" = False
    | otherwise = True
        where pos = consigueFilaYColumna raton
              nombreFicha = uncurry getElem pos mundo

-- Comprueba y/o restringe los movimientos de las fichas del juego (uso de guardas)
-- TERMINAR
movimientoPosible :: (Float, Float) -> World -> Bool
movimientoPosible _ _ = True
movimientoPosible raton@(x,y) mundo
    | not (existeCasilla raton) || encuentraS mundo == (0,0) || compruebaNifu mundo = False
    | enJaque mundo = False --solo se permite mover al rey para salir del jaque
    | otherwise = True

-- Cambia la matriz con las nuevas modificaciones tras un movimiento
mundoTrasMovimiento :: (Float, Float) -> World -> World
mundoTrasMovimiento raton mundo = setElem nombreFicha pos $ setElem "" fichaPos mundo
    where pos = consigueFilaYColumna raton
          fichaPos = encuentraS mundo
          nombreFicha = P.tail $ uncurry getElem fichaPos mundo   --obtiene el nombre de la ficha (sin el primer caracter que identifica a Negras o Blancas)

-- Añade las letras claves en los nombres de las fichas señaladas con el ratón (orden superior, Data.Matrix y Data.Prelude)
-- INCOMPLETA hasta que termine la parte de reglas y modifique esta función
pintaMovimientos :: (Float, Float) -> World -> World
pintaMovimientos raton = M.mapPos (\fyc ficha -> if fyc `P.elem` listaPosiblesMovimientos
        then if fyc==pos then 'S':('P':ficha) else 'P':ficha else ficha)
    where pos = consigueFilaYColumna raton
          f = fst pos
          c = snd pos
          listaPosiblesMovimientos = [(f+i,c+j) | i<-[-1..1],j<-[-1..1],f+i>=1 && f+i<=9 && c+j>=1 && c+j<=9]

-- Obtiene la fila y columna más cercana al punto clickeado con el ratón (uso de guardas, orden superior, Data.Prelude y Data.Matrix)
consigueFilaYColumna :: (Float, Float) -> (Int,Int)
consigueFilaYColumna (x,y)
    | P.null listaDropeada = (0,0)
    | otherwise = P.head listaDropeada
    where matrizDistancias = M.mapPos (\_ (a,b) -> abs (a-x)+abs (b-y)) matrizPosiciones
          masCercana = P.minimum $ M.toList $ M.mapPos (\_ n -> n) matrizDistancias
          listaDeFilasYColumnas = M.toList $ M.mapPos (\(r,c) n -> if n==masCercana then (r,c) else (0,0)) matrizDistancias
          listaDropeada = P.dropWhile (==(0,0)) listaDeFilasYColumnas

-- Obtiene la fila y columna en la que está la ficha previamente seleccionada para mover (uso de guardas, orden superior y Data.Prelude)
encuentraS :: World -> (Int, Int)
encuentraS mundo
    | P.null listaDropeada = (0,0)
    | otherwise = P.head listaDropeada
    where listaDropeada = P.dropWhile (==(0,0)) (M.toList (M.mapPos
                (\(r,c) s -> if P.null s then (0,0)
                                else if P.head s=='S'
                                    then (r,c)
                                    else (0,0)) mundo))

-- Función que comprueba la ficha y su posición, y la mueve según lo establecido en el Shogi (uso de case of)
-- movimientoFicha :: (Float, Float) -> World -> World
-- movimientoFicha raton mundo = case nombreFicha of
--                         "Peon" -> M.setElem nombreFicha (x, y+1) $ setElem "" fichaPos mundo
--                         "Lancero" -> M.setElem nombreFicha (x, y) $ setElem "" fichaPos mundo
--                         "Caballo" -> M.setElem nombreFicha (x+1,y+2) $ setElem "" fichaPos mundo
--                         "G.Plata" -> M.setElem nombreFicha (x, y) $ setElem "" fichaPos mundo
--                         "G.Oro" -> M.setElem nombreFicha (x, y) $ setElem "" fichaPos mundo
--                         "Alfil" -> M.setElem nombreFicha (x, y) $ setElem "" fichaPos mundo
--                         "Torre" -> M.setElem nombreFicha (x, y) $ setElem "" fichaPos mundo
--                         "Rey" -> M.setElem nombreFicha (x, y) $ setElem "" fichaPos mundo
--                         _ -> mundo
--     where pos = consigueFilaYColumna raton  --obtiene la fila y columna más cercanas al lugar donde se ha hecho clic
--           x = fst pos
--           y = snd pos
--           fichaPos = encuentraS mundo   --posición de la ficha
--           nombreFicha = P.tail $ getElem (fst fichaPos) (snd fichaPos) mundo --obtiene el nombre de la ficha sin el prefijo N o B

-- ---------------------------------------------------------------------------------------

--              Funciones relacionadas con la creación del tablero

-- ---------------------------------------------------------------------------------------

-- Creación de la cuadrícula mediante la conjunción del fondo como un cuadrado y la red de casillas (listas por comprensión)
cuadricula :: Picture
cuadricula = pictures ps
    where ps = fondoTablero:[casilla x y | x<-[-264, -198..264], y<-[-264, -198..264]]

fondoTablero :: Picture
fondoTablero = color orange (rectangleSolid 600 600)

casilla :: Float -> Float -> Picture
casilla x y = translate x y (rectangleWire 66 66)

-- ---------------------------------------------------------------------------------------

--                Métrica de posiciones orientativas para el programador
--          Más tarde se podría cambiar por el típico sistema de a..i y de 1..9

-- ---------------------------------------------------------------------------------------

-- Función para dibujar la métrica alrededor de la cuadrícula
metrica :: Picture
metrica = pictures (dibujosX P.++dibujosY)
    where dibujosY = auxMY
          dibujosX = auxMX

-- Listas por comprensión
auxMY, auxMX :: [Picture]
auxMY = [transformaMetrica x y mensaje | ((x,y),mensaje)<-P.zip lugarMensajesY listaTextosCompletaY]
    where
        lugarMensajesY :: [(Float, Float)]
        lugarMensajesY = (305,335):[(305,y) | y<-listaPosicionamientoY]
        textosY = ["y="P.++show y | y<-listaPosicionamientoY]
        textoTituloY = "Puntos para cuadrar las fichas en la y"
        listaTextosCompletaY = textoTituloY:textosY

auxMX = [transformaMetrica x y mensaje | ((x,y),mensaje)<-P.zip lugarMensajesX listaTextosCompletaX]
    where
        lugarMensajesX :: [(Float, Float)]
        lugarMensajesX = (-600,-320):[(x,-320) | x<-[-290, -224 .. 300]]
        textosX = ["x="P.++show x | x<-listaPosicionamientoX]
        textoTituloX = "Puntos para cuadrar las fichas en la x"
        listaTextosCompletaX = textoTituloX:textosX

-- Función auxiliar para las anteriores -auxMY y auxMX- (es posible que la parte de la métrica se pueda hacer más escueta)
transformaMetrica :: Float -> Float -> String -> Picture
transformaMetrica x y mensaje = translate x y $ scale 0.1 0.1 $ color black $ text mensaje

-- ---------------------------------------------------------------------------------------











