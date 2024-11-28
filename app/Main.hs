import Shogi

main :: IO()
main = do 
    putStrLn "Se está cargando el juego..."
    Shogi.main
    putStrLn "Fin del juego..."