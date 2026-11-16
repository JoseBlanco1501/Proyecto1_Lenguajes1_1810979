-- | Módulo: Main
-- Descripción: Punto de entrada del ejecutable aventura, gestión de CLI y bucle interactivo.
module Main (main) where

import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)
import System.IO (hFlush, hIsEOF, stdin, stdout)

import Motor.Comandos (leerComando)
import Motor.Nucleo (describirSala, ejecutar, estadoInicial, metaCumplida)
import Motor.Persistencia (cargarPartida, guardarPartida, leerArchivoMundo)
import Motor.Tipos (Comando (..), Estado (..), Mundo (..), mostrarError)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-validar", ruta]  -> ejecutarValidacion ruta
    ["--validar", ruta] -> ejecutarValidacion ruta
    ["\8211validar", ruta] -> ejecutarValidacion ruta -- Soporte para en-dash '–validar'
    [ruta]              -> iniciarAventura ruta
    _                   -> do
      putStrLn "Uso: aventura [-validar] ARCHIVO_MUNDO"
      exitWith (ExitFailure 1)

-- | Ejecuta la comprobación del mundo e informa de su validez o lista sus errores.
ejecutarValidacion :: FilePath -> IO ()
ejecutarValidacion ruta = do
  res <- leerArchivoMundo ruta
  case res of
    Left errs -> do
      mapM_ (putStrLn . mostrarError) errs
      exitWith (ExitFailure 1)
    Right _ -> do
      putStrLn "Mundo válido."
      exitSuccess

-- | Inicia la partida cargando el mundo e ingresando al bucle de juego.
iniciarAventura :: FilePath -> IO ()
iniciarAventura ruta = do
  res <- leerArchivoMundo ruta
  case res of
    Left errs -> do
      mapM_ (putStrLn . mostrarError) errs
      exitWith (ExitFailure 1)
    Right mundo -> do
      putStrLn (mundoTitulo mundo)
      let est0 = estadoInicial mundo
      mapM_ putStrLn (describirSala est0)
      bucleJuego est0

-- | Bucle principal del juego. Lee comandos línea a línea hasta ganar, salir o EOF.
bucleJuego :: Estado -> IO ()
bucleJuego est
  | metaCumplida est = do
      putStrLn "*** ¡Has ganado! ***"
      exitSuccess
  | estTerminado est =
      exitSuccess
  | otherwise = do
      putStr "> "
      hFlush stdout
      finDeEntrada <- hIsEOF stdin
      if finDeEntrada
        then exitSuccess
        else do
          linea <- getLine
          case leerComando linea of
            Nothing -> do
              putStrLn "No entiendo ese comando. Escribe 'ayuda' para ver las opciones disponibles."
              bucleJuego est

            Just (Guardar arch) -> do
              res <- guardarPartida arch est
              case res of
                Left err -> putStrLn ("Error: " ++ err)
                Right () -> putStrLn ("Partida guardada exitosamente en '" ++ arch ++ "'.")
              bucleJuego est

            Just (Cargar arch) -> do
              res <- cargarPartida arch (estMundo est)
              case res of
                Left err -> do
                  putStrLn ("Error al cargar partida: " ++ err)
                  bucleJuego est
                Right nuevoEst -> do
                  putStrLn ("Partida cargada desde '" ++ arch ++ "'.")
                  mapM_ putStrLn (describirSala nuevoEst)
                  bucleJuego nuevoEst

            Just cmd -> do
              let (mensajes, nuevoEst) = ejecutar cmd est
              mapM_ putStrLn mensajes
              bucleJuego nuevoEst
