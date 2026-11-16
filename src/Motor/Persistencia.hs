-- | Módulo: Motor.Persistencia
-- Descripción: Operaciones de entrada/salida para lectura de mundos y persistencia de partidas.
module Motor.Persistencia
  ( leerArchivoMundo
  , guardarPartida
  , cargarPartida
  ) where

import Control.Exception (IOException, catch)
import qualified Data.Map.Strict as Map
import System.Directory (doesFileExist)
import System.IO (IOMode (ReadMode, WriteMode), hGetContents, hPutStrLn, hSetEncoding, utf8, withFile)
import Text.Read (readMaybe)

import Motor.Mundo (leerMundo)
import Motor.Tipos

-- | Lee un archivo de mundo desde disco con codificación UTF-8 y lo procesa con leerMundo.
leerArchivoMundo :: FilePath -> IO (Either [ErrorMundo] Mundo)
leerArchivoMundo ruta = do
  existe <- doesFileExist ruta
  if not existe
    then pure (Left [ErrorSintaxis 0 ("El archivo no existe: " ++ ruta)])
    else do
      contenido <- withFile ruta ReadMode $ \h -> do
        hSetEncoding h utf8
        c <- hGetContents h
        -- Forzamos la evaluación para no dejar el handle abierto
        _ <- length c `seq` pure ()
        pure c
      pure (leerMundo contenido)

-- | Guarda el estado de la partida en el archivo especificado.
guardarPartida :: FilePath -> Estado -> IO (Either String ())
guardarPartida ruta est =
  let pg = PartidaGuardada
        { pgTituloMundo  = mundoTitulo (estMundo est)
        , pgUbicacion    = estUbicacion est
        , pgInventario   = estInventario est
        , pgObjetosSalas = Map.toList (estObjetosSalas est)
        }
      contenido = show pg
      accion = withFile ruta WriteMode $ \h -> do
        hSetEncoding h utf8
        hPutStrLn h contenido
        pure (Right ())
  in catch accion (\e -> pure (Left ("No se pudo guardar la partida: " ++ show (e :: IOException))))

-- | Carga una partida guardada previamente comprobando consistencia con el mundo actual.
-- Nunca lanza excepciones no capturadas.
cargarPartida :: FilePath -> Mundo -> IO (Either String Estado)
cargarPartida ruta mundo = do
  existe <- doesFileExist ruta
  if not existe
    then pure (Left ("El archivo '" ++ ruta ++ "' no existe."))
    else do
      resLectura <- catch
        (withFile ruta ReadMode $ \h -> do
           hSetEncoding h utf8
           c <- hGetContents h
           _ <- length c `seq` pure ()
           pure (Right c))
        (\e -> pure (Left ("Error de lectura al cargar partida: " ++ show (e :: IOException))))
      case resLectura of
        Left err -> pure (Left err)
        Right contenido ->
          case readMaybe contenido of
            Nothing ->
              pure (Left "El archivo de partida está corrupto o su formato es irreconocible.")
            Just pg ->
              if pgTituloMundo pg /= mundoTitulo mundo
                then pure (Left ("La partida guardada pertenece al mundo '" ++ pgTituloMundo pg
                                 ++ "', pero el actual es '" ++ mundoTitulo mundo ++ "'."))
                else if not (Map.member (pgUbicacion pg) (mundoSalas mundo))
                       then pure (Left "La partida guardada contiene una sala que no existe en el mundo actual.")
                       else
                         let mapaRestaurado = Map.fromList (pgObjetosSalas pg)
                             estadoRestaurado = Estado
                               { estMundo        = mundo
                               , estUbicacion    = pgUbicacion pg
                               , estInventario   = pgInventario pg
                               , estObjetosSalas = mapaRestaurado
                               , estTerminado    = False
                               }
                         in pure (Right estadoRestaurado)
