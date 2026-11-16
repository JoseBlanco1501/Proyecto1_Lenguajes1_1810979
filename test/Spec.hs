-- | Módulo: Spec
-- Descripción: Suite de pruebas automatizadas para aventura (cabal test).
module Main (main) where

import qualified Data.Map.Strict as Map
import System.IO (hSetEncoding, stdout, utf8)

import Motor.Comandos (leerComando)
import Motor.Mundo (leerMundo)
import Motor.Nucleo (ejecutar, estadoInicial, metaCumplida)
import Motor.Tipos

-- | Función de verificación simple: si la condición no se cumple, falla con mensaje explicativo.
afirmar :: String -> Bool -> IO ()
afirmar nombre ok = do
  putStr ("  " ++ nombre ++ " ... ")
  if ok
    then putStrLn "OK"
    else do
      putStrLn "FALLO"
      error ("Prueba fallida: " ++ nombre)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  putStrLn "=== Iniciando Suite de Pruebas de Aventura ==="

  -- ---------------------------------------------------------------------------
  -- 1. Pruebas de leerComando
  -- ---------------------------------------------------------------------------
  putStrLn "\n--- Pruebas de leerComando ---"
  afirmar "leerComando: mirar"
    (leerComando "mirar" == Just Mirar)

  afirmar "leerComando: ir norte"
    (leerComando "ir norte" == Just (Ir Norte))

  afirmar "leerComando: dirección directa 'sur'"
    (leerComando "sur" == Just (Ir Sur))

  afirmar "leerComando: abreviatura 'o'"
    (leerComando "o" == Just (Ir Oeste))

  afirmar "leerComando: sinónimo coger"
    (leerComando "coger moneda" == Just (Tomar (IdObjeto "moneda")))

  afirmar "leerComando: sinónimo inv"
    (leerComando "inv" == Just Inventario)

  afirmar "leerComando: variante combinar"
    (leerComando "combinar pocion con hierba" == Just (Combinar (IdObjeto "pocion") (IdObjeto "hierba")))

  afirmar "leerComando: comando inválido"
    (leerComando "volar hacia el cielo" == Nothing)

  afirmar "leerComando: entrada vacía"
    (leerComando "   " == Nothing)

  -- ---------------------------------------------------------------------------
  -- 2. Pruebas de leerMundo
  -- ---------------------------------------------------------------------------
  putStrLn "\n--- Pruebas de leerMundo sobre archivos del curso ---"
  txtEntrega <- readFile "material/mundos/entrega.txt"
  case leerMundo txtEntrega of
    Left errs -> error ("El mundo entrega.txt debería ser válido, pero dio errores: " ++ show errs)
    Right _   -> afirmar "leerMundo: entrega.txt válido" True

  txtSemantica <- readFile "material/mundos/invalido_semantica.txt"
  case leerMundo txtSemantica of
    Right _   -> error "invalido_semantica.txt debería fallar con 9 errores."
    Left errs -> afirmar "leerMundo: invalido_semantica.txt tiene exactamente 9 errores" (length errs == 9)

  txtSintaxis <- readFile "material/mundos/invalido_sintaxis.txt"
  case leerMundo txtSintaxis of
    Right _   -> error "invalido_sintaxis.txt debería fallar."
    Left errs -> afirmar "leerMundo: invalido_sintaxis.txt detecta errores sintácticos" (not (null errs))

  putStrLn "\n--- Pruebas de leerMundo sobre mundos propios ---"
  txtMundoPropio <- readFile "mundos/mundo.txt"
  case leerMundo txtMundoPropio of
    Left errs -> error ("mundos/mundo.txt debería ser válido, pero dio errores: " ++ show errs)
    Right _   -> afirmar "leerMundo: mundos/mundo.txt propio es válido" True

  txtInv1 <- readFile "mundos/invalidos/invalido1.txt"
  case leerMundo txtInv1 of
    Right _   -> error "invalido1.txt debería ser inválido."
    Left errs -> afirmar "leerMundo: invalido1.txt tiene exactamente 3 errores (incluye variante)" (length errs == 3)

  txtInv2 <- readFile "mundos/invalidos/invalido2.txt"
  case leerMundo txtInv2 of
    Right _   -> error "invalido2.txt debería ser inválido."
    Left errs -> afirmar "leerMundo: invalido2.txt tiene exactamente 3 errores" (length errs == 3)

  txtInv3 <- readFile "mundos/invalidos/invalido3.txt"
  case leerMundo txtInv3 of
    Right _   -> error "invalido3.txt debería ser inválido."
    Left errs -> afirmar "leerMundo: invalido3.txt tiene exactamente 3 errores" (length errs == 3)

  -- ---------------------------------------------------------------------------
  -- 3. Pruebas de ejecutar (pura, sin IO)
  -- ---------------------------------------------------------------------------
  putStrLn "\n--- Pruebas de ejecutar ---"
  mundoEjemplo <- case leerMundo txtEntrega of
    Right m  -> pure m
    Left err -> error ("Error inesperado: " ++ show err)
  let est0 = estadoInicial mundoEjemplo

  -- Choque con salida inexistente
  let (msgNoSalida, estTrasOeste) = ejecutar (Ir Oeste) est0
  afirmar "ejecutar: choque con salida inexistente no cambia estado"
    (estUbicacion estTrasOeste == IdSala "estacionamiento" && not (null msgNoSalida))

  -- Tomar objeto disponible
  let (_msgTomar, estConCarnet) = ejecutar (Tomar (IdObjeto "carnet")) est0
  afirmar "ejecutar: tomar objeto lo agrega a inventario"
    (elem (IdObjeto "carnet") (estInventario estConCarnet))

  -- Choque con salida bloqueada sin requisito
  let (_, estEnPlaza) = ejecutar (Ir Norte) est0 -- Va a plaza sin carnet
      (_msgBloq, estBloq) = ejecutar (Ir Oeste) estEnPlaza -- Intenta ir a biblioteca (requiere carnet)
  afirmar "ejecutar: salida bloqueada detiene al jugador"
    (estUbicacion estBloq == IdSala "plaza")

  -- Atravesar salida bloqueada llevando el requisito
  let (_, estPlazaConCarnet) = ejecutar (Ir Norte) estConCarnet
      (_, estBiblio) = ejecutar (Ir Oeste) estPlazaConCarnet
  afirmar "ejecutar: salida bloqueada permite el paso con requisito"
    (estUbicacion estBiblio == IdSala "biblioteca")

  -- Soltar objeto en la sala
  let (_, estSoltado) = ejecutar (Soltar (IdObjeto "carnet")) estBiblio
  afirmar "ejecutar: soltar objeto lo remueve de inventario y lo deja en la sala"
    (not (elem (IdObjeto "carnet") (estInventario estSoltado))
     && elem (IdObjeto "carnet") (Map.findWithDefault [] (IdSala "biblioteca") (estObjetosSalas estSoltado)))

  -- Examinar objeto
  let (msgExamen, _) = ejecutar (Examinar (IdObjeto "carnet")) estSoltado
  afirmar "ejecutar: examinar objeto en la sala"
    (not (null msgExamen))

  -- ---------------------------------------------------------------------------
  -- 4. Prueba de la Variante 3: Combinar
  -- ---------------------------------------------------------------------------
  putStrLn "\n--- Pruebas de Variante 3 (Combinar) ---"
  let recetaPrueba = Receta (IdObjeto "palo") (IdObjeto "piedra") (IdObjeto "hacha")
      mundoConReceta = mundoEjemplo { mundoRecetas = [recetaPrueba] }
      estConIngredientes = est0 { estMundo = mundoConReceta, estInventario = [IdObjeto "palo", IdObjeto "piedra"] }
      (_msgComb, estCombinado) = ejecutar (Combinar (IdObjeto "palo") (IdObjeto "piedra")) estConIngredientes

  afirmar "ejecutar: combinar consume ingredientes y entrega producto"
    (elem (IdObjeto "hacha") (estInventario estCombinado)
     && not (elem (IdObjeto "palo") (estInventario estCombinado))
     && not (elem (IdObjeto "piedra") (estInventario estCombinado)))

  -- Conmutatividad de la receta
  let (_msgCombRev, estCombinadoRev) = ejecutar (Combinar (IdObjeto "piedra") (IdObjeto "palo")) estConIngredientes
  afirmar "ejecutar: combinar es conmutativo (orden no importa)"
    (elem (IdObjeto "hacha") (estInventario estCombinadoRev))

  -- Combinar sin tener los dos ingredientes
  let estSoloPalo = est0 { estMundo = mundoConReceta, estInventario = [IdObjeto "palo"] }
      (_msgFalta, estSinCambios) = ejecutar (Combinar (IdObjeto "palo") (IdObjeto "piedra")) estSoloPalo
  afirmar "ejecutar: combinar sin ingredientes falla sin modificar inventario"
    (estInventario estSinCambios == [IdObjeto "palo"])

  -- ---------------------------------------------------------------------------
  -- 5. Secuencia completa que gana el juego (guion de entrega)
  -- ---------------------------------------------------------------------------
  putStrLn "\n--- Prueba de secuencia completa de victoria ---"
  let guionComandos =
        [ Mirar
        , Ir Oeste              -- Falla en estacionamiento
        , Tomar (IdObjeto "carnet")
        , Inventario
        , Ir Norte              -- plaza
        , Ir Oeste              -- biblioteca (requiere carnet)
        , Tomar (IdObjeto "memoria_usb")
        , Ir Este               -- plaza
        , Ir Norte              -- pasillo_mys
        , Ir Arriba             -- Falla (requiere llave_del_ascensor)
        , Ir Este               -- ldc (requiere carnet)
        , Tomar (IdObjeto "llave_del_ascensor")
        , Ir Oeste              -- pasillo_mys
        , Ir Oeste              -- fotocopiadora (requiere memoria_usb)
        , Tomar (IdObjeto "proyecto_impreso")
        , Ir Este               -- pasillo_mys
        , Ir Arriba             -- segundo_piso (requiere llave_del_ascensor)
        , Ir Norte              -- oficina_del_profesor
        ]
      estFinal = foldl (\est cmd -> snd (ejecutar cmd est)) est0 guionComandos

  afirmar "ejecutar: guión de entrega.txt alcanza la meta (victoria)"
    (metaCumplida estFinal)

  -- Secuencia de victoria sobre el mundo propio
  mundoPropio <- case leerMundo txtMundoPropio of
    Right m  -> pure m
    Left err -> error ("Error al leer mundo propio: " ++ show err)
  lineasGuionPropio <- lines <$> readFile "guiones/mundo.txt"
  let cmdsPropios = [ cmd | l <- lineasGuionPropio, Just cmd <- [leerComando l] ]
      estPropio0 = estadoInicial mundoPropio
      estPropioFinal = foldl (\est cmd -> snd (ejecutar cmd est)) estPropio0 cmdsPropios
  afirmar "ejecutar: guión propio guiones/mundo.txt alcanza la meta (victoria con recetas)"
    (metaCumplida estPropioFinal)

  putStrLn "\n=== Todas las pruebas pasaron exitosamente ==="
