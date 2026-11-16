-- | Módulo: Motor.Comandos
-- Descripción: Reconocimiento puro de los comandos introducidos por el jugador.
module Motor.Comandos
  ( leerComando
  ) where

import Data.Char (isSpace, toLower)

import Motor.Tipos

-- | Parsea una línea de texto introducida por el usuario y produce un Comando si es válida.
leerComando :: String -> Maybe Comando
leerComando s = case words (map toLower s) of
  [] -> Nothing

  -- Comandos de movimiento explícito con 'ir'
  ["ir", dirStr] -> Ir <$> parseDireccion dirStr

  -- Direcciones directas (permitidas por el formato y usadas en los guiones del curso)
  ["norte"]  -> Just (Ir Norte)
  ["sur"]    -> Just (Ir Sur)
  ["este"]   -> Just (Ir Este)
  ["oeste"]  -> Just (Ir Oeste)
  ["arriba"] -> Just (Ir Arriba)
  ["abajo"]  -> Just (Ir Abajo)

  -- Abreviaturas comunes de direcciones
  ["n"] -> Just (Ir Norte)
  ["s"] -> Just (Ir Sur)
  ["e"] -> Just (Ir Este)
  ["o"] -> Just (Ir Oeste)

  -- Observación del entorno
  ["mirar"] -> Just Mirar
  ["m"]     -> Just Mirar

  -- Gestión de inventario y objetos
  ["tomar", objStr] -> Just (Tomar (IdObjeto objStr))
  ["coger", objStr] -> Just (Tomar (IdObjeto objStr))
  ["soltar", objStr] -> Just (Soltar (IdObjeto objStr))
  ["examinar", objStr] -> Just (Examinar (IdObjeto objStr))
  ["x", objStr] -> Just (Examinar (IdObjeto objStr))

  ["inventario"] -> Just Inventario
  ["inv"]        -> Just Inventario
  ["i"]          -> Just Inventario

  -- Variante 3: Combinar dos objetos del inventario
  ["combinar", objA, "con", objB] ->
    Just (Combinar (IdObjeto objA) (IdObjeto objB))

  -- Persistencia
  ["guardar", _] -> Just (Guardar (obtenerRutaOriginal s "guardar"))
  ["cargar", _]  -> Just (Cargar (obtenerRutaOriginal s "cargar"))

  -- Ayuda y salida
  ["ayuda"] -> Just Ayuda
  ["help"]  -> Just Ayuda
  ["?"]     -> Just Ayuda

  ["salir"] -> Just Salir
  ["exit"]  -> Just Salir
  ["quit"]  -> Just Salir

  _ -> Nothing

-- | Extrae la ruta preservando mayúsculas y minúsculas originales para el nombre de archivo.
obtenerRutaOriginal :: String -> String -> FilePath
obtenerRutaOriginal entrada comando =
  let resto = drop (length comando) (dropWhile isSpace entrada)
  in dropWhile isSpace resto
