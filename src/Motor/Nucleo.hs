-- | Módulo: Motor.Nucleo
-- Descripción: Lógica pura de transición de estados y ejecución de comandos del juego.
module Motor.Nucleo
  ( estadoInicial
  , ejecutar
  , metaCumplida
  , describirSala
  ) where

import qualified Data.Map.Strict as Map

import Motor.Tipos

-- | Genera el estado inicial del juego a partir de un mundo validado.
estadoInicial :: Mundo -> Estado
estadoInicial mundo = Estado
  { estMundo        = mundo
  , estUbicacion    = mundoInicio mundo
  , estInventario   = []
  , estObjetosSalas = Map.fromList [ (salaId s, salaObjetos s) | s <- Map.elems (mundoSalas mundo) ]
  , estTerminado    = False
  }

-- | Determina si se ha alcanzado la meta del mundo en el estado actual.
metaCumplida :: Estado -> Bool
metaCumplida est = case mundoMeta (estMundo est) of
  LlegarA salaMeta ->
    estUbicacion est == salaMeta
  LlevarA objMeta salaMeta ->
    estUbicacion est == salaMeta && elem objMeta (estInventario est)

-- | Genera las líneas descriptivas de la sala actual: texto, salidas y objetos.
describirSala :: Estado -> [String]
describirSala est =
  let mundo = estMundo est
      ubicacion = estUbicacion est
  in case Map.lookup ubicacion (mundoSalas mundo) of
       Nothing -> ["Te encuentras en un lugar desconocido."]
       Just sala ->
         let lineaDesc = salaDesc sala
             sals = salaSalidas sala
             lineaSalidas = if null sals
                              then "Salidas: ninguna."
                              else "Salidas: " ++ unwords (map (mostrarDireccion . salidaDir) sals) ++ "."
             objsEnSala = Map.findWithDefault [] ubicacion (estObjetosSalas est)
             lineaObjs = if null objsEnSala
                           then []
                           else ["Hay aquí: " ++ unwords (map unIdObjeto objsEnSala) ++ "."]
         in [lineaDesc, lineaSalidas] ++ lineaObjs

-- | Ejecuta un comando de forma puramente funcional, devolviendo los mensajes al usuario
-- y el nuevo estado del juego.
ejecutar :: Comando -> Estado -> ([String], Estado)
ejecutar cmd est = case cmd of
  Mirar ->
    (describirSala est, est)

  Ir dir ->
    let mundo = estMundo est
        ubicacion = estUbicacion est
    in case Map.lookup ubicacion (mundoSalas mundo) of
         Nothing -> (["Error interno: la sala actual no existe en el mundo."], est)
         Just sala ->
           case filter (\s -> salidaDir s == dir) (salaSalidas sala) of
             [] ->
               (["No hay salida hacia el " ++ mostrarDireccion dir ++ "."], est)
             (sal : _) ->
               case salidaReq sal of
                 Just objReq | not (elem objReq (estInventario est)) ->
                   (["No puedes ir hacia el " ++ mostrarDireccion dir ++ ": la salida está bloqueada. Requiere " ++ unIdObjeto objReq ++ "."], est)
                 _ ->
                   let nuevoEst = est { estUbicacion = salidaDestino sal }
                   in (describirSala nuevoEst, nuevoEst)

  Tomar targetObj ->
    let ubicacion = estUbicacion est
        objsAqui = Map.findWithDefault [] ubicacion (estObjetosSalas est)
    in case removerElemento targetObj objsAqui of
         Nothing ->
           (["No ves eso aquí."], est)
         Just nuevosObjsAqui ->
           let nuevoMapa = Map.insert ubicacion nuevosObjsAqui (estObjetosSalas est)
               nuevoEst = est
                 { estInventario   = targetObj : estInventario est
                 , estObjetosSalas = nuevoMapa
                 }
           in (["Has tomado " ++ unIdObjeto targetObj ++ "."], nuevoEst)

  Soltar targetObj ->
    case removerElemento targetObj (estInventario est) of
      Nothing ->
        (["No llevas eso contigo."], est)
      Just nuevoInv ->
        let ubicacion = estUbicacion est
            objsAqui = Map.findWithDefault [] ubicacion (estObjetosSalas est)
            nuevoMapa = Map.insert ubicacion (targetObj : objsAqui) (estObjetosSalas est)
            nuevoEst = est
              { estInventario   = nuevoInv
              , estObjetosSalas = nuevoMapa
              }
        in (["Has soltado " ++ unIdObjeto targetObj ++ "."], nuevoEst)

  Examinar targetObj ->
    let enInv = elem targetObj (estInventario est)
        enSala = elem targetObj (Map.findWithDefault [] (estUbicacion est) (estObjetosSalas est))
    in if not enInv && not enSala
         then (["No ves eso aquí ni lo llevas contigo."], est)
         else case Map.lookup targetObj (mundoObjetos (estMundo est)) of
                Just obj -> ([objDesc obj], est)
                Nothing  -> (["No hay nada especial sobre eso."], est)

  Inventario ->
    let inv = estInventario est
    in if null inv
         then (["No llevas nada en el inventario."], est)
         else (["Llevas contigo: " ++ unwords (map unIdObjeto inv)], est)

  -- Variante 3: Combinar dos objetos del inventario (conmutativa)
  Combinar objA objB ->
    let inv = estInventario est
    in case removerElemento objA inv of
         Nothing ->
           (["No tienes " ++ unIdObjeto objA ++ " en tu inventario."], est)
         Just invSinA ->
           case removerElemento objB invSinA of
             Nothing ->
               (["No tienes " ++ unIdObjeto objB ++ " en tu inventario."], est)
             Just invSinAB ->
               let recetas = mundoRecetas (estMundo est)
                   calza r = (recIng1 r == objA && recIng2 r == objB)
                          || (recIng1 r == objB && recIng2 r == objA)
               in case filter calza recetas of
                    [] ->
                      (["Esos objetos no se pueden combinar."], est)
                    (r : _) ->
                      let producto = recRes r
                          nuevoEst = est { estInventario = producto : invSinAB }
                      in (["Has combinado " ++ unIdObjeto objA ++ " con " ++ unIdObjeto objB ++ " y obtuviste " ++ unIdObjeto producto ++ "."], nuevoEst)

  Ayuda ->
    ( [ "Comandos disponibles:"
      , "  ir <dirección> | <dirección>  - Moverse (norte, sur, este, oeste, arriba, abajo)"
      , "  mirar                        - Describe la sala actual"
      , "  tomar <objeto> | coger <obj> - Recoger un objeto de la sala"
      , "  soltar <objeto>              - Dejar un objeto en la sala"
      , "  examinar <objeto>            - Ver detalles de un objeto"
      , "  inventario | inv             - Ver lo que llevas"
      , "  combinar <a> con <b>         - Combina dos objetos de tu inventario"
      , "  guardar <archivo>            - Guarda la partida en un archivo"
      , "  cargar <archivo>             - Carga una partida guardada"
      , "  ayuda                        - Muestra esta lista"
      , "  salir                        - Termina el juego"
      ]
    , est
    )

  Salir ->
    (["Fin del juego."], est { estTerminado = True })

  -- Guardar y Cargar son gestionados por el bucle de E/S en Main y Persistencia
  Guardar _ -> ([], est)
  Cargar _  -> ([], est)

-- | Remueve la primera aparición de un elemento en una lista.
removerElemento :: Eq a => a -> [a] -> Maybe [a]
removerElemento _ [] = Nothing
removerElemento y (x : xs)
  | y == x    = Just xs
  | otherwise = (x :) <$> removerElemento y xs
