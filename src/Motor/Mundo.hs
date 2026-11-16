-- | Módulo: Motor.Mundo
-- Descripción: Reconocimiento y validación sintáctica y semántica de archivos de mundo.
module Motor.Mundo
  ( leerMundo
  ) where

import Control.Applicative (Alternative (..))
import Data.Char (isSpace)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Motor.Reconocedor
import Motor.Tipos

-- | Línea numerada para rastrear la posición original en el archivo fuente.
type LineaNum = (Int, String)

-- | Bloque de líneas agrupadas con sus números de línea originales.
type BloqueNum = [LineaNum]

-- | Función principal pura: reconoce y valida el texto de un mundo.
leerMundo :: String -> Either [ErrorMundo] Mundo
leerMundo contenido =
  let lineas = zip [1 ..] (lines contenido)
      lineasFiltradas = filter (not . debeIgnorarse . snd) lineas
      bloques = partirEnBloques lineasFiltradas
  in case bloques of
       [] -> Left [ErrorSintaxis 1 "El archivo de mundo está vacío."]
       (cabecera : restoBloques) ->
         case validarSintaxis cabecera restoBloques of
           Left errsSintaxis -> Left errsSintaxis
           Right (mundoPrelim, salasPrelim, objsPrelim, recetasPrelim) ->
             case validarSemantica mundoPrelim salasPrelim objsPrelim recetasPrelim of
               [] -> Right (construirMundo mundoPrelim salasPrelim objsPrelim recetasPrelim)
               errsSemantica -> Left errsSemantica

--------------------------------------------------------------------------------
-- Fase 1: Preprocesamiento y segmentación en bloques
--------------------------------------------------------------------------------

-- | Determina si una línea es comentario (#) o vacía tras quitar espacios.
debeIgnorarse :: String -> Bool
debeIgnorarse s = case dropWhile isSpace s of
  []      -> True
  ('#':_) -> True
  _       -> False

-- | Comprueba si una línea es un separador de bloques (solo guiones, al menos dos).
esSeparadorBloque :: String -> Bool
esSeparadorBloque s =
  let limpia = trim s
  in length limpia >= 2 && all (== '-') limpia

-- | Elimina espacios al inicio y al final de una cadena.
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

-- | Divide la secuencia de líneas en bloques según los separadores '--'.
partirEnBloques :: [LineaNum] -> [BloqueNum]
partirEnBloques [] = []
partirEnBloques lns =
  let (bloque, resto) = break (esSeparadorBloque . snd) lns
      restoSinSeparadores = dropWhile (esSeparadorBloque . snd) resto
  in if null bloque
       then partirEnBloques restoSinSeparadores
       else bloque : partirEnBloques restoSinSeparadores

--------------------------------------------------------------------------------
-- Fase 2: Validación sintáctica (V1)
--------------------------------------------------------------------------------

-- | Datos preliminares extraídos de la cabecera.
data CabeceraPrelim = CabeceraPrelim
  { cabTitulo  :: String
  , cabInicio  :: IdSala
  , cabMeta    :: Meta
  , cabRecetas :: [Receta]
  }

-- | Datos preliminares de una sala.
data SalaPrelim = SalaPrelim
  { spId      :: IdSala
  , spDesc    :: String
  , spSalidas :: [Salida]
  , spObjetos :: [IdObjeto]
  }

-- | Datos preliminares de un objeto.
data ObjetoPrelim = ObjetoPrelim
  { opId   :: IdObjeto
  , opDesc :: String
  }

-- | Parser de una línea con formato CLAVE: valor.
parseLineaClaveValor :: Parser (String, String)
parseLineaClaveValor = do
  clave <- alMenosUno (satisface (\c -> c >= 'A' && c <= 'Z'))
  _ <- char ':'
  _ <- espacios
  resto <- muchos caracter
  pure (clave, trim resto)

-- | Extrae la clave y el valor de una línea, validando el formato sintáctico.
extraerClaveValor :: LineaNum -> Either ErrorMundo (String, String)
extraerClaveValor (num, linea) =
  let limpia = trim linea
  in case parseCompleto parseLineaClaveValor limpia of
       Just (k, v) -> Right (k, v)
       Nothing ->
         Left (ErrorSintaxis num ("Línea sin la forma 'CLAVE: valor' o clave no en mayúsculas: '" ++ limpia ++ "'."))

-- | Parser para la meta 'llegar a <sala>'.
parseMetaLlegar :: Parser Meta
parseMetaLlegar = do
  _ <- simbolo "llegar"
  _ <- simbolo "a"
  s <- identificador
  pure (LlegarA (IdSala s))

-- | Parser para la meta 'llevar <objeto> a <sala>'.
parseMetaLlevar :: Parser Meta
parseMetaLlevar = do
  _ <- simbolo "llevar"
  o <- token identificador
  _ <- simbolo "a"
  s <- identificador
  pure (LlevarA (IdObjeto o) (IdSala s))

-- | Reconocedor de META.
parseMetaVal :: Parser Meta
parseMetaVal = parseMetaLlegar +++ parseMetaLlevar

-- | Parser para salida con requisito: '<dir> -> <sala> requiere <objeto>'.
parseSalidaReq :: Parser Salida
parseSalidaReq = do
  dirNom <- token identificador
  _ <- simbolo "->"
  destNom <- token identificador
  _ <- simbolo "requiere"
  objNom <- identificador
  case parseDireccion dirNom of
    Just d  -> pure (Salida d (IdSala destNom) (Just (IdObjeto objNom)))
    Nothing -> empty

-- | Parser para salida simple: '<dir> -> <sala>'.
parseSalidaSimple :: Parser Salida
parseSalidaSimple = do
  dirNom <- token identificador
  _ <- simbolo "->"
  destNom <- identificador
  case parseDireccion dirNom of
    Just d  -> pure (Salida d (IdSala destNom) Nothing)
    Nothing -> empty

-- | Reconocedor de SALIDA.
parseSalidaVal :: Parser Salida
parseSalidaVal = parseSalidaReq +++ parseSalidaSimple

-- | Parser para RECETA: '<a> + <b> -> <c>' (Variante 3).
parseRecetaVal :: Parser Receta
parseRecetaVal = do
  a <- token identificador
  _ <- simbolo "+"
  b <- token identificador
  _ <- simbolo "->"
  c <- identificador
  pure (Receta (IdObjeto a) (IdObjeto b) (IdObjeto c))

-- | Valida la sintaxis de todos los bloques reportando errores con número de línea.
validarSintaxis :: BloqueNum -> [BloqueNum]
                -> Either [ErrorMundo] (CabeceraPrelim, [SalaPrelim], [ObjetoPrelim], [Receta])
validarSintaxis cabecera restoBloques =
  let errsCabecera = validarCabecera cabecera
      (errsBloques, salas, objs, recsExtra) = foldl' procesarBloque ([], [], [], []) restoBloques
  in case errsCabecera of
       Left errs -> Left (errs ++ errsBloques)
       Right cab ->
         if not (null errsBloques)
           then Left errsBloques
           else Right (cab { cabRecetas = cabRecetas cab ++ recsExtra }, salas, objs, cabRecetas cab ++ recsExtra)
  where
    procesarBloque (errsAcc, salasAcc, objsAcc, recsAcc) bloque =
      case parsearBloque bloque of
        Left errs -> (errsAcc ++ errs, salasAcc, objsAcc, recsAcc)
        Right (Left sala) -> (errsAcc, sala : salasAcc, objsAcc, recsAcc)
        Right (Right obj) -> (errsAcc, salasAcc, obj : objsAcc, recsAcc)

-- | Valida la cabecera del archivo de mundo.
validarCabecera :: BloqueNum -> Either [ErrorMundo] CabeceraPrelim
validarCabecera [] = Left [ErrorSintaxis 1 "Cabecera vacía."]
validarCabecera lns@((numCab, _) : _) =
  let paresResultados = map extraerClaveValor lns
      (errsFormato, pares) = particionarResultados paresResultados
  in if not (null errsFormato)
       then Left errsFormato
       else
         let paresConNum = zip (map fst lns) pares
             (errsClaves, tituloM, inicioM, metaM, recetas) = foldl' revisarClave ([], Nothing, Nothing, Nothing, []) paresConNum
             errsObligatorios =
               (case tituloM of Nothing -> [ErrorSintaxis numCab "Falta campo obligatorio MUNDO en cabecera."]; _ -> [])
               ++ (case inicioM of Nothing -> [ErrorSintaxis numCab "Falta campo obligatorio INICIO en cabecera."]; _ -> [])
               ++ (case metaM of Nothing -> [ErrorSintaxis numCab "Falta campo obligatorio META en cabecera."]; _ -> [])
             todosErrs = errsClaves ++ errsObligatorios
         in if not (null todosErrs)
              then Left todosErrs
              else case (tituloM, inicioM, metaM) of
                     (Just tit, Just ini, Just met) ->
                       Right (CabeceraPrelim tit ini met (reverse recetas))
                     _ -> Left [ErrorSintaxis numCab "Error inesperado en cabecera."]
  where
    revisarClave (errs, t, i, m, recs) (num, (k, v)) =
      case k of
        "MUNDO" ->
          case t of
            Just _  -> (errs ++ [ErrorSintaxis num "Campo MUNDO repetido en cabecera."], t, i, m, recs)
            Nothing -> (errs, Just v, i, m, recs)
        "INICIO" ->
          case i of
            Just _  -> (errs ++ [ErrorSintaxis num "Campo INICIO repetido en cabecera."], t, i, m, recs)
            Nothing ->
              case parseCompleto identificador v of
                Just s  -> (errs, t, Just (IdSala s), m, recs)
                Nothing -> (errs ++ [ErrorSintaxis num ("Identificador inválido en INICIO: '" ++ v ++ "'.")], t, i, m, recs)
        "META" ->
          case m of
            Just _  -> (errs ++ [ErrorSintaxis num "Campo META repetido en cabecera."], t, i, m, recs)
            Nothing ->
              case parseCompleto parseMetaVal v of
                Just met -> (errs, t, i, Just met, recs)
                Nothing  -> (errs ++ [ErrorSintaxis num ("META mal formada: '" ++ v ++ "'.")], t, i, m, recs)
        "RECETA" ->
          case parseCompleto parseRecetaVal v of
            Just rec -> (errs, t, i, m, rec : recs)
            Nothing  -> (errs ++ [ErrorSintaxis num ("RECETA mal formada: '" ++ v ++ "'.")], t, i, m, recs)
        otraClave ->
          (errs ++ [ErrorSintaxis num ("Clave desconocida en cabecera: '" ++ otraClave ++ "'.")], t, i, m, recs)

-- | Parsea un bloque secundario como SALA u OBJETO.
parsearBloque :: BloqueNum -> Either [ErrorMundo] (Either SalaPrelim ObjetoPrelim)
parsearBloque [] = Left [ErrorSintaxis 1 "Bloque vacío."]
parsearBloque (primeraLinea : restoLineas) =
  case extraerClaveValor primeraLinea of
    Left err -> Left [err]
    Right (k, v) ->
      case k of
        "SALA" -> parsearBloqueSala (fst primeraLinea) v restoLineas
        "OBJETO" -> parsearBloqueObjeto (fst primeraLinea) v restoLineas
        otraClave ->
          Left [ErrorSintaxis (fst primeraLinea) ("Primera línea de bloque debe ser SALA u OBJETO, no: '" ++ otraClave ++ "'.")]

-- | Parsea el cuerpo de un bloque de sala.
parsearBloqueSala :: Int -> String -> [LineaNum] -> Either [ErrorMundo] (Either SalaPrelim ObjetoPrelim)
parsearBloqueSala numLinea idStr restoLineas =
  let parseId = parseCompleto identificador idStr
      errId = case parseId of
        Just _  -> []
        Nothing -> [ErrorSintaxis numLinea ("Identificador de sala inválido: '" ++ idStr ++ "'.")]
      paresResultados = map extraerClaveValor restoLineas
      (errsFormato, pares) = particionarResultados paresResultados
  in if not (null errId) || not (null errsFormato)
       then Left (errId ++ errsFormato)
       else
         let paresConNum = zip (map fst restoLineas) pares
             (errsCuerpo, descM, salidas, objs) = foldl' procesarClaveSala ([], Nothing, [], []) paresConNum
             errsObligatorios = case descM of
               Nothing -> [ErrorSintaxis numLinea "Campo obligatorio DESC ausente en bloque SALA."]
               Just _  -> []
             todosErrs = errsCuerpo ++ errsObligatorios
         in if not (null todosErrs)
              then Left todosErrs
              else case (parseId, descM) of
                     (Just sId, Just d) ->
                       Right (Left (SalaPrelim (IdSala sId) d (reverse salidas) (reverse objs)))
                     _ -> Left [ErrorSintaxis numLinea "Error inesperado en sala."]
  where
    procesarClaveSala (errs, dM, sals, objs) (num, (k, v)) =
      case k of
        "DESC" ->
          case dM of
            Just _  -> (errs ++ [ErrorSintaxis num "Campo DESC repetido en sala."], dM, sals, objs)
            Nothing -> (errs, Just v, sals, objs)
        "SALIDA" ->
          case parseCompleto parseSalidaVal v of
            Just sal -> (errs, dM, sal : sals, objs)
            Nothing  -> (errs ++ [ErrorSintaxis num ("SALIDA mal formada o dirección inexistente: '" ++ v ++ "'.")], dM, sals, objs)
        "OBJETO" ->
          case parseCompleto identificador v of
            Just o  -> (errs, dM, sals, IdObjeto o : objs)
            Nothing -> (errs ++ [ErrorSintaxis num ("Identificador de objeto inválido en sala: '" ++ v ++ "'.")], dM, sals, objs)
        otraClave ->
          (errs ++ [ErrorSintaxis num ("Clave desconocida en bloque SALA: '" ++ otraClave ++ "'.")], dM, sals, objs)

-- | Parsea el cuerpo de un bloque de objeto.
parsearBloqueObjeto :: Int -> String -> [LineaNum] -> Either [ErrorMundo] (Either SalaPrelim ObjetoPrelim)
parsearBloqueObjeto numLinea idStr restoLineas =
  let parseId = parseCompleto identificador idStr
      errId = case parseId of
        Just _  -> []
        Nothing -> [ErrorSintaxis numLinea ("Identificador de objeto inválido: '" ++ idStr ++ "'.")]
      paresResultados = map extraerClaveValor restoLineas
      (errsFormato, pares) = particionarResultados paresResultados
  in if not (null errId) || not (null errsFormato)
       then Left (errId ++ errsFormato)
       else
         let paresConNum = zip (map fst restoLineas) pares
             (errsCuerpo, descM) = foldl' procesarClaveObjeto ([], Nothing) paresConNum
             errsObligatorios = case descM of
               Nothing -> [ErrorSintaxis numLinea "Campo obligatorio DESC ausente en bloque OBJETO."]
               Just _  -> []
             todosErrs = errsCuerpo ++ errsObligatorios
         in if not (null todosErrs)
              then Left todosErrs
              else case (parseId, descM) of
                     (Just oId, Just d) ->
                       Right (Right (ObjetoPrelim (IdObjeto oId) d))
                     _ -> Left [ErrorSintaxis numLinea "Error inesperado en objeto."]
  where
    procesarClaveObjeto (errs, dM) (num, (k, v)) =
      case k of
        "DESC" ->
          case dM of
            Just _  -> (errs ++ [ErrorSintaxis num "Campo DESC repetido en objeto."], dM)
            Nothing -> (errs, Just v)
        otraClave ->
          (errs ++ [ErrorSintaxis num ("Clave desconocida en bloque OBJETO: '" ++ otraClave ++ "'.")], dM)

-- | Separa resultados de listas de Either.
particionarResultados :: [Either a b] -> ([a], [b])
particionarResultados [] = ([], [])
particionarResultados (Left e : xs) =
  let (es, rs) = particionarResultados xs in (e : es, rs)
particionarResultados (Right r : xs) =
  let (es, rs) = particionarResultados xs in (es, r : rs)

--------------------------------------------------------------------------------
-- Fase 3: Validación semántica (V2 a V10)
--------------------------------------------------------------------------------

-- | Comprueba todas las reglas semánticas y acumula los errores.
validarSemantica :: CabeceraPrelim -> [SalaPrelim] -> [ObjetoPrelim] -> [Receta] -> [ErrorMundo]
validarSemantica cab salas objs recetas =
  let setSalas = Set.fromList (map spId salas)
      setObjs  = Set.fromList (map opId objs)
      errsV2 = validarV2 salas objs
      errsV3 = validarV3 salas setSalas
      errsV4 = validarV4 salas setObjs
      errsV5 = validarV5 salas
      errsV6 = validarV6 salas setObjs
      errsV7 = validarV7 salas
      errsV8 = validarV8 cab setSalas setObjs
      errsV9 = validarV9 cab salas setSalas
      errsV10 = validarV10 cab salas setObjs recetas
      errsRec = validarRecetas recetas setObjs
  in errsV2 ++ errsV3 ++ errsV4 ++ errsV5 ++ errsV6 ++ errsV7 ++ errsV8 ++ errsV9 ++ errsV10 ++ errsRec

-- | V2: Dos salas o dos objetos con el mismo identificador.
validarV2 :: [SalaPrelim] -> [ObjetoPrelim] -> [ErrorMundo]
validarV2 salas objs =
  let dupSalas = encontrarDuplicados (map (unIdSala . spId) salas)
      dupObjs  = encontrarDuplicados (map (unIdObjeto . opId) objs)
  in [ ErrorIdDuplicado "sala" s | s <- dupSalas ]
     ++ [ ErrorIdDuplicado "objeto" o | o <- dupObjs ]

-- | V3: Una salida hacia una sala que no existe.
validarV3 :: [SalaPrelim] -> Set.Set IdSala -> [ErrorMundo]
validarV3 salas setSalas =
  [ ErrorSalidaInexistente (spId sala) (salidaDir sal) (salidaDestino sal)
  | sala <- salas
  , sal <- spSalidas sala
  , not (Set.member (salidaDestino sal) setSalas)
  ]

-- | V4: Una salida que requiere un objeto que no existe.
validarV4 :: [SalaPrelim] -> Set.Set IdObjeto -> [ErrorMundo]
validarV4 salas setObjs =
  [ ErrorSalidaReqInexistente (spId sala) (salidaDir sal) reqObj
  | sala <- salas
  , sal <- spSalidas sala
  , Just reqObj <- [salidaReq sal]
  , not (Set.member reqObj setObjs)
  ]

-- | V5: Una sala con dos salidas en la misma dirección.
validarV5 :: [SalaPrelim] -> [ErrorMundo]
validarV5 salas =
  [ ErrorSalidasDuplicadas (spId sala) dir
  | sala <- salas
  , dir <- encontrarDuplicados (map salidaDir (spSalidas sala))
  ]

-- | V6: Una sala que contiene un objeto no definido.
validarV6 :: [SalaPrelim] -> Set.Set IdObjeto -> [ErrorMundo]
validarV6 salas setObjs =
  [ ErrorObjetoNoDefinido (spId sala) obj
  | sala <- salas
  , obj <- spObjetos sala
  , not (Set.member obj setObjs)
  ]

-- | V7: Un objeto colocado en más de una sala (o dos veces en la misma).
validarV7 :: [SalaPrelim] -> [ErrorMundo]
validarV7 salas =
  let pares = [ (obj, spId sala) | sala <- salas, obj <- spObjetos sala ]
      mapa = foldl' (\m (o, s) -> Map.insertWith (++) o [s] m) Map.empty pares
  in [ ErrorObjetoMultiplesSalas o sls
     | (o, sls) <- Map.toList mapa
     , length sls > 1
     ]

-- | V8: INICIO o META con una sala o un objeto inexistentes.
validarV8 :: CabeceraPrelim -> Set.Set IdSala -> Set.Set IdObjeto -> [ErrorMundo]
validarV8 cab setSalas setObjs =
  let errIni = [ ErrorInicioInexistente (cabInicio cab) | not (Set.member (cabInicio cab) setSalas) ]
      errMeta = case cabMeta cab of
        LlegarA s ->
          [ ErrorMetaInexistente ("sala '" ++ unIdSala s ++ "'") | not (Set.member s setSalas) ]
        LlevarA o s ->
          [ ErrorMetaInexistente ("objeto '" ++ unIdObjeto o ++ "'") | not (Set.member o setObjs) ]
          ++ [ ErrorMetaInexistente ("sala '" ++ unIdSala s ++ "'") | not (Set.member s setSalas) ]
  in errIni ++ errMeta

-- | V9: Una sala que no es alcanzable desde INICIO siguiendo salidas (ignorando requisitos).
validarV9 :: CabeceraPrelim -> [SalaPrelim] -> Set.Set IdSala -> [ErrorMundo]
validarV9 cab salas setSalas =
  let inicio = cabInicio cab
  in if not (Set.member inicio setSalas)
       then [] -- Ya reportado por V8
       else
         let mapaSalas = Map.fromList [ (spId s, spSalidas s) | s <- salas ]
             alcanzables = calcularAlcanzables (Set.singleton inicio) [inicio] mapaSalas setSalas
         in [ ErrorSalaInalcanzable (spId s)
            | s <- salas
            , not (Set.member (spId s) alcanzables)
            ]
  where
    calcularAlcanzables visitados [] _ _ = visitados
    calcularAlcanzables visitados (curr : cola) mapaSalas existentes =
      let salidas = Map.findWithDefault [] curr mapaSalas
          vecinos = [ dest
                    | sal <- salidas
                    , let dest = salidaDestino sal
                    , Set.member dest existentes
                    , not (Set.member dest visitados)
                    ]
          visitados' = foldl' (flip Set.insert) visitados vecinos
      in calcularAlcanzables visitados' (cola ++ vecinos) mapaSalas existentes

-- | V10: Un objeto que se necesita (meta o salidas) pero no está colocado ni es obtenible por recetas.
validarV10 :: CabeceraPrelim -> [SalaPrelim] -> Set.Set IdObjeto -> [Receta] -> [ErrorMundo]
validarV10 cab salas setObjs recetas =
  let objMeta = case cabMeta cab of
        LlevarA o _ -> [o | Set.member o setObjs]
        _           -> []
      objsSalidas = [ reqObj
                    | s <- salas
                    , sal <- spSalidas s
                    , Just reqObj <- [salidaReq sal]
                    , Set.member reqObj setObjs
                    ]
      necesarios = Set.fromList (objMeta ++ objsSalidas)
      iniciales = Set.fromList [ o | s <- salas, o <- spObjetos s, Set.member o setObjs ]
      conseguibles = clausuraRecetas iniciales recetas
  in [ ErrorObjetoInalcanzable o
     | o <- Set.toList necesarios
     , not (Set.member o conseguibles)
     ]
  where
    clausuraRecetas disponibles recs =
      let nuevos = [ recRes r
                   | r <- recs
                   , Set.member (recIng1 r) disponibles
                   , Set.member (recIng2 r) disponibles
                   , not (Set.member (recRes r) disponibles)
                   ]
      in if null nuevos
           then disponibles
           else clausuraRecetas (foldl' (flip Set.insert) disponibles nuevos) recs

-- | Validación propia de la Variante 3: los objetos de cada receta deben existir.
validarRecetas :: [Receta] -> Set.Set IdObjeto -> [ErrorMundo]
validarRecetas recetas setObjs =
  [ ErrorRecetaObjetoInexistente r o
  | r <- recetas
  , o <- [recIng1 r, recIng2 r, recRes r]
  , not (Set.member o setObjs)
  ]

-- | Encuentra elementos duplicados en una lista.
encontrarDuplicados :: Ord a => [a] -> [a]
encontrarDuplicados xs =
  let (_, dups) = foldl' check (Set.empty, Set.empty) xs
  in Set.toList dups
  where
    check (vistos, dups) x
      | Set.member x vistos = (vistos, Set.insert x dups)
      | otherwise           = (Set.insert x vistos, dups)

--------------------------------------------------------------------------------
-- Fase 4: Construcción del Mundo validado
--------------------------------------------------------------------------------

-- | Ensambla el tipo Mundo a partir de los datos preliminares una vez validados.
construirMundo :: CabeceraPrelim -> [SalaPrelim] -> [ObjetoPrelim] -> [Receta] -> Mundo
construirMundo cab salas objs recetas =
  let mapaSalas = Map.fromList [ (spId s, toSala s) | s <- salas ]
      mapaObjs  = Map.fromList [ (opId o, toObjeto o) | o <- objs ]
  in Mundo
       { mundoTitulo  = cabTitulo cab
       , mundoInicio  = cabInicio cab
       , mundoMeta    = cabMeta cab
       , mundoSalas   = mapaSalas
       , mundoObjetos = mapaObjs
       , mundoRecetas = recetas
       }
  where
    toSala sp = Sala
      { salaId      = spId sp
      , salaDesc    = spDesc sp
      , salaSalidas = spSalidas sp
      , salaObjetos = spObjetos sp
      }
    toObjeto op = Objeto
      { objId   = opId op
      , objDesc = opDesc op
      }
