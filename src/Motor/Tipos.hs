-- | Módulo: Motor.Tipos
-- Descripción: Modelado del dominio del juego mediante tipos algebraicos.
module Motor.Tipos
  ( Direccion (..)
  , parseDireccion
  , mostrarDireccion
  , IdSala (..)
  , IdObjeto (..)
  , Objeto (..)
  , Salida (..)
  , Sala (..)
  , Meta (..)
  , Receta (..)
  , Mundo (..)
  , Comando (..)
  , Estado (..)
  , PartidaGuardada (..)
  , ErrorMundo (..)
  , mostrarError
  ) where

import Data.Char (toLower)
import Data.Map.Strict (Map)

-- | Direcciones válidas para el movimiento en el mundo.
data Direccion
  = Norte
  | Sur
  | Este
  | Oeste
  | Arriba
  | Abajo
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Representación textual en minúsculas de una dirección.
mostrarDireccion :: Direccion -> String
mostrarDireccion Norte  = "norte"
mostrarDireccion Sur    = "sur"
mostrarDireccion Este   = "este"
mostrarDireccion Oeste  = "oeste"
mostrarDireccion Arriba = "arriba"
mostrarDireccion Abajo  = "abajo"

-- | Parsea el nombre de una dirección.
parseDireccion :: String -> Maybe Direccion
parseDireccion s = case map toLower s of
  "norte"  -> Just Norte
  "sur"    -> Just Sur
  "este"   -> Just Este
  "oeste"  -> Just Oeste
  "arriba" -> Just Arriba
  "abajo"  -> Just Abajo
  _        -> Nothing

-- | Identificador de sala.
newtype IdSala = IdSala { unIdSala :: String }
  deriving (Eq, Ord, Show, Read)

-- | Identificador de objeto.
newtype IdObjeto = IdObjeto { unIdObjeto :: String }
  deriving (Eq, Ord, Show, Read)

-- | Representación de un objeto en el mundo.
data Objeto = Objeto
  { objId   :: IdObjeto
  , objDesc :: String
  } deriving (Eq, Show, Read)

-- | Salida de una sala hacia otra, opcionalmente con bloqueo por objeto.
data Salida = Salida
  { salidaDir     :: Direccion
  , salidaDestino :: IdSala
  , salidaReq     :: Maybe IdObjeto
  } deriving (Eq, Show, Read)

-- | Sala del mundo de juego.
data Sala = Sala
  { salaId      :: IdSala
  , salaDesc    :: String
  , salaSalidas :: [Salida]
  , salaObjetos :: [IdObjeto]
  } deriving (Eq, Show, Read)

-- | Condición de victoria del mundo.
data Meta
  = LlegarA IdSala
  | LlevarA IdObjeto IdSala
  deriving (Eq, Show, Read)

-- | Receta de combinación de objetos (Variante 3).
data Receta = Receta
  { recIng1 :: IdObjeto
  , recIng2 :: IdObjeto
  , recRes  :: IdObjeto
  } deriving (Eq, Show, Read)

-- | Estructura estática del mundo validado.
data Mundo = Mundo
  { mundoTitulo   :: String
  , mundoInicio   :: IdSala
  , mundoMeta     :: Meta
  , mundoSalas    :: Map IdSala Sala
  , mundoObjetos  :: Map IdObjeto Objeto
  , mundoRecetas  :: [Receta]
  } deriving (Eq, Show, Read)

-- | Comandos ejecutables por el jugador.
data Comando
  = Ir Direccion
  | Mirar
  | Tomar IdObjeto
  | Soltar IdObjeto
  | Examinar IdObjeto
  | Inventario
  | Guardar FilePath
  | Cargar FilePath
  | Ayuda
  | Salir
  | Combinar IdObjeto IdObjeto
  deriving (Eq, Show)

-- | Estado dinámico del juego.
data Estado = Estado
  { estMundo        :: Mundo
  , estUbicacion    :: IdSala
  , estInventario   :: [IdObjeto]
  , estObjetosSalas :: Map IdSala [IdObjeto]
  , estTerminado    :: Bool
  } deriving (Eq, Show, Read)

-- | Formato de serialización para guardar/cargar partidas sin acoplar detalles no requeridos.
data PartidaGuardada = PartidaGuardada
  { pgTituloMundo  :: String
  , pgUbicacion    :: IdSala
  , pgInventario   :: [IdObjeto]
  , pgObjetosSalas :: [(IdSala, [IdObjeto])]
  } deriving (Eq, Show, Read)

-- | Errores de validación sintáctica (V1) y semántica (V2 a V10) de un archivo de mundo.
data ErrorMundo
  = ErrorSintaxis Int String
  | ErrorIdDuplicado String String
  | ErrorSalidaInexistente IdSala Direccion IdSala
  | ErrorSalidaReqInexistente IdSala Direccion IdObjeto
  | ErrorSalidasDuplicadas IdSala Direccion
  | ErrorObjetoNoDefinido IdSala IdObjeto
  | ErrorObjetoMultiplesSalas IdObjeto [IdSala]
  | ErrorInicioInexistente IdSala
  | ErrorMetaInexistente String
  | ErrorSalaInalcanzable IdSala
  | ErrorObjetoInalcanzable IdObjeto
  | ErrorRecetaObjetoInexistente Receta IdObjeto
  deriving (Eq, Show)

-- | Convierte un ErrorMundo a mensaje con prefijo 'Error: [Vx] ' según la sección 4 del enunciado.
mostrarError :: ErrorMundo -> String
mostrarError err = case err of
  ErrorSintaxis linea detalle ->
    "Error: [V1] Línea " ++ show linea ++ ": " ++ detalle
  ErrorIdDuplicado tipo idNom ->
    "Error: [V2] Identificador de " ++ tipo ++ " duplicado: '" ++ idNom ++ "'."
  ErrorSalidaInexistente origen dir destino ->
    "Error: [V3] La sala '" ++ unIdSala origen ++ "' tiene salida al "
    ++ mostrarDireccion dir ++ " hacia la sala inexistente '" ++ unIdSala destino ++ "'."
  ErrorSalidaReqInexistente origen dir obj ->
    "Error: [V4] La salida al " ++ mostrarDireccion dir ++ " en '" ++ unIdSala origen
    ++ "' requiere el objeto inexistente '" ++ unIdObjeto obj ++ "'."
  ErrorSalidasDuplicadas sala dir ->
    "Error: [V5] La sala '" ++ unIdSala sala ++ "' tiene más de una salida hacia el "
    ++ mostrarDireccion dir ++ "."
  ErrorObjetoNoDefinido sala obj ->
    "Error: [V6] La sala '" ++ unIdSala sala ++ "' contiene el objeto no definido '"
    ++ unIdObjeto obj ++ "'."
  ErrorObjetoMultiplesSalas obj salas ->
    "Error: [V7] El objeto '" ++ unIdObjeto obj
    ++ "' está colocado en múltiples salas (o repetido): "
    ++ unwords (map unIdSala salas) ++ "."
  ErrorInicioInexistente sala ->
    "Error: [V8] La sala de INICIO '" ++ unIdSala sala ++ "' no existe."
  ErrorMetaInexistente detalle ->
    "Error: [V8] La META hace referencia a un elemento inexistente: " ++ detalle ++ "."
  ErrorSalaInalcanzable sala ->
    "Error: [V9] La sala '" ++ unIdSala sala ++ "' no es alcanzable desde INICIO."
  ErrorObjetoInalcanzable obj ->
    "Error: [V10] El objeto '" ++ unIdObjeto obj
    ++ "' es necesario para la meta o salidas pero no está en ninguna sala ni puede obtenerse por receta."
  ErrorRecetaObjetoInexistente (Receta a b c) obj ->
    "Error: [Variante] La receta '" ++ unIdObjeto a ++ " + " ++ unIdObjeto b ++ " -> " ++ unIdObjeto c
    ++ "' requiere el objeto inexistente '" ++ unIdObjeto obj ++ "'."
