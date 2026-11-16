# Registro de Decisiones de Arquitectura (DECISIONES.md)
**Proyecto 1 Motor de aventuras de texto en Haskell**  
**CI-3661 Laboratorio de Lenguajes de Programación I**  
**Estudiante:** José Alejandro (Carnet: 18-10979)  
**Variante asignada:** $1810979 \pmod 4 = 3$  

---

## D1. Representación del mundo y los identificadores (⋆ Obligatoria)
* **Contexto:**  
  Un mundo de aventura posee múltiples entidades: salas, objetos y recetas. Representar los identificadores simplemente como `String` es propenso a errores sutiles en los que se suministra accidentalmente el identificador de una sala en una función que espera un objeto, o viceversa. Además, el acceso a las salas durante el juego debe ser eficiente y seguro.
* **Opciones consideradas:**  
  1. *Identificadores con sinónimos de tipo y listas de asociación:*  
     Usar `type IdSala = String`, `type IdObjeto = String` y almacenar el mundo como listas de tuplas `[(IdSala, Sala)]`.  
  2. *Tipos nominales dedicados (`newtype`) y mapas finitos (`Map`):*  
     Definir `newtype IdSala = IdSala String` y `newtype IdObjeto = IdObjeto String` junto con `Data.Map.Strict.Map IdSala Sala` y `Map IdObjeto Objeto`.
* **Decisión:**  
  Se seleccionó la **Opción 2**.
* **Consecuencias:**  
  * *Ganancias:* Seguridad estricta garantizada por el sistema de tipos de Haskell en tiempo de compilación. Las búsquedas de salas y objetos en el grafo operan en tiempo $O(\log n)$ en lugar de $O(n)$.
  * *Pérdidas:* Requiere empaquetar y desempaquetar (`unIdSala`, `unIdObjeto`) al mostrar mensajes y realizar reconocimientos sintácticos.
  * *Invariantes:* El tipo garantiza la separación estricta de espacios de nombres entre salas y objetos. No garantiza por construcción la unicidad de identificadores en el archivo de texto; dicha invariante se valida semánticamente mediante la comprobación V2 antes de instanciar el tipo `Mundo`.

---

## D2. Estado dinámico y ubicación de objetos (⋆ Obligatoria)
* **Contexto:**  
  Durante la ejecución de la aventura, el jugador toma y suelta objetos, cambia de sala y puede sintetizar nuevos ítems mediante recetas. Debía decidirse si modificar recursivamente todo el registro estático `Mundo` o aislar las mutaciones funcionales en una estructura separada.
* **Opciones consideradas:**  
  1. *Copia completa del mundo con salas mutadas:*  
     En cada paso de juego, actualizar la lista `salaObjetos` de cada `Sala` dentro del `Map IdSala Sala` contenido en `Mundo`.  
  2. *Separación de modelo estático y estado dinámico:*  
     Mantener el `Mundo` inmutable como lectura de referencia y almacenar en `Estado` únicamente las variables dinámicas: ubicación del jugador (`estUbicacion`), inventario (`estInventario`) y un mapa dinámico de objetos por sala (`estObjetosSalas :: Map IdSala [IdObjeto]`).
* **Decisión:**  
  Se seleccionó la **Opción 2**.
* **Consecuencias:**  
  * *Ganancias:* La lógica de `Motor.Nucleo` es completamente pura y modular. Los objetos soltados van directamente a `estObjetosSalas` bajo la clave de la sala actual sin alterar la definición original de la sala. Esto hace que guardar y restaurar el estado sea liviano y no duplique descripciones estáticas.
  * *Pérdidas:* Se requiere consultar dos fuentes (`mundoSalas` para la descripción/salidas y `estObjetosSalas` para los ítems presentes) para componer la visión global de la sala.

---

## D3. Reporte exhaustivo de errores de validación frente a parada temprana (⋆ Obligatoria)
* **Contexto:**  
  La especificación exige detectar y reportar todos los errores de la sección 4 de forma conjunta, sin detenerse en el primero. Se requería entender por qué la composición monádica estándar con `Either` y `do` resulta insuficiente para esta meta.
* **Opciones consideradas:**  
  1. *Encadenamiento monádico con `do` sobre `Either ErrorMundo a`:*  
     Ejecutar secuencialmente cada comprobación en un bloque monádico.  
  2. *Acumulación monoidal de errores mediante listas independientes:*  
     Separar la validación en dos niveles (sintaxis V1 vs semántica V2–V10). En la etapa semántica, cada regla produce una lista de errores `[ErrorMundo]` que se concatenan con `++`.
* **Decisión:**  
  Se seleccionó la **Opción 2**.
* **Consecuencias:**  
  * *Por qué no basta `do`:* La mónada `Either` implementa semántica de cortocircuito (*fail-fast*); la primera computación que devuelva `Left` interrumpe de inmediato el resto de las comprobaciones.
  * *Ganancias:* La Opción 2 permite evaluar todas las reglas semánticas (V2 a V10) sobre el mismo árbol sintáctico preliminar, recolectando la totalidad de inconsistencias simultáneamente en una única lista `[ErrorMundo]`. Si dicha lista es no vacía, se retorna `Left errores`; en caso contrario, `Right mundo`.

---

## D4. Diseño del reconocedor y gestión de ambigüedad (⋆ Obligatoria)
* **Contexto:**  
  El proyecto prohíbe el uso de librerías externas de parsing (`parsec`, `megaparsec`, `attoparsec`, regex) y pide basar el reconocedor en `ReadS a = String -> [(a, String)]` visto en la clase 5, reportando además el número de línea de cada fallo sintáctico.
* **Opciones consideradas:**  
  1. *Uso directo de `ReadS a` mediante funciones sueltas y listas por comprensión.*  
  2. *Encapsulación en `newtype Parser a = Parser { runParser :: ReadS a }`:*  
     Implementar instancias de `Functor`, `Applicative`, `Monad` y `Alternative`, junto con un operador de elección determinista `(+++)` sesgado a la izquierda.
* **Decisión:**  
  Se seleccionó la **Opción 2**.
* **Consecuencias:**  
  * *Gestión de la ambigüedad:* Para evitar explosión combinatoria por ambigüedad léxica, se utilizó `(+++)` para opciones exclusivas y la función `parseCompleto`, la cual exige que el parseo consuma toda la cadena (o solo deje espacios) y selecciona deterministamente el primer resultado válido.
  * *Número de línea:* Se preprocesa el texto crudo asociando cada línea con su índice original 1-basado (`zip [1..] (lines contenido)`). Al descartar comentarios y líneas vacías, los números de línea originales se conservan intactos en cada tupla `(Int, String)`, permitiendo que cualquier falla sintáctica en `Motor.Mundo` reporte con precisión la línea infractora.

---

## D5. Decisiones de diseño de la Variante 3: Combinar (⋆ Obligatoria)
* **Contexto:**  
  La Variante 3 introduce recetas en la cabecera (`RECETA: a + b -> c`) y el comando `combinar a con b`. El enunciado deja abierto si el orden de los ingredientes importa y cómo interactúa con la regla de validación V10.
* **Opciones consideradas:**  
  1. *Combinación ordenada y validación estática de colocación:*  
     Exigir que los ingredientes se ingresen exactamente en el orden de la receta; en V10 exigir que todo objeto necesario esté físicamente en una sala.  
  2. *Combinación conmutativa y clausura transitiva en V10:*  
     Considerar que `combinar a con b` es equivalente a `combinar b con a`. En V10, calcular el conjunto de objetos conseguibles mediante el cálculo de punto fijo sobre las recetas aplicables a los objetos iniciales.
* **Decisión:**  
  Se seleccionó la **Opción 2**.
* **Consecuencias:**  
  * *Ganancias:* Experiencia de usuario natural (al jugador no le frustra si escribe los ingredientes en orden inverso). La validación V10 es matemáticamente rigurosa: si un objeto es indispensable para abrir una puerta o para la meta, pero no está en ninguna sala, se acepta como válido siempre y cuando exista una cadena de recetas que permita fabricarlo a partir de los objetos disponibles en el mundo.
  * *Validación propia:* Se añadió la regla que exige que los tres identificadores de una receta (`a`, `b`, `c`) correspondan a objetos formalmente definidos en el archivo.

---

## D6. Formato de guardado y resiliencia ante mundos distintos (Escogida)
* **Contexto:**  
  La especificación pide que si un archivo de partida guardada no existe, está corrupto o pertenece a otro mundo, el motor debe informarlo y continuar jugando sin abortar jamás.
* **Opciones consideradas:**  
  1. *Serialización cruda de `Estado`:*  
     Hacer `show estado` y `read estado`.  
  2. *Estructura desacoplada `PartidaGuardada` con validación de cabecera:*  
     Crear un tipo específico que guarda el título del mundo, la sala actual, el inventario y la distribución actual de objetos. Al cargar, se lee de forma segura con `Text.Read.readMaybe` y se verifica la concordancia del título del mundo y la existencia de las salas.
* **Decisión:**  
  Se seleccionó la **Opción 2**.
* **Consecuencias:**  
  * *Ganancias:* Desacoplamiento total de representaciones internas de funciones o estructuras pesadas. Al validar mediante `readMaybe`, si el archivo tiene texto corrupto o formato no válido, simplemente se devuelve `Left mensaje` sin lanzar excepciones de Haskell (`Prelude.read: no parse`). Si el título del mundo grabado no coincide con el mundo en ejecución, se notifica el conflicto y el bucle principal continúa en el estado actual sin alterarse.
