# Declaración de Uso de Asistentes de Inteligencia Artificial (IA.md)
**Proyecto 1 — Motor de aventuras de texto en Haskell**  
**CI-3661 — Laboratorio de Lenguajes de Programación I**  
**Estudiante:** José Alejandro (Carnet: 18-10979)  

---

### 1. Herramientas utilizadas
* **Herramienta:** Gemini.
* **Propósito:** Asistente de consulta técnica puntual para resolver dudas sobre el sistema de tipos de Haskell, depurar advertencias del compilador (`-Wall`), discutir alternativas para algoritmos puros sobre grafos y asistir en la redacción de casos de prueba repetitivos.

---

### 2. Partes en las que se consultó a la herramienta
* **Depuración de tipos y advertencias del compilador:**  
  Consultas puntuales para resolver advertencias emitidas por GHC 9.10 con `-Wall`, específicamente sobre el uso de funciones parciales (`-Wx-partial`) y sombreado de identificadores de registros en `Motor.Nucleo`.
* **Discusión de algoritmos funcionales:**  
  Consulta de ideas para implementar de forma puramente funcional la comprobación de alcanzabilidad de salas (recorrido BFS con `Data.Set`) en la regla V9 y el cálculo de punto fijo para la clausura de recetas en la regla V10 de la variante.
* **Apoyo en la suite de pruebas (`test/Spec.hs`):**  
  Generación de plantillas para casos de prueba repetitivos sobre variantes de comandos de entrada (`leerComando`) y verificación de conteo de errores.
* **Formato y estructura de documentación:**  
  Apoyo en la maquetación y estructura formal del estándar ADR para redactar el documento `DECISIONES.md` en base a las decisiones de diseño adoptadas.

---

### 3. Ejemplos concretos de sugerencias corregidas o rechazadas

#### Ejemplo 1: Encadenamiento con cortocircuito en `Either` (Rechazado y corregido)
* **Sugerencia inicial:**  
  Durante la fase de validación de `leerMundo`, se propuso encadenar las verificaciones semánticas en un bloque monádico estándar con `do` sobre el tipo `Either ErrorMundo Mundo`.
* **Razón del rechazo/corrección:**  
  La semántica monádica de `Either` aborta de inmediato (*fail-fast*) en cuanto la primera comprobación devuelve `Left`. Esto contravenía la especificación del enunciado de la sección 4, la cual exige recopilar y mostrar **todos** los errores semánticos juntos (por ejemplo, los 9 errores de `invalido_semantica.txt`).  
* **Corrección aplicada:**  
  Se rechazó el encadenamiento monádico para las reglas V2–V10 y se implementó un acumulador monoidal de listas `[ErrorMundo]` que evalúa todas las propiedades de manera independiente y concatena los fallos resultantes.

#### Ejemplo 2: Inyección de recetas en el estado de prueba (Corregido)
* **Sugerencia inicial:**  
  En la suite de pruebas `test/Spec.hs` para la Variante 3, se construyó un mundo auxiliar con recetas `mundoConReceta = mundoEjemplo { mundoRecetas = [recetaPrueba] }`, pero el estado de prueba subsiguiente se definió como `estConIngredientes = est0 { estInventario = [...] }`, omitiendo asignar `estMundo = mundoConReceta`.
* **Razón de la corrección:**  
  Al ejecutar `cabal test`, el comando `combinar` fallaba al no encontrar ninguna receta registrada en el mundo activo, y el compilador advertía que `mundoConReceta` no estaba siendo usado.  
* **Corrección aplicada:**  
  Se corrigió la asignación a `estConIngredientes = est0 { estMundo = mundoConReceta, estInventario = [...] }`, logrando que la prueba del comando de la variante ejecutara y verificara con éxito tanto el consumo de insumos como la conmutatividad.

#### Ejemplo 3: Codificación de caracteres en Windows (Corregido)
* **Sugerencia inicial:**  
  Usar impresiones directas de cadenas acentuadas como `FALLÓ` en la salida estándar de `Spec.hs` y `Main.hs`.
* **Razón de la corrección:**  
  En el entorno Windows con páginas de código de consola por defecto (como CP1252), la emisión de caracteres acentuados lanzaba excepciones de runtime del tipo `invalid argument (cannot encode character '\211')`.
* **Corrección aplicada:**  
  Se introdujo explícitamente `hSetEncoding stdout utf8` y `hSetEncoding stdin utf8` en los puntos de entrada principales, garantizando compatibilidad total con UTF-8 en cualquier consola.
