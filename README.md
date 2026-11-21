# Proyecto 1: Motor de Aventuras de Texto en Haskell

**CI-3661 — Laboratorio de Lenguajes de Programación I**  
**Universidad Simón Bolívar (Sep – Dic 2026)**  
**Profesor:** Leonardo López

* **Estudiante:** José Alejandro
* **Carnet:** 18-10979

---

## 1. Variante Asignada

Cálculo según el carnet del estudiante:
$$\text{Carnet} = 1810979$$
$$1810979 \pmod 4 = 3$$

**Variante 3 — Combinar:**
* Permite definir en la cabecera líneas de la forma `RECETA: a + b -> c`.
* Agrega el comando interactivo `combinar a con b`, el cual consume del inventario los ingredientes `a` y `b` y entrega el producto `c`.
* Implementa combinación conmutativa (el orden de `a` y `b` no altera el resultado).
* La regla de validación V10 considera conseguible cualquier objeto que pueda ser sintetizado mediante una cadena de recetas válidas a partir de los objetos colocados en el mundo.
* Valida adicionalmente que los tres objetos de cada receta existan en el mundo.

---

## 2. Requisitos y Dependencias

* GHC $\ge 9.0$ (probado en GHC 9.10.3)
* Cabal $\ge 3.0$
* Paquetes de Haskell permitidos: `base`, `containers`, `directory`, `filepath`.
* Cero librerías externas de parsing ni de regex; reconocedor puro construido sobre `ReadS`.

---

## 3. Compilación y Ejecución

### Compilar el proyecto
```bash
cabal build
```

### Validar un archivo de mundo
Valida la sintaxis (V1) y la semántica completa (V2–V10) reportando todos los errores juntos o confirmando validez:
```bash
cabal run aventura -- -validar mundos/mundo.txt
```

### Jugar interactivamente
```bash
cabal run aventura -- mundos/mundo.txt
```

### Jugar con entrada redirigida (guion)
Permite correr un guion automático de comandos hasta ganar o alcanzar el fin de archivo (EOF):
```bash
# Mundo y guion:
cabal run aventura -- mundos/mundo.txt < guiones/mundo.txt
```

---

## 4. Ejecución de Pruebas Automatizadas

La suite de pruebas contiene **27 casos de prueba** que verifican:
1. Reconocimiento de comandos válidos, sinónimos (`coger`, `inv`), abreviaturas y comandos inválidos (`Motor.Comandos`).
2. Validación de mundos válidos y detección exacta de la cantidad de errores en mundos inválidos (`Motor.Mundo`).
3. Lógica pura del juego (`Motor.Nucleo`): movimientos, choque contra salidas inexistentes, choque contra salidas bloqueadas, paso con requisitos, tomar y soltar ítems, examinar objetos.
4. Lógica de la variante: combinación conmutativa de ingredientes, rechazo por falta de insumos o receta inexistente.
5. Secuencias de comandos puras que conducen a la victoria tanto en el mundo de ejemplo como en el mundo propio.

Para ejecutar todas las pruebas:
```bash
cabal test --test-show-details=always
```

---

## 5. Estructura del Proyecto

```
.
├── aventura.cabal               # Configuración del paquete Cabal
├── src/
│   └── Motor/
│       ├── Tipos.hs             # Modelado con tipos algebraicos del dominio
│       ├── Reconocedor.hs       # Combinadores monádicos basados en ReadS
│       ├── Mundo.hs             # Parser y validación exhaustiva V1 a V10
│       ├── Comandos.hs          # Parser puro de comandos del jugador
│       ├── Nucleo.hs            # Motor puro de ejecución y reglas del juego
│       └── Persistencia.hs      # Entrada/Salida: lectura de mundos y partidas
├── app/
│   └── Main.hs                  # Punto de entrada CLI y bucle interactivo
├── test/
│   └── Spec.hs                  # Suite de pruebas automatizadas
├── mundos/
│   ├── mundo.txt                # Mundo original (9 salas, 8 objetos, 2 bloqueos, usa recetas)
│   └── invalidos/
│       ├── invalido1.txt        # Errores: V5, V10, y receta con objeto inexistente
│       ├── invalido2.txt        # Errores: V2, V6, V7
│       └── invalido3.txt        # Errores: V3, V8, V9
├── guiones/
│   └── mundo.txt                # Guion que gana mundos/mundo.txt
├── DECISIONES.md                # 6 Decisiones arquitectónicas en formato ADR
├── IA.md                        # Declaración transparente de uso de IA
└── README.md                    # Documentación general del proyecto
```
