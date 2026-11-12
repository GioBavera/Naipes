# Naipes

**Versión 1.0**

**Naipes** es un lenguaje de dominio específico (DSL) **externo**, con sintaxis propia **en español**, para describir las reglas de un juego de cartas. Los programas se escriben en archivos `.naipes`. Un intérprete escrito en **Haskell** lee esa descripción y **simula una partida completa**, sin intervención humana.

- **El lenguaje describe** las reglas de un juego (no una partida en particular).
- **El programa** interpreta esas reglas y las ejecuta.
- **El resultado** es la traza de la partida, jugada por jugada, y el ganador.

## Índice

1. [Uso](#uso)
2. [Ejemplo completo](#ejemplo-completo)
3. [Estructura del proyecto](#estructura-del-proyecto)
4. [El lenguaje](#el-lenguaje)
5. [Cómo se desarrolla una partida](#cómo-se-desarrolla-una-partida)
6. [Juegos incluidos](#juegos-incluidos)
7. [Errores](#errores)
8. [Decisiones de diseño](#decisiones-de-diseño)
9. [Alcance y limitaciones](#alcance-y-limitaciones)
10. [Trabajo pendiente](#trabajo-pendiente)

## Uso

### Requisitos

GHC (probado con la versión 9.4.7). Solo se usan paquetes que vienen incluidos con GHC: `parsec`, `mtl`, `containers` y `time`. No hace falta Cabal ni Stack.

### Desde GHCi

```
$ ghci Main.hs
> simular "juegos/siete_y_medio.naipes"
> simularCon 6 "juegos/siete_y_medio.naipes"
```

Si GHCi informa que algún módulo pertenece a un paquete oculto (`hidden package`), hay que indicarle los paquetes al arrancar:

```
$ ghci -package mtl -package containers -package parsec -package time Main.hs
```

Esto pasa cuando la instalación tiene un entorno de paquetes por defecto (lo crea `cabal install --lib`), que oculta los paquetes que no figuran en él.

| Función | Qué hace |
|---|---|
| `simular archivo` | Simula una partida con una semilla tomada del reloj. |
| `simularCon semilla archivo` | Simula una partida con la semilla indicada. La misma semilla produce siempre la misma partida. |

La traza siempre muestra la semilla usada, así que cualquier partida se puede repetir con `simularCon`.

### Compilado

```
$ ghc -O Main.hs -o naipes
$ ./naipes juegos/siete_y_medio.naipes        # semilla del reloj
$ ./naipes juegos/siete_y_medio.naipes 6      # semilla fija
```

## Ejemplo completo

### Entrada: `juegos/siete_y_medio.naipes`

```
juego "Siete y medio"

mazo española sin 8, 9

zonas de jugador: mano

-- Las figuras (10, 11 y 12) valen medio punto.
valor de carta: si rango(carta) >= 10 entonces 0.5 sino rango(carta)

estrategia prudente:
  plantarse cuando suma(mano) >= 5
  pedir
fin

estrategia arriesgada:
  plantarse cuando suma(mano) >= 7
  pedir
fin

jugadores:
  Ana  usa prudente
  Luis usa arriesgada
  Sol  usa azar

preparacion:
  mezclar mazo
  repartir 1 de mazo a mano
fin

turno:
  jugada pedir:
    mover 1 de mazo a mano
    mostrar "suma" suma(mano)
    si suma(mano) > 7.5 entonces
      mostrar "se pasa de siete y medio"
      eliminar
    sino
      repetir turno
    fin
  fin

  jugada plantarse:
    plantar
  fin
fin

gana mayor suma(mano)
```

### Salida: `simularCon 6 "juegos/siete_y_medio.naipes"`

```
== Siete y medio ==
Semilla: 6
Jugadores: Ana (prudente), Luis (arriesgada), Sol (azar)

Preparación
      se mezcla mazo
      Ana recibe: 7 de oros
      Luis recibe: 1 de copas
      Sol recibe: 11 de espadas

Ronda 1
  Ana: plantarse
      Ana se planta
  Luis: pedir
      mazo -> mano de Luis: 11 de oros
      Luis: suma 1.5
  Luis: pedir
      mazo -> mano de Luis: 4 de copas
      Luis: suma 5.5
  Luis: pedir
      mazo -> mano de Luis: 1 de oros
      Luis: suma 6.5
  Luis: pedir
      mazo -> mano de Luis: 7 de copas
      Luis: suma 13.5
      Luis: se pasa de siete y medio
      Luis queda fuera del juego
  Sol: pedir
      mazo -> mano de Sol: 4 de bastos
      Sol: suma 4.5
  Sol: pedir
      mazo -> mano de Sol: 1 de bastos
      Sol: suma 5.5
  Sol: plantarse
      Sol se planta

Fin de la partida: no quedan jugadores por jugar.

Resultado (gana el mayor puntaje)
  Ana   7
  Luis  13.5  (fuera del juego)
  Sol   5.5

Gana Ana
```

## Estructura del proyecto

```
AST.hs         Tipos de datos del lenguaje
Parser.hs      Del texto al AST (Parsec)
Evaluador.hs   Validación de las reglas y simulación
Main.hs        Entrada y salida
juegos/        Juegos de ejemplo
  guerra.naipes
  siete_y_medio.naipes
  ocho_loco.naipes
  ...
```

El recorrido de los datos es:

```
archivo .naipes --Parser--> Juego (AST) --validar--> Juego --simularJuego--> traza
```

| Módulo | Responsabilidad | Funciones principales |
|---|---|---|
| `AST.hs` | Define qué es un juego, una jugada, un efecto, una condición y una expresión. No tiene lógica de ejecución. | Tipos `Juego`, `Jugada`, `Efecto`, `Cond`, `Expr` |
| `Parser.hs` | Analiza el texto y construye el `Juego`. Informa los errores de sintaxis en español, con línea y columna. | `parsearJuego :: FilePath -> String -> Either String Juego` |
| `Evaluador.hs` | Revisa que las reglas tengan sentido y simula la partida. Es un módulo puro. | `validar :: Juego -> [String]`, `simularJuego :: Juego -> Int -> [String]` |
| `Main.hs` | Lee el archivo, encadena los pasos e imprime. Es el único módulo con entrada y salida. | `simular`, `simularCon`, `main` |

En el evaluador, la simulación corre en la mónada `StateT Estado (Writer [String])`: el estado de la partida cambia con cada efecto y la traza se va acumulando.

## El lenguaje

### Reglas generales

- Los comentarios empiezan con `--` y llegan hasta el fin de la línea.
- Los espacios y saltos de línea no tienen significado. Los bloques se cierran con `fin`.
- Las secciones van **en un orden fijo**, el de la tabla siguiente.
- Los nombres (de zonas, jugadas, estrategias y jugadores) son identificadores: letras, dígitos y `_`. No pueden coincidir con una palabra reservada.

### Secciones de un archivo

| # | Sección | Obligatoria | Qué define |
|---|---|---|---|
| 1 | `juego "Nombre"` | Sí | El nombre del juego |
| 2 | `mazo ...` | Sí | La baraja y qué rangos se quitan |
| 3 | `zonas comunes: ...` | No | Zonas compartidas por todos |
| 4 | `zonas de jugador: ...` | No | Zonas que tiene cada jugador |
| 5 | `variables comunes: ...` | No | Datos compartidos que el juego recuerda |
| 6 | `variables de jugador: ...` | No | Datos que el juego recuerda de cada jugador |
| 7 | `valor de carta: ...` | No | Cuánto vale cada carta |
| 8 | `estrategia ...` | No (varias) | Cómo decide un jugador |
| 9 | `jugadores: ...` | Sí | Quiénes juegan y con qué estrategia |
| 10 | `preparacion: ... fin` | Sí | Qué se hace antes de empezar |
| 11 | `turno: ... fin` | Sí | Las jugadas posibles |
| 12 | `cierre de ronda: ... fin` | No | Qué pasa cuando todos jugaron |
| 13 | `termina cuando ...` | No | Condición de fin |
| 14 | `gana mayor ...` / `gana menor ...` | Sí | Cómo se decide el ganador |
| 15 | `limite N turnos` | No | Máximo de turnos (por defecto, 1000) |

### Mazo

```
mazo española
mazo española sin 8, 9
mazo francesa
```

| Baraja | Rangos | Palos |
|---|---|---|
| `española` | 1 a 12 | `oros`, `copas`, `espadas`, `bastos` |
| `francesa` | 1 a 13 | `corazones`, `diamantes`, `treboles`, `picas` |

En la baraja francesa, las reglas usan números (1, 11, 12, 13) y la traza muestra A, J, Q y K.

### Zonas

Una zona es un lugar donde hay cartas, ordenadas: la primera es la de **arriba** y la última es la del **fondo**.

```
zonas comunes: descarte
zonas de jugador: mano
```

- La zona común `mazo` siempre existe y empieza con todas las cartas.
- Una zona de jugador existe una vez por cada jugador.

Cómo se nombra una zona dentro de las reglas:

| Forma | Significado |
|---|---|
| `descarte` | La zona común |
| `mano` | La zona del jugador actual |
| `mano del siguiente` | La zona del próximo jugador en el sentido de juego |
| `mano de todos` | La zona de cada uno de los jugadores |

### Variables

Una variable es un dato que el juego recuerda de un turno a otro: un contador, puntos acumulados o un palo.

```
variables comunes: manos = 0, triunfo = ninguno
variables de jugador: puntos = 0
```

- Una variable común tiene un solo valor. Una variable de jugador tiene un valor por cada jugador.
- Toda variable se declara con su valor inicial: un número, un palo o `ninguno`.
- El tipo (número o palo) sale del valor inicial y no cambia.
- `ninguno` es un palo que no coincide con ningún palo real. Es también el palo de `tope(Z)` cuando la zona está vacía.

Se cambian con el efecto `asignar` y se usan en cualquier expresión por su nombre:

```
asignar puntos = puntos + suma(mesa de todos)
asignar triunfo = palo(tope(muestra))
...
gana mayor puntos
```

Una variable de jugador, sin aclaración, es la del jugador actual, igual que las zonas de jugador. Por eso no se puede usar donde no hay jugador actual (por ejemplo, en `termina cuando`, salvo dentro de `alguno(...)` o `todos(...)`).

En `valor de carta` se pueden usar variables comunes, pero no de jugador.

### Valor de las cartas

```
valor de carta: si rango(carta) >= 10 entonces 0.5 sino rango(carta)
```

Es una expresión sobre `carta`. Si no se define, el valor de una carta es su rango. La usan `valor(...)` y `suma(...)`.

Puede depender de una variable común, por ejemplo un palo de triunfo que se sortea en cada partida:

```
valor de carta: rango(carta) + si palo(carta) = triunfo entonces 100 sino 0
```

### Expresiones

| Expresión | Resultado |
|---|---|
| `5`, `7.5` | Un número |
| `oros`, `picas`, ... | Un palo |
| `ninguno` | El palo que no coincide con ninguno real |
| `NOMBRE` | El valor de una variable |
| `rango(C)` | El rango de una carta |
| `palo(C)` | El palo de una carta |
| `valor(C)` | El valor de una carta, según `valor de carta` |
| `suma(Z)` | La suma de los valores de las cartas de una zona |
| `cantidad(Z)` | La cantidad de cartas de una zona |
| `E + E`, `E - E`, `E * E`, `E / E`, `-E` | Aritmética |
| `si COND entonces E sino E` | Elección |

Donde `C` es una carta: `carta` (la carta elegida en la jugada) o `tope(Z)` (la carta de arriba de una zona).

Si la zona está vacía, `tope(Z)` tiene rango 0, valor 0 y palo `ninguno`.

### Condiciones

| Condición | Se cumple cuando |
|---|---|
| `E = E`, `E != E` | Los valores son iguales o distintos (números o palos) |
| `E < E`, `E <= E`, `E > E`, `E >= E` | Comparación entre números |
| `COND y COND`, `COND o COND`, `no COND` | Operadores lógicos (`no` liga más fuerte, luego `y`, luego `o`) |
| `alguno(COND)` | Algún jugador cumple la condición |
| `todos(COND)` | Todos los jugadores la cumplen |
| `puede(JUGADA)` | El jugador actual tiene disponible esa jugada |

`alguno` y `todos` evalúan la condición una vez por cada jugador que no fue eliminado, tomándolo como jugador actual.

### Jugadas

La sección `turno` lista las jugadas que un jugador puede hacer cuando le toca.

```
jugada NOMBRE [con carta de ZONA [donde COND]] [cuando COND]:
  efectos
fin
```

- **`cuando COND`**: la jugada solo es legal si se cumple la condición.
- **`con carta de ZONA`**: la jugada se hace con una carta de esa zona. Hay una opción por cada carta, y `donde COND` filtra cuáles sirven. Dentro de la jugada, `carta` es la elegida.

Esta parte del lenguaje es **declarativa**: se dice qué jugadas son válidas, y el intérprete calcula las opciones del jugador en cada turno.

```
jugada tirar con carta de mano donde
    rango(carta) = 8
    o rango(carta) = rango(tope(descarte))
    o palo(carta) = palo(tope(descarte)):
  mover carta a descarte
fin

jugada robar cuando no puede(tirar):
  mover 1 de mazo a mano
fin
```

`puede(X)` solo puede referirse a una jugada declarada antes, para evitar definiciones circulares.

### Efectos

Los efectos son la parte **imperativa**: se ejecutan en orden. Se usan en la preparación, en las jugadas y en el cierre de ronda.

| Efecto | Qué hace |
|---|---|
| `mezclar Z` | Mezcla la zona |
| `mover N de Z1 a Z2` | Pasa N cartas de arriba de Z1 a arriba de Z2 |
| `mover todo de Z1 a Z2` | Pasa todas las cartas |
| `mover carta a Z` | Pasa la carta elegida en la jugada |
| `repartir N de Z a ZJ` | Da N cartas a cada jugador, de a una, en su zona ZJ |
| `repartir todo de Z a ZJ` | Reparte hasta que no queden cartas |
| `si COND entonces ... [sino ...] fin` | Ejecuta unos efectos u otros |
| `plantar` | El jugador actual deja de jugar, pero sigue en carrera |
| `eliminar` | El jugador actual queda fuera y no puede ganar |
| `repetir turno` | El jugador actual vuelve a jugar |
| `saltear siguiente` | El próximo jugador pierde el turno |
| `invertir sentido` | Cambia el sentido de juego |
| `mostrar "texto" [E]` | Escribe una línea en la traza, con un valor opcional |
| `asignar VARIABLE = E` | Cambia el valor de una variable; el cambio queda anotado en la traza |
| `cada jugador: ... fin` | Ejecuta los efectos una vez por cada jugador |
| `mayor en E: ... [empate: ...] fin` | Ejecuta los efectos para el jugador con el mayor valor de E |
| `menor en E: ... [empate: ...] fin` | Lo mismo, para el menor valor |

Detalles:

- En `mover` y `repartir`, el origen y el destino admiten `fondo de`: `mover 1 de fondo de descarte a mazo`, `mover todo de botin a fondo de pila`.
- Si se piden más cartas de las que hay, se mueven las que haya.
- La cantidad puede ser una expresión: `mover cantidad(descarte) - 1 de fondo de descarte a mazo`.
- Con origen `Z de todos`, se toma esa cantidad de la zona de cada jugador.
- En `mayor en` y `menor en`, si hay un único jugador destacado, los efectos se ejecutan con él como jugador actual. Si hay empate, se ejecuta el bloque `empate` (si existe).

### Estrategias

Como no hay jugadores humanos, cada jugador sigue una estrategia: una lista de preferencias, de mayor a menor prioridad.

```
estrategia NOMBRE:
  JUGADA [con mayor E | con menor E] [cuando COND]
  ...
fin
```

En cada turno se usa **la primera regla aplicable**: la jugada tiene que ser legal y, si hay `cuando`, se tiene que cumplir la condición.

- `con mayor E` / `con menor E` elige con qué carta hacer la jugada (solo para jugadas `con carta`). Sin esa aclaración, la carta se elige al azar.
- Si ninguna regla es aplicable, se elige al azar entre las jugadas legales.
- `azar` es una estrategia predefinida que siempre elige al azar.

```
estrategia prudente:
  plantarse cuando suma(mano) >= 5
  pedir
fin

estrategia descargadora:
  tirar con mayor rango(carta)
fin
```

### Jugadores

```
jugadores:
  Ana  usa prudente
  Luis usa azar
```

Tiene que haber al menos dos. El orden en que se escriben es el orden de juego.

### Fin y ganador

```
termina cuando alguno(cantidad(mano) = 0)
gana menor cantidad(mano)
limite 2000 turnos
```

- `termina cuando` se evalúa sin jugador actual, así que las zonas de jugador van dentro de `alguno(...)` o `todos(...)`.
- `gana mayor E` / `gana menor E`: se calcula E para cada jugador no eliminado. Si hay más de uno con el mejor puntaje, es empate.

### Gramática

```
juego      ::= 'juego' TEXTO
               'mazo' ('española' | 'francesa') ['sin' NAT (',' NAT)*]
               ['zonas' 'comunes' ':' nombres]
               ['zonas' 'de' 'jugador' ':' nombres]
               ['variables' 'comunes' ':' variables]
               ['variables' 'de' 'jugador' ':' variables]
               ['valor' 'de' 'carta' ':' expr]
               estrategia*
               'jugadores' ':' (ID 'usa' ID)+
               'preparacion' ':' efecto* 'fin'
               'turno' ':' jugada+ 'fin'
               ['cierre' 'de' 'ronda' ':' efecto* 'fin']
               ['termina' 'cuando' cond]
               'gana' criterio expr
               ['limite' NAT 'turnos']

nombres    ::= ID (',' ID)*
variables  ::= ID '=' inicial (',' ID '=' inicial)*
inicial    ::= NUM | '-' NUM | PALO | 'ninguno'
criterio   ::= 'mayor' | 'menor'

estrategia ::= 'estrategia' ID ':' regla+ 'fin'
regla      ::= ID ['con' criterio expr] ['cuando' cond]

jugada     ::= 'jugada' ID ['con' 'carta' 'de' ID ['donde' cond]]
               ['cuando' cond] ':' efecto* 'fin'

efecto     ::= 'mezclar' zona
             | 'mover' 'carta' 'a' lugar
             | 'mover' cantidad 'de' lugar 'a' lugar
             | 'repartir' cantidad 'de' lugar 'a' ID
             | 'si' cond 'entonces' efecto* ['sino' efecto*] 'fin'
             | 'plantar' | 'eliminar'
             | 'repetir' 'turno' | 'saltear' 'siguiente' | 'invertir' 'sentido'
             | 'mostrar' TEXTO [expr]
             | 'asignar' ID '=' expr
             | 'cada' 'jugador' ':' efecto* 'fin'
             | criterio 'en' expr ':' efecto* ['empate' ':' efecto*] 'fin'

cantidad   ::= 'todo' | expr
lugar      ::= ['fondo' 'de'] zona
zona       ::= ID ['de' 'todos' | 'del' 'siguiente']

expr       ::= expr ('+' | '-' | '*' | '/') expr | '-' expr | '(' expr ')'
             | NUM | PALO | 'ninguno' | ID
             | ('rango' | 'palo' | 'valor') '(' carta ')'
             | ('suma' | 'cantidad') '(' zona ')'
             | 'si' cond 'entonces' expr 'sino' expr
carta      ::= 'carta' | 'tope' '(' zona ')'

cond       ::= cond 'y' cond | cond 'o' cond | 'no' cond | '(' cond ')'
             | expr ('=' | '!=' | '<' | '<=' | '>' | '>=') expr
             | ('alguno' | 'todos') '(' cond ')'
             | 'puede' '(' ID ')'
```

También se aceptan `espanola`, `preparación` y `límite`.

## Cómo se desarrolla una partida

1. **Preparación.** Todas las cartas están en `mazo`. Se ejecutan los efectos de `preparacion`.
2. **Turno.** Empieza el primer jugador de la lista. En su turno:
   1. Se calculan sus jugadas legales.
   2. Si no tiene ninguna, pasa.
   3. Si tiene, su estrategia elige una y se ejecutan sus efectos.
   4. Si la jugada ejecutó `repetir turno` y el jugador sigue activo, vuelve a jugar.
3. **Cambio de turno.** Pasa al próximo jugador activo, según el sentido de juego. Los jugadores plantados o eliminados no juegan más. Si hubo `saltear siguiente`, ese jugador pierde el turno.
4. **Cierre de ronda.** Cuando todos los jugadores activos tuvieron su turno, termina la ronda y se ejecutan los efectos de `cierre de ronda`.
5. **Fin.** La partida termina cuando ocurre alguna de estas cosas:

| Motivo | Cuándo se revisa | Resultado |
|---|---|---|
| Se cumple `termina cuando` | Después de cada jugada y de cada cierre de ronda | Se aplica `gana` |
| No quedan jugadores activos | Después de cada jugada | Se aplica `gana` |
| Nadie pudo jugar en toda una ronda | Al cerrar la ronda | Se aplica `gana` |
| Se alcanza el límite de turnos | Antes de cada turno | Empate |

## Juegos incluidos

| Archivo | Juego | Qué pone a prueba |
|---|---|---|
| `guerra.naipes` | Guerra | Reparto, zonas de jugador, cierre de ronda, `mayor en`. No tiene decisiones. |
| `siete_y_medio.naipes` | Siete y medio | Valor de cartas, `suma`, estrategias, `plantar`, `eliminar`, `repetir turno`. |
| `ocho_loco.naipes` | Ocho loco | Jugadas con carta y filtro, `puede`, cartas con efecto (`saltear`, `invertir`, hacer robar al siguiente), rearmado del mazo. |

Los tres se escriben con las mismas construcciones: ninguno necesitó una primitiva hecha a medida.

Otros juegos, escritos después sin modificar el intérprete:

| Archivo | Juego | Qué pone a prueba |
|---|---|---|
| `blackjack.naipes` | Blackjack | Lo mismo que Siete y medio, con otra baraja, y la banca como un jugador con estrategia propia. |
| `la_mayor.naipes` | La mayor | Rondas donde todos tiran una carta y gana la más alta. Una variable de jugador cuenta las rondas ganadas. |
| `triunfo.naipes` | La mayor con triunfo | Una variable común de tipo palo: el triunfo se sortea en cada partida y `valor de carta` depende de él. |
| `tres_manos.naipes` | Tres manos | Puntos acumulados y un contador de manos: se reparte tres veces en la misma partida. |
| `treinta_y_uno.naipes` | Treinta y uno | Una suma compartida en una zona común; se pierde al pasarse. |
| `burro.naipes` | Burro | Seguir el palo y robar varias veces en un turno. |
| `casita_robada.naipes` | Casita robada | Zonas del rival (`del siguiente`) y reparto repetido en el cierre de ronda. |

Simplificaciones respecto de las reglas habituales:

- **Guerra**: en un empate no se agregan cartas boca abajo; las cartas quedan en la mesa y la batalla siguiente decide quién se lleva todo.
- **Siete y medio**: no hay banca ni apuestas; todos juegan contra todos.
- **Ocho loco**: quien tira un 8 no elige palo; el siguiente sigue el palo o el rango de ese 8.

## Errores

Si el archivo tiene errores, no se simula nada.

**Errores de sintaxis**, detectados por el parser, con línea y columna:

```
Error de sintaxis en línea 23, columna 8:
no se esperaba "p"
se esperaba "usa"
```

**Errores en las reglas**, detectados por `validar` antes de simular. Se informan todos juntos:

```
Errores en las reglas:
  - El jugador Sol usa la estrategia "loca", que no existe.
  - En "valor de carta": se compara un número con un palo.
  - En la jugada "pedir": la zona "descarte" no está declarada.
```

Qué se valida:

- Zonas, variables, jugadas y estrategias: que existan y no estén repetidas.
- Variables: que a cada una se le asigne un valor de su tipo.
- Tipos: no mezclar números con palos; los palos solo admiten `=` y `!=`.
- Palos y rangos: que pertenezcan a la baraja elegida.
- Contexto: no usar `carta` donde no hay una carta elegida, ni una zona o variable de jugador donde no hay jugador actual, ni `repetir turno` fuera de una jugada.
- `puede(X)`: que se refiera a una jugada declarada antes.
- Que haya al menos dos jugadores, con nombres distintos.

## Decisiones de diseño

- **Intérprete, no traductor.** Las reglas se ejecutan directamente; no se genera código Haskell.
- **Simulación, no juego interactivo.** No hay entrada del usuario durante la partida. Las decisiones las toman las estrategias, escritas en el mismo lenguaje.
- **Mezcla de estilos.** Lo que es un hecho se declara (mazo, zonas, valor, jugadas legales, fin, ganador); lo que es una secuencia se escribe como pasos (preparación y efectos).
- **Evaluador puro.** `simularJuego` no hace entrada ni salida: recibe el juego y una semilla y devuelve la traza. Por eso las partidas son reproducibles.
- **Generador de azar propio.** Un generador congruencial lineal, para no depender del paquete `random`, que no viene con GHC.
- **Validación antes de ejecutar.** Los errores de las reglas se detectan todos de una vez y antes de simular, en lugar de aparecer en mitad de una partida.
- **Sintaxis sin indentación significativa.** Los bloques se cierran con `fin`, lo que simplifica el parser y los mensajes de error.

## Alcance y limitaciones

**Qué se puede representar:** juegos por turnos, de dos o más jugadores, con baraja española o francesa, donde las jugadas consisten en mover cartas entre zonas y el ganador se decide con una expresión sobre el estado final.

**Qué queda afuera:**

- Apuestas y faroleo (póker).
- Cantos y respuestas fuera de turno (Truco).
- Cartas con texto propio (Magic).
- Juegos de tiempo real.
- Juegos que necesitan combinar varias cartas en una jugada (Escoba de 15) o reconocer escaleras y grupos (Chinchón).

**Limitaciones de esta versión:**

- Los errores de validación indican la sección o jugada, pero no la línea.
- Algunos errores de sintaxis dentro de una condición señalan el comienzo de la condición y no el punto exacto.
- Una jugada elige una sola carta.
- Las estrategias ven todo el estado de la partida: no se modela la información oculta.
- Con cambios de sentido, las rondas dejan de ser vueltas completas: un jugador puede jugar dos veces en la misma ronda.
- Las secciones del archivo van en orden fijo.
- La traza escribe "Cierre de ronda" aunque en esa ronda el cierre no haga nada.
- Una variable de jugador solo se puede leer y cambiar para el jugador actual: no existe `puntos del siguiente`.

## Trabajo pendiente

- Números de línea en los errores de validación.
- Un efecto para que el ganador de una ronda empiece la siguiente (Burro).
- Jugadas que elijan un palo además de una carta (el 8 del Ocho loco).
- Volver a poner en juego a un jugador plantado o eliminado, para jugar varias manos en juegos como Siete y medio.
- Jugadas con varias cartas, para llegar a la Escoba de 15.
- Estadísticas sobre muchas partidas, llamando a `simularJuego` con distintas semillas.
