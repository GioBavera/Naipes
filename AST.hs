-- | Árbol de sintaxis abstracta de Naipes, un lenguaje para describir
-- juegos de cartas.
--
-- Este módulo solo define tipos de datos: qué es un juego, una jugada,
-- un efecto, una condición y una expresión. No tiene lógica de ejecución.
module AST where

-- ---------------------------------------------------------------------
-- Cartas y barajas
-- ---------------------------------------------------------------------

-- | Una carta: su número y su palo.
data Carta = Carta
  { rangoDe :: Int
  , paloDe  :: String
  } deriving (Eq, Show)

data Baraja = Espanola | Francesa
  deriving (Eq, Show)

-- | Palos de cada baraja, en el orden en que se arma el mazo.
palosDe :: Baraja -> [String]
palosDe Espanola = ["oros", "copas", "espadas", "bastos"]
palosDe Francesa = ["corazones", "diamantes", "treboles", "picas"]

-- | Rangos de cada baraja (en la francesa, 11 = J, 12 = Q, 13 = K).
rangosDe :: Baraja -> [Int]
rangosDe Espanola = [1 .. 12]
rangosDe Francesa = [1 .. 13]

-- ---------------------------------------------------------------------
-- El juego completo
-- ---------------------------------------------------------------------

-- | Un programa del lenguaje: las reglas de un juego.
data Juego = Juego
  { nombre       :: String
  , baraja       :: Baraja
  , sinRangos    :: [Int]               -- ^ rangos que se quitan del mazo
  , zonasComunes :: [String]            -- ^ además de "mazo", que siempre existe
  , zonasJugador :: [String]            -- ^ cada jugador tiene una de cada una
  , varsComunes  :: [(String, Expr)]    -- ^ variables compartidas, con su valor inicial
  , varsJugador  :: [(String, Expr)]    -- ^ variables que tiene cada jugador
  , valorCarta   :: Maybe Expr          -- ^ si no se define, el valor es el rango
  , estrategias  :: [Estrategia]
  , jugadores    :: [(String, String)]  -- ^ (nombre, estrategia)
  , preparacion  :: [Efecto]
  , jugadas      :: [Jugada]
  , cierreRonda  :: [Efecto]
  , termina      :: Maybe Cond
  , ganador      :: (Criterio, Expr)
  , limite       :: Int                 -- ^ máximo de turnos
  } deriving Show

-- | Una jugada posible en el turno de un jugador.
data Jugada = Jugada
  { jNombre  :: String
  , jCarta   :: Maybe (String, Maybe Cond)  -- ^ zona de donde se elige una carta, y filtro
  , jCuando  :: Maybe Cond                  -- ^ condición para que la jugada sea legal
  , jEfectos :: [Efecto]
  } deriving Show

-- | Una estrategia: lista de preferencias, de mayor a menor prioridad.
data Estrategia = Estrategia
  { eNombre :: String
  , eReglas :: [Regla]
  } deriving Show

data Regla = Regla
  { rJugada      :: String
  , rPreferencia :: Maybe (Criterio, Expr)  -- ^ qué carta elegir, si la jugada usa carta
  , rCuando      :: Maybe Cond
  } deriving Show

data Criterio = Mayor | Menor
  deriving (Eq, Show)

-- ---------------------------------------------------------------------
-- Expresiones y condiciones
-- ---------------------------------------------------------------------

data Expr
  = Num Double
  | PaloLit String             -- ^ oros, copas, picas...
  | Ninguno                    -- ^ el palo "ninguno": no coincide con ningún palo real
  | Var String                 -- ^ una variable del juego
  | Rango RefCarta             -- ^ rango(carta)
  | Palo RefCarta              -- ^ palo(carta)
  | Valor RefCarta             -- ^ valor(carta), según "valor de carta"
  | Suma RefZona               -- ^ suma de los valores de una zona
  | Cantidad RefZona           -- ^ cantidad de cartas de una zona
  | Arit OpArit Expr Expr
  | SiExpr Cond Expr Expr      -- ^ si C entonces E1 sino E2
  deriving Show

data OpArit = Mas | Menos | Por | Dividido
  deriving (Eq, Show)

-- | A qué carta se refiere una expresión.
data RefCarta
  = LaCarta                    -- ^ la carta elegida en la jugada
  | Tope RefZona               -- ^ la carta de arriba de una zona
  deriving Show

-- | Referencia a una zona. Una zona de jugador sin aclaración es la del
-- jugador actual.
data RefZona = RefZona String Alcance
  deriving Show

data Alcance
  = Directa                    -- ^ zona común, o la del jugador actual
  | DeTodos                    -- ^ "de todos": la de cada jugador
  | DelSiguiente               -- ^ "del siguiente": la del próximo jugador
  deriving (Eq, Show)

data Cond
  = Comparar OpRel Expr Expr
  | Y Cond Cond
  | O Cond Cond
  | No Cond
  | Alguno Cond                -- ^ algún jugador cumple la condición
  | Todos Cond                 -- ^ todos los jugadores la cumplen
  | Puede String               -- ^ el jugador actual tiene disponible esa jugada
  deriving Show

data OpRel = Igual | Distinto | MenorQue | MenorIgual | MayorQue | MayorIgual
  deriving (Eq, Show)

-- ---------------------------------------------------------------------
-- Efectos
-- ---------------------------------------------------------------------

data Efecto
  = Mezclar RefZona
  | Mover Cantidad Extremo RefZona Extremo RefZona  -- ^ mover N de O a D
  | MoverCarta Extremo RefZona                      -- ^ mover carta a D
  | Repartir Cantidad Extremo RefZona String        -- ^ repartir N de O a zona de jugador
  | Si Cond [Efecto] [Efecto]
  | Plantar                                         -- ^ el jugador actual deja de jugar
  | Eliminar                                        -- ^ el jugador actual queda fuera
  | RepetirTurno
  | SaltearSiguiente
  | InvertirSentido
  | Mostrar String (Maybe Expr)
  | Asignar String Expr                             -- ^ asignar VARIABLE = E
  | CadaJugador [Efecto]
  | Destacado Criterio Expr [Efecto] [Efecto]       -- ^ mayor/menor en E: ... empate: ...
  deriving Show

data Cantidad = Todo | Cant Expr
  deriving Show

-- | De qué extremo de la zona se saca o se pone.
data Extremo = Arriba | Abajo
  deriving (Eq, Show)
