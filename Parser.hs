-- | Analizador sintáctico: convierte el texto de un archivo de reglas en
-- un valor de tipo 'Juego'.
module Parser (parsearJuego) where

import Data.Functor.Identity (Identity)
import Data.List (isPrefixOf)
import Text.Parsec
import Text.Parsec.Error (errorMessages, showErrorMessages)
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as T

import AST

-- | Analiza el contenido de un archivo. Devuelve el juego o un mensaje de
-- error en español con línea y columna.
parsearJuego :: FilePath -> String -> Either String Juego
parsearJuego archivo texto =
  case parse juegoP archivo texto of
    Right j -> Right j
    Left e  -> Left (mostrarError e)

mostrarError :: ParseError -> String
mostrarError e =
  "Error de sintaxis en línea " ++ show (sourceLine p)
    ++ ", columna " ++ show (sourceColumn p) ++ ":"
    ++ traducir (showErrorMessages "o" "error desconocido" "se esperaba"
         "no se esperaba" "el fin del archivo" (errorMessages e))
  where p = errorPos e

-- | Pasa al español los textos que Parsec genera internamente en inglés.
traducir :: String -> String
traducir = reemplazar
  [ ("reserved word ", "la palabra reservada ")
  , ("letter or digit", "letra o dígito")
  , ("end of input", "el fin del archivo")
  , ("end of ", "el fin de ")
  , ("operator", "un operador")
  , ("literal string", "un texto entre comillas")
  , ("string character", "un carácter del texto")
  , ("end of string", "comillas de cierre")
  , ("natural", "un número natural")
  , ("number", "un número")
  , ("digit", "dígito")
  , ("letter", "letra")
  , ("space", "espacio")
  ]

reemplazar :: [(String, String)] -> String -> String
reemplazar _ [] = []
reemplazar tabla s@(c : resto) =
  case [(viejo, nuevo) | (viejo, nuevo) <- tabla, viejo `isPrefixOf` s] of
    (viejo, nuevo) : _ -> nuevo ++ reemplazar tabla (drop (length viejo) s)
    [] -> c : reemplazar tabla resto

-- ---------------------------------------------------------------------
-- Léxico
-- ---------------------------------------------------------------------

lexico :: T.TokenParser ()
lexico = T.makeTokenParser emptyDef
  { T.commentLine     = "--"
  , T.identStart      = letter <|> char '_'
  , T.identLetter     = alphaNum <|> char '_'
  , T.reservedNames   = reservadas
  , T.reservedOpNames = ["+", "-", "*", "/", "=", "!=", "<", "<=", ">", ">="]
  }

reservadas :: [String]
reservadas =
  [ "juego", "mazo", "española", "espanola", "francesa", "sin"
  , "zonas", "comunes", "de", "del", "jugador", "valor", "carta"
  , "estrategia", "fin", "con", "mayor", "menor", "cuando"
  , "jugadores", "usa", "preparacion", "preparación", "turno", "jugada"
  , "donde", "cierre", "ronda", "termina", "gana", "limite", "límite", "turnos"
  , "mezclar", "mover", "todo", "a", "fondo", "repartir"
  , "si", "entonces", "sino", "plantar", "eliminar", "repetir"
  , "saltear", "siguiente", "invertir", "sentido", "mostrar"
  , "cada", "en", "empate", "todos", "alguno", "puede", "no", "y", "o"
  , "rango", "palo", "suma", "cantidad", "tope"
  , "variables", "asignar", "ninguno"
  ] ++ palosDe Espanola ++ palosDe Francesa

-- | Una palabra clave.
palabra :: String -> Parser ()
palabra = T.reserved lexico

-- | Varias palabras clave seguidas.
palabras :: [String] -> Parser ()
palabras = mapM_ palabra

-- | Una palabra clave que admite más de una escritura (con o sin tilde).
variantes :: [String] -> Parser ()
variantes = choice . map palabra

ident :: Parser String
ident = T.identifier lexico <?> "un nombre"

-- | Nombre de una zona: un identificador, o "mazo", que siempre existe.
nombreZona :: Parser String
nombreZona = "mazo" <$ palabra "mazo" <|> ident <?> "el nombre de una zona"

operador :: String -> Parser ()
operador = T.reservedOp lexico

dosPuntos :: Parser ()
dosPuntos = () <$ T.colon lexico

parentesis :: Parser a -> Parser a
parentesis = T.parens lexico

natural :: Parser Int
natural = fromInteger <$> T.natural lexico

numero :: Parser Double
numero = either fromInteger id <$> T.naturalOrFloat lexico

cadena :: Parser String
cadena = T.stringLiteral lexico

-- ---------------------------------------------------------------------
-- Estructura del archivo
-- ---------------------------------------------------------------------

juegoP :: Parser Juego
juegoP = do
  T.whiteSpace lexico
  palabra "juego"
  nom <- cadena <?> "el nombre del juego entre comillas"
  palabra "mazo"
  bar <- barajaP
  sinR <- option [] (palabra "sin" *> T.commaSep1 lexico natural)
  zc <- option [] (try (palabras ["zonas", "comunes"]) *> dosPuntos *> listaIdent)
  zj <- option [] (try (palabras ["zonas", "de", "jugador"]) *> dosPuntos *> listaIdent)
  vc <- option [] (try (palabras ["variables", "comunes"]) *> dosPuntos *> listaVars)
  vj <- option [] (try (palabras ["variables", "de", "jugador"]) *> dosPuntos *> listaVars)
  val <- optionMaybe (try (palabras ["valor", "de", "carta"]) *> dosPuntos *> expr)
  ests <- many estrategiaP
  palabra "jugadores" *> dosPuntos
  jugs <- many1 ((,) <$> ident <* palabra "usa" <*> ident)
  variantes ["preparacion", "preparación"] *> dosPuntos
  prep <- bloque
  palabra "turno" *> dosPuntos
  jgs <- many1 jugadaP <* palabra "fin"
  cierre <- option [] (palabras ["cierre", "de", "ronda"] *> dosPuntos *> bloque)
  ter <- optionMaybe (palabras ["termina", "cuando"] *> cond)
  palabra "gana"
  gan <- (,) <$> criterioP <*> expr
  lim <- option 1000 (variantes ["limite", "límite"] *> natural <* palabra "turnos")
  eof
  return Juego
    { nombre = nom, baraja = bar, sinRangos = sinR
    , zonasComunes = zc, zonasJugador = zj
    , varsComunes = vc, varsJugador = vj, valorCarta = val
    , estrategias = ests, jugadores = jugs, preparacion = prep
    , jugadas = jgs, cierreRonda = cierre, termina = ter
    , ganador = gan, limite = lim
    }

listaIdent :: Parser [String]
listaIdent = T.commaSep1 lexico ident

-- | NOMBRE = VALOR_INICIAL, ...
listaVars :: Parser [(String, Expr)]
listaVars = T.commaSep1 lexico ((,) <$> ident <* operador "=" <*> literal)

-- | Valor inicial de una variable: un número, un palo o "ninguno".
literal :: Parser Expr
literal = choice
  [ Num <$> numero
  , Num . negate <$> (operador "-" *> numero)
  , paloLit
  , Ninguno <$ palabra "ninguno"
  ] <?> "un valor inicial (número, palo o ninguno)"

paloLit :: Parser Expr
paloLit = PaloLit <$> choice [p <$ palabra p | p <- palosDe Espanola ++ palosDe Francesa]

barajaP :: Parser Baraja
barajaP = Espanola <$ variantes ["española", "espanola"]
      <|> Francesa <$ palabra "francesa"
      <?> "la baraja (española o francesa)"

criterioP :: Parser Criterio
criterioP = Mayor <$ palabra "mayor" <|> Menor <$ palabra "menor"

-- | estrategia NOMBRE: reglas... fin
estrategiaP :: Parser Estrategia
estrategiaP = do
  palabra "estrategia"
  nom <- ident
  dosPuntos
  reglas <- many1 reglaP
  palabra "fin"
  return (Estrategia nom reglas)

-- | JUGADA [con mayor|menor EXPR] [cuando COND]
reglaP :: Parser Regla
reglaP = do
  jg <- ident
  pref <- optionMaybe (palabra "con" *> ((,) <$> criterioP <*> expr))
  cu <- optionMaybe (palabra "cuando" *> cond)
  return (Regla jg pref cu)

-- | jugada NOMBRE [con carta de ZONA [donde COND]] [cuando COND]: efectos fin
jugadaP :: Parser Jugada
jugadaP = do
  palabra "jugada"
  nom <- ident
  car <- optionMaybe $ do
    palabras ["con", "carta", "de"]
    z <- nombreZona
    filtro <- optionMaybe (palabra "donde" *> cond)
    return (z, filtro)
  cu <- optionMaybe (palabra "cuando" *> cond)
  dosPuntos
  efs <- bloque
  return (Jugada nom car cu efs)

-- ---------------------------------------------------------------------
-- Efectos
-- ---------------------------------------------------------------------

-- | Una lista de efectos terminada en "fin".
bloque :: Parser [Efecto]
bloque = many efecto <* palabra "fin"

efecto :: Parser Efecto
efecto = choice
  [ Mezclar <$> (palabra "mezclar" *> zonaRef)
  , palabra "mover" *> moverP
  , repartirP
  , siP
  , Plantar <$ palabra "plantar"
  , Eliminar <$ palabra "eliminar"
  , RepetirTurno <$ palabras ["repetir", "turno"]
  , SaltearSiguiente <$ palabras ["saltear", "siguiente"]
  , InvertirSentido <$ palabras ["invertir", "sentido"]
  , Mostrar <$> (palabra "mostrar" *> cadena) <*> optionMaybe (try expr)
  , Asignar <$> (palabra "asignar" *> ident) <*> (operador "=" *> expr)
  , CadaJugador <$> (palabras ["cada", "jugador"] *> dosPuntos *> bloque)
  , destacadoP
  ] <?> "un efecto"

-- | mover carta a DESTINO  |  mover CANTIDAD de ORIGEN a DESTINO
moverP :: Parser Efecto
moverP = moverCarta <|> moverVarias
  where
    moverCarta = do
      palabras ["carta", "a"]
      (ext, z) <- extremoZona
      return (MoverCarta ext z)
    moverVarias = do
      n <- cantidadP
      palabra "de"
      (extO, o) <- extremoZona
      palabra "a"
      (extD, d) <- extremoZona
      return (Mover n extO o extD d)

-- | repartir CANTIDAD de ORIGEN a ZONA_DE_JUGADOR
repartirP :: Parser Efecto
repartirP = do
  palabra "repartir"
  n <- cantidadP
  palabra "de"
  (ext, o) <- extremoZona
  palabra "a"
  z <- ident
  return (Repartir n ext o z)

cantidadP :: Parser Cantidad
cantidadP = Todo <$ palabra "todo" <|> Cant <$> expr

-- | [fondo de] ZONA
extremoZona :: Parser (Extremo, RefZona)
extremoZona = (,) <$> option Arriba (Abajo <$ palabras ["fondo", "de"]) <*> zonaRef

-- | ZONA [de todos | del siguiente]
zonaRef :: Parser RefZona
zonaRef = RefZona <$> nombreZona <*> option Directa alcance
  where
    alcance = DeTodos <$ try (palabras ["de", "todos"])
          <|> DelSiguiente <$ palabras ["del", "siguiente"]

-- | si COND entonces efectos [sino efectos] fin
siP :: Parser Efecto
siP = do
  palabra "si"
  c <- cond
  palabra "entonces"
  entonces <- many efecto
  sino <- option [] (palabra "sino" *> many efecto)
  palabra "fin"
  return (Si c entonces sino)

-- | mayor|menor en EXPR: efectos [empate: efectos] fin
destacadoP :: Parser Efecto
destacadoP = do
  crit <- criterioP
  palabra "en"
  e <- expr
  dosPuntos
  efs <- many efecto
  emp <- option [] (palabra "empate" *> dosPuntos *> many efecto)
  palabra "fin"
  return (Destacado crit e efs emp)

-- ---------------------------------------------------------------------
-- Expresiones
-- ---------------------------------------------------------------------

expr :: Parser Expr
expr = buildExpressionParser tablaArit factor <?> "una expresión"

tablaArit :: OperatorTable String () Identity Expr
tablaArit =
  [ [Prefix (Arit Menos (Num 0) <$ operador "-")]
  , [binario "*" Por, binario "/" Dividido]
  , [binario "+" Mas, binario "-" Menos]
  ]
  where binario s op = Infix (Arit op <$ operador s) AssocLeft

factor :: Parser Expr
factor = choice
  [ Num <$> numero
  , paloLit
  , Ninguno <$ palabra "ninguno"
  , Rango <$> (palabra "rango" *> parentesis refCarta)
  , Palo <$> (palabra "palo" *> parentesis refCarta)
  , Valor <$> (palabra "valor" *> parentesis refCarta)
  , Suma <$> (palabra "suma" *> parentesis zonaRef)
  , Cantidad <$> (palabra "cantidad" *> parentesis zonaRef)
  , siExpr
  , parentesis expr
  , Var <$> ident
  ]
  where
    siExpr = do
      palabra "si"
      c <- cond
      palabra "entonces"
      a <- expr
      palabra "sino"
      b <- expr
      return (SiExpr c a b)

-- | carta  |  tope(ZONA)
refCarta :: Parser RefCarta
refCarta = LaCarta <$ palabra "carta"
       <|> Tope <$> (palabra "tope" *> parentesis zonaRef)
       <?> "una carta (carta o tope(zona))"

-- ---------------------------------------------------------------------
-- Condiciones
-- ---------------------------------------------------------------------

cond :: Parser Cond
cond = buildExpressionParser tablaLogica condAtomo <?> "una condición"

tablaLogica :: OperatorTable String () Identity Cond
tablaLogica =
  [ [Prefix (No <$ palabra "no")]
  , [Infix (Y <$ palabra "y") AssocLeft]
  , [Infix (O <$ palabra "o") AssocLeft]
  ]

condAtomo :: Parser Cond
condAtomo = choice
  [ Alguno <$> (palabra "alguno" *> parentesis cond)
  , Todos <$> (palabra "todos" *> parentesis cond)
  , Puede <$> (palabra "puede" *> parentesis ident)
  , try comparacion
  , parentesis cond
  ]

comparacion :: Parser Cond
comparacion = do
  a <- expr
  op <- opRel
  b <- expr
  return (Comparar op a b)

opRel :: Parser OpRel
opRel = choice
  [ MenorIgual <$ operador "<="
  , MayorIgual <$ operador ">="
  , Distinto <$ operador "!="
  , Igual <$ operador "="
  , MenorQue <$ operador "<"
  , MayorQue <$ operador ">"
  ] <?> "un operador de comparación"
