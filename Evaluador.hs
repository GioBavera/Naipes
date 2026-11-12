-- | Semántica del lenguaje: validación de las reglas y simulación de una
-- partida. Todo el módulo es puro: la única fuente de variación es la
-- semilla, así que la misma semilla produce siempre la misma partida.
module Evaluador (validar, simularJuego) where

import Control.Monad (forM, forM_, unless, when)
import Control.Monad.State (StateT, evalStateT, get, gets, modify)
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.List (delete, elemIndex, find, intercalate, nub, (\\))
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)

import AST

-- =====================================================================
-- Validación estática
-- =====================================================================

-- | Qué se puede usar en el punto del programa que se está revisando.
data Amb = Amb
  { aJugador :: Bool      -- ^ hay un jugador actual
  , aCarta   :: Bool      -- ^ hay una carta elegida
  , aTurno   :: Bool      -- ^ se está dentro de una jugada
  , aValor   :: Bool      -- ^ se está dentro de "valor de carta"
  , aPrevias :: [String]  -- ^ jugadas a las que puede referirse puede(...)
  , aLugar   :: String    -- ^ descripción para los mensajes
  }

data Tipo = TNum | TPalo
  deriving Eq

-- | Revisa las reglas antes de simular. Devuelve la lista de errores; si
-- está vacía, el juego se puede ejecutar.
validar :: Juego -> [String]
validar j = concat
  [ [ "Se necesitan al menos dos jugadores." | length nombresJ < 2 ]
  , repetidos "jugador" nombresJ
  , repetidos "zona o variable" (todasLasZonas ++ todasLasVars)
  , [ "La zona \"mazo\" ya existe: no hay que declararla."
    | "mazo" `elem` zonasComunes j ++ zonasJugador j ]
  , [ "La baraja no tiene el rango " ++ show r ++ "."
    | r <- sinRangos j, r `notElem` rangosDe (baraja j) ]
  , repetidos "estrategia" (map eNombre (estrategias j))
  , [ "\"azar\" es una estrategia predefinida: no se puede redefinir."
    | "azar" `elem` map eNombre (estrategias j) ]
  , [ "El jugador " ++ n ++ " usa la estrategia \"" ++ e ++ "\", que no existe."
    | (n, e) <- jugadores j, e /= "azar", e `notElem` map eNombre (estrategias j) ]
  , repetidos "jugada" nombresJugadas
  , [ "El límite de turnos debe ser mayor que cero." | limite j <= 0 ]
  , maybe [] (\e -> vNum ambValor e) (valorCarta j)
  , concatMap vEstrategia (estrategias j)
  , vEfs (amb "En la preparación") (preparacion j)
  , concat (zipWith vJugada [0 ..] (jugadas j))
  , vEfs (amb "En el cierre de ronda") (cierreRonda j)
  , maybe [] (vCond (amb "En \"termina cuando\"")) (termina j)
  , vNum (amb "En \"gana\"") { aJugador = True } (snd (ganador j))
  ]
  where
    nombresJ = map fst (jugadores j)
    nombresJugadas = map jNombre (jugadas j)
    todasLasZonas = zonasComunes j ++ zonasJugador j
    todasLasVars = map fst (varsComunes j ++ varsJugador j)
    esVarComun x = x `elem` map fst (varsComunes j)
    esVarPropia x = x `elem` map fst (varsJugador j)
    esComun z = z == "mazo" || z `elem` zonasComunes j
    esPropia z = z `elem` zonasJugador j

    amb lugar = Amb False False False False nombresJugadas lugar
    ambValor = (amb "En \"valor de carta\"") { aCarta = True, aValor = True }

    err a msg = [aLugar a ++ ": " ++ msg]

    repetidos que xs =
      [ "Hay más de un elemento de tipo " ++ que ++ " con el nombre \"" ++ x ++ "\"."
      | x <- nub (xs \\ nub xs) ]

    -- Estrategias ---------------------------------------------------

    vEstrategia est = concatMap vRegla (eReglas est)
      where
        a = (amb ("En la estrategia \"" ++ eNombre est ++ "\""))
              { aJugador = True, aTurno = True }
        vRegla rg = case find ((== rJugada rg) . jNombre) (jugadas j) of
          Nothing -> err a ("la jugada \"" ++ rJugada rg ++ "\" no existe.")
          Just jg -> concat
            [ maybe [] (vCond a) (rCuando rg)
            , case rPreferencia rg of
                Nothing -> []
                Just (_, e)
                  | isNothing (jCarta jg) ->
                      err a ("la jugada \"" ++ rJugada rg
                             ++ "\" no elige carta: no admite \"con mayor/menor\".")
                  | otherwise -> vNum a { aCarta = True } e
            ]

    -- Jugadas -------------------------------------------------------

    vJugada i jg = concat
      [ case jCarta jg of
          Nothing -> []
          Just (z, filtro) ->
            vZona a (RefZona z Directa)
              ++ maybe [] (vCond a { aCarta = True }) filtro
      , maybe [] (vCond a) (jCuando jg)
      , vEfs a { aCarta = isJust (jCarta jg) } (jEfectos jg)
      ]
      where
        a = Amb True False True False (take i nombresJugadas)
              ("En la jugada \"" ++ jNombre jg ++ "\"")

    -- Efectos -------------------------------------------------------

    vEfs a = concatMap (vEf a)

    vEf a ef = case ef of
      Mezclar z -> vZona a z ++ unaSola a z
      Mover n _ o _ d -> vCant a n ++ vZona a o ++ vZona a d ++ unaSola a d
      MoverCarta _ d ->
        [ m | not (aCarta a), m <- err a "\"mover carta\" necesita una jugada \"con carta de ...\"." ]
          ++ vZona a d ++ unaSola a d
      Repartir n _ o z ->
        vCant a n ++ vZona a o ++ unaSola a o
          ++ [ m | not (esPropia z)
                 , m <- err a ("se reparte a una zona de jugador, y \"" ++ z ++ "\" no lo es.") ]
      Si c xs ys -> vCond a c ++ vEfs a xs ++ vEfs a ys
      Plantar -> necesitaJugador a "plantar"
      Eliminar -> necesitaJugador a "eliminar"
      RepetirTurno -> necesitaTurno a "repetir turno"
      SaltearSiguiente -> necesitaTurno a "saltear siguiente"
      InvertirSentido -> []
      Mostrar _ me -> maybe [] (vExpr a) me
      Asignar x e -> vExpr a e ++ vExpr a (Var x)
        ++ [ m | x `elem` todasLasVars, tipoDe e /= tipoDe (Var x)
               , m <- err a ("a la variable \"" ++ x ++ "\" se le asigna un valor de otro tipo.") ]
      CadaJugador xs -> vEfs a { aJugador = True } xs
      Destacado _ e xs ys ->
        vNum a { aJugador = True } e ++ vEfs a { aJugador = True } xs ++ vEfs a ys

    vCant _ Todo = []
    vCant a (Cant e) = vNum a e

    unaSola a (RefZona z DeTodos) =
      err a ("acá va una sola zona, no \"" ++ z ++ " de todos\".")
    unaSola _ _ = []

    necesitaJugador a que =
      [ m | not (aJugador a), m <- err a ("\"" ++ que ++ "\" necesita un jugador actual.") ]
    necesitaTurno a que =
      [ m | not (aTurno a), m <- err a ("\"" ++ que ++ "\" solo se puede usar dentro de una jugada.") ]

    -- Zonas ---------------------------------------------------------

    vZona a (RefZona z alc)
      | aValor a = err a "no se pueden usar zonas."
      | esComun z =
          [ m | alc /= Directa
              , m <- err a ("\"" ++ z ++ "\" es una zona común: no lleva \"de todos\" ni \"del siguiente\".") ]
      | esPropia z = case alc of
          Directa | not (aJugador a) ->
            err a ("\"" ++ z ++ "\" es una zona de jugador y acá no hay jugador actual "
                   ++ "(usar \"de todos\", \"cada jugador\", \"alguno\" o \"todos\").")
          DelSiguiente | not (aTurno a) ->
            err a "\"del siguiente\" solo se puede usar dentro de una jugada."
          _ -> []
      | otherwise = err a ("la zona \"" ++ z ++ "\" no está declarada.")

    -- Expresiones ---------------------------------------------------

    tipoDe e = case e of
      PaloLit _ -> TPalo
      Ninguno -> TPalo
      Palo _ -> TPalo
      Var x -> maybe TNum tipoDe (lookup x (varsComunes j ++ varsJugador j))
      SiExpr _ x _ -> tipoDe x
      _ -> TNum

    -- Una expresión que debe ser numérica.
    vNum a e = vExpr a e
      ++ [ m | tipoDe e /= TNum, m <- err a "se esperaba un número y hay un palo." ]

    vExpr a e = case e of
      Num _ -> []
      PaloLit p ->
        [ m | p `notElem` palosDe (baraja j)
            , m <- err a ("el palo \"" ++ p ++ "\" no pertenece a la baraja del juego.") ]
      Ninguno -> []
      Var x
        | esVarComun x -> []
        | esVarPropia x ->
            [ m | not (aJugador a)
                , m <- err a ("\"" ++ x ++ "\" es una variable de jugador y acá no hay jugador actual.") ]
        | otherwise -> err a ("la variable \"" ++ x ++ "\" no está declarada.")
      Rango r -> vRefCarta a r
      Palo r -> vRefCarta a r
      Valor r -> soloFueraDeValor a "valor(...)" ++ vRefCarta a r
      Suma z -> soloFueraDeValor a "suma(...)" ++ vZona a z
      Cantidad z -> vZona a z
      Arit _ x y -> vNum a x ++ vNum a y
      SiExpr c x y -> vCond a c ++ vExpr a x ++ vExpr a y
        ++ [ m | tipoDe x /= tipoDe y
               , m <- err a "las dos ramas de \"si ... entonces ... sino\" deben ser del mismo tipo." ]

    soloFueraDeValor a que =
      [ m | aValor a, m <- err a ("no se puede usar " ++ que ++ ".") ]

    vRefCarta a LaCarta =
      [ m | not (aCarta a), m <- err a "se usa \"carta\" pero acá no hay una carta elegida." ]
    vRefCarta a (Tope z) = vZona a z

    -- Condiciones ---------------------------------------------------

    vCond a c = case c of
      Comparar op x y -> vExpr a x ++ vExpr a y
        ++ [ m | tipoDe x /= tipoDe y
               , m <- err a "se compara un número con un palo." ]
        ++ [ m | tipoDe x == TPalo, op `notElem` [Igual, Distinto]
               , m <- err a "los palos solo se comparan con = y !=." ]
      Y x y -> vCond a x ++ vCond a y
      O x y -> vCond a x ++ vCond a y
      No x -> vCond a x
      Alguno x -> vCond a { aJugador = True } x
      Todos x -> vCond a { aJugador = True } x
      Puede n
        | n `notElem` nombresJugadas -> err a ("puede(" ++ n ++ "): esa jugada no existe.")
        | n `notElem` aPrevias a ->
            err a ("puede(" ++ n ++ ") solo puede referirse a una jugada declarada antes.")
        | not (aJugador a) -> err a ("puede(" ++ n ++ ") necesita un jugador actual.")
        | otherwise -> []

-- =====================================================================
-- Estado de la partida
-- =====================================================================

data Situacion = Activo | Plantado | Eliminado
  deriving Eq

-- | Un lugar concreto donde hay cartas.
data Lugar
  = Comun String        -- ^ zona común
  | De String String    -- ^ zona de un jugador: (jugador, zona)
  deriving (Eq, Ord)

data Estado = Estado
  { zonas       :: M.Map Lugar [Carta]      -- ^ el primer elemento es la carta de arriba
  , situaciones :: M.Map String Situacion
  , variables   :: M.Map Lugar Val            -- ^ comunes y de cada jugador
  , semilla     :: Int
  , sentido     :: Int                      -- ^ 1 o -1
  , saltos      :: Int                      -- ^ jugadores que pierden el turno
  , repite      :: Bool                     -- ^ el jugador actual vuelve a jugar
  }

-- | La simulación: un estado que cambia y una traza que se va escribiendo.
type Sim = StateT Estado (Writer [String])

-- | Contexto en el que se evalúa una expresión o se ejecuta un efecto.
data Ctx = Ctx
  { cJuego   :: Juego
  , cJugador :: Maybe String   -- ^ jugador actual
  , cCarta   :: Maybe Carta    -- ^ carta elegida
  , cOrigen  :: Maybe Lugar    -- ^ de dónde salió la carta elegida
  }

data Val = VNum Double | VPalo String
  deriving Eq

estadoInicial :: Juego -> Int -> Estado
estadoInicial j s = Estado
  { zonas = M.singleton (Comun "mazo") mazoInicial
  , situaciones = M.fromList [(n, Activo) | (n, _) <- jugadores j]
  , variables = M.fromList $
      [ (Comun x, inicial v) | (x, v) <- varsComunes j ]
      ++ [ (De n x, inicial v) | (n, _) <- jugadores j, (x, v) <- varsJugador j ]
  , semilla = abs s `mod` modulo
  , sentido = 1
  , saltos = 0
  , repite = False
  }
  where
    inicial (Num n) = VNum n
    inicial (PaloLit p) = VPalo p
    inicial _ = VPalo ""
    mazoInicial =
      [ Carta r p | p <- palosDe (baraja j), r <- rangosDe (baraja j)
                  , r `notElem` sinRangos j ]

-- ---------------------------------------------------------------------
-- Consultas sobre el estado
-- ---------------------------------------------------------------------

nombres :: Juego -> [String]
nombres = map fst . jugadores

situacion :: Estado -> String -> Situacion
situacion e n = M.findWithDefault Activo n (situaciones e)

-- | Jugadores que siguen jugando.
activos :: Juego -> Estado -> [String]
activos j e = [n | n <- nombres j, situacion e n == Activo]

-- | Jugadores que no fueron eliminados (pueden ganar).
enJuego :: Juego -> Estado -> [String]
enJuego j e = [n | n <- nombres j, situacion e n /= Eliminado]

-- | El próximo jugador activo según el sentido de juego. Si no hay otro,
-- es el mismo jugador.
siguienteDe :: Juego -> Estado -> String -> String
siguienteDe j e n = head ([m | m <- orden, situacion e m == Activo] ++ [n])
  where
    ns = nombres j
    k = length ns
    i = fromMaybe 0 (elemIndex n ns)
    orden = [ns !! ((i + sentido e * d) `mod` k) | d <- [1 .. k]]

actualDe :: Ctx -> String
actualDe c = fromMaybe (error "error interno: no hay jugador actual") (cJugador c)

-- | Lugares concretos a los que se refiere una zona.
lugares :: Ctx -> Estado -> RefZona -> [Lugar]
lugares c e (RefZona z alc)
  | z == "mazo" || z `elem` zonasComunes j = [Comun z]
  | otherwise = case alc of
      Directa      -> [De (actualDe c) z]
      DeTodos      -> [De n z | n <- nombres j]
      DelSiguiente -> [De (siguienteDe j e (actualDe c)) z]
  where j = cJuego c

-- | Dónde se guarda una variable: es común, o es del jugador actual.
lugarVar :: Ctx -> String -> Lugar
lugarVar c x
  | x `elem` map fst (varsComunes (cJuego c)) = Comun x
  | otherwise = De (actualDe c) x

cartasEn :: Estado -> Lugar -> [Carta]
cartasEn e l = M.findWithDefault [] l (zonas e)

cartasDe :: Ctx -> Estado -> RefZona -> [Carta]
cartasDe c e z = concatMap (cartasEn e) (lugares c e z)

-- ---------------------------------------------------------------------
-- Evaluación de expresiones y condiciones
-- ---------------------------------------------------------------------

evalExpr :: Ctx -> Estado -> Expr -> Val
evalExpr c e ex = case ex of
  Num n        -> VNum n
  PaloLit p    -> VPalo p
  Ninguno      -> VPalo ""
  Var x        -> M.findWithDefault (VNum 0) (lugarVar c x) (variables e)
  -- El tope de una zona vacía tiene rango y valor 0, y ningún palo.
  Rango r      -> VNum (maybe 0 (fromIntegral . rangoDe) (carta r))
  Palo r       -> VPalo (maybe "" paloDe (carta r))
  Valor r      -> VNum (maybe 0 (valorDe c e) (carta r))
  Suma z       -> VNum (sum (map (valorDe c e) (cartasDe c e z)))
  Cantidad z   -> VNum (fromIntegral (length (cartasDe c e z)))
  Arit op a b  -> VNum (aplicar op (evalNum c e a) (evalNum c e b))
  SiExpr k a b -> if evalCond c e k then evalExpr c e a else evalExpr c e b
  where
    carta LaCarta = cCarta c
    carta (Tope z) = listToMaybe (cartasDe c e z)
    aplicar Mas = (+)
    aplicar Menos = (-)
    aplicar Por = (*)
    aplicar Dividido = (/)

evalNum :: Ctx -> Estado -> Expr -> Double
evalNum c e ex = case evalExpr c e ex of
  VNum n -> n
  VPalo _ -> 0  -- la validación impide llegar acá

-- | Valor de una carta según la sección "valor de carta".
valorDe :: Ctx -> Estado -> Carta -> Double
valorDe c e k = case valorCarta (cJuego c) of
  Nothing -> fromIntegral (rangoDe k)
  Just ex -> evalNum c { cCarta = Just k } e ex

evalCond :: Ctx -> Estado -> Cond -> Bool
evalCond c e k = case k of
  Comparar op a b -> comparar op (evalExpr c e a) (evalExpr c e b)
  Y a b    -> evalCond c e a && evalCond c e b
  O a b    -> evalCond c e a || evalCond c e b
  No a     -> not (evalCond c e a)
  Alguno a -> any (\n -> evalCond c { cJugador = Just n } e a) (enJuego j e)
  Todos a  -> all (\n -> evalCond c { cJugador = Just n } e a) (enJuego j e)
  Puede n  -> any (not . null . instancias c e) [jg | jg <- jugadas j, jNombre jg == n]
  where
    j = cJuego c
    comparar Igual x y = x == y
    comparar Distinto x y = x /= y
    comparar op (VNum x) (VNum y) = case op of
      MenorQue   -> x < y
      MenorIgual -> x <= y
      MayorQue   -> x > y
      _          -> x >= y
    comparar _ _ _ = False

-- | Formas concretas en que el jugador actual puede hacer una jugada:
-- una por cada carta elegible, o una sola si la jugada no elige carta.
-- La lista vacía significa que la jugada no es legal.
instancias :: Ctx -> Estado -> Jugada -> [(Maybe Carta, Maybe Lugar)]
instancias c e jg
  | not (maybe True (evalCond c e) (jCuando jg)) = []
  | otherwise = case jCarta jg of
      Nothing -> [(Nothing, Nothing)]
      Just (z, filtro) ->
        [ (Just k, Just l)
        | l <- lugares c e (RefZona z Directa)
        , k <- nub (cartasEn e l)
        , maybe True (evalCond c { cCarta = Just k } e) filtro ]

-- ---------------------------------------------------------------------
-- Azar
-- ---------------------------------------------------------------------

modulo :: Int
modulo = 2147483648

-- | Número al azar entre 0 y n-1 (generador congruencial lineal).
azar :: Int -> Sim Int
azar n = do
  s <- gets semilla
  let s' = (s * 1103515245 + 12345) `mod` modulo
  modify (\e -> e { semilla = s' })
  return ((s' `div` 65536) `mod` max 1 n)

unoAlAzar :: [a] -> Sim a
unoAlAzar xs = (xs !!) <$> azar (length xs)

mezclarLista :: [a] -> Sim [a]
mezclarLista [] = return []
mezclarLista xs = do
  i <- azar (length xs)
  case splitAt i xs of
    (antes, x : despues) -> (x :) <$> mezclarLista (antes ++ despues)
    _ -> return xs

-- ---------------------------------------------------------------------
-- Traza
-- ---------------------------------------------------------------------

anotar :: String -> Sim ()
anotar s = tell [s]

-- | Línea de detalle, debajo de una jugada.
detalle :: String -> Sim ()
detalle s = anotar ("      " ++ s)

mostrarNum :: Double -> String
mostrarNum d
  | isInfinite d || isNaN d = show d
  | d == fromIntegral r = show r
  | otherwise = show d
  where r = round d :: Integer

nombreCarta :: Carta -> String
nombreCarta (Carta r p) = figura ++ " de " ++ p
  where
    francesa = p `elem` palosDe Francesa
    figura = case r of
      1  | francesa -> "A"
      11 | francesa -> "J"
      12 | francesa -> "Q"
      13 | francesa -> "K"
      _  -> show r

-- | Lista de cartas para la traza; si son muchas, solo la cantidad.
listaCartas :: [Carta] -> String
listaCartas ks
  | length ks > 8 = show (length ks) ++ " cartas"
  | otherwise = intercalate ", " (map nombreCarta ks)

nombreLugar :: Lugar -> String
nombreLugar (Comun z) = z
nombreLugar (De n z) = z ++ " de " ++ n

-- ---------------------------------------------------------------------
-- Ejecución de efectos
-- ---------------------------------------------------------------------

-- | Saca hasta n cartas de un lugar, de arriba o del fondo.
sacar :: Int -> Extremo -> Lugar -> Sim [Carta]
sacar n ext l = do
  ks <- gets (`cartasEn` l)
  let m = max 0 (min n (length ks))
      (tomadas, resto) = case ext of
        Arriba -> splitAt m ks
        Abajo  -> let (r, t) = splitAt (length ks - m) ks in (reverse t, r)
  modify (\e -> e { zonas = M.insert l resto (zonas e) })
  return tomadas

-- | Pone cartas en un lugar, de a una: arriba (la última queda encima) o
-- en el fondo (la última queda más abajo).
poner :: Extremo -> Lugar -> [Carta] -> Sim ()
poner ext l ks = modify $ \e ->
  let viejas = cartasEn e l
      nuevas = case ext of
        Arriba -> reverse ks ++ viejas
        Abajo  -> viejas ++ ks
  in e { zonas = M.insert l nuevas (zonas e) }

ejecutarTodos :: Ctx -> [Efecto] -> Sim ()
ejecutarTodos c = mapM_ (ejecutar c)

ejecutar :: Ctx -> Efecto -> Sim ()
ejecutar c ef = case ef of

  Mezclar z -> do
    e <- get
    forM_ (lugares c e z) $ \l -> do
      ks <- mezclarLista (cartasEn e l)
      modify (\s -> s { zonas = M.insert l ks (zonas s) })
      detalle ("se mezcla " ++ nombreLugar l)

  Mover cant extO o extD d -> do
    e <- get
    let n = case cant of
          Todo    -> maxBound
          Cant ex -> floor (evalNum c e ex)
    forM_ (take 1 (lugares c e d)) $ \destino ->
      forM_ (lugares c e o) $ \origen ->
        when (origen /= destino) $ do
          ks <- sacar n extO origen
          poner extD destino ks
          unless (null ks) $
            detalle (nombreLugar origen ++ " -> " ++ nombreLugar destino
                     ++ ": " ++ listaCartas ks)

  MoverCarta ext d -> do
    e <- get
    case (cCarta c, cOrigen c, lugares c e d) of
      (Just k, Just origen, destino : _)
        | k `elem` cartasEn e origen -> do
            modify (\s -> s { zonas = M.adjust (delete k) origen (zonas s) })
            poner ext destino [k]
            detalle (nombreLugar origen ++ " -> " ++ nombreLugar destino
                     ++ ": " ++ nombreCarta k)
      _ -> return ()

  Repartir cant ext o z -> do
    e <- get
    let ns = nombres j
        vueltas = case cant of
          Todo    -> maxBound `div` max 1 (length ns)
          Cant ex -> max 0 (floor (evalNum c e ex))
    forM_ (take 1 (lugares c e o)) $ \origen -> do
      let total = min (length (cartasEn e origen)) (vueltas * length ns)
      recibidas <- forM (take total (cycle ns)) $ \n -> do
        ks <- sacar 1 ext origen
        poner Arriba (De n z) ks
        return (n, ks)
      forM_ ns $ \n ->
        detalle (n ++ " recibe: "
                 ++ listaCartas (concat [ks | (m, ks) <- recibidas, m == n]))

  Si k entonces sino -> do
    e <- get
    ejecutarTodos c (if evalCond c e k then entonces else sino)

  Plantar -> do
    cambiarSituacion Plantado
    detalle (actualDe c ++ " se planta")

  Eliminar -> do
    cambiarSituacion Eliminado
    detalle (actualDe c ++ " queda fuera del juego")

  RepetirTurno -> modify (\s -> s { repite = True })

  SaltearSiguiente -> modify (\s -> s { saltos = saltos s + 1 })

  InvertirSentido -> do
    modify (\s -> s { sentido = negate (sentido s) })
    detalle "se invierte el sentido de juego"

  Mostrar texto mex -> do
    e <- get
    let quien = maybe "" (++ ": ") (cJugador c)
        dato = maybe "" ((" " ++) . mostrarVal . evalExpr c e) mex
    detalle (quien ++ texto ++ dato)

  Asignar x ex -> do
    e <- get
    let v = evalExpr c e ex
        l = lugarVar c x
    modify (\s -> s { variables = M.insert l v (variables s) })
    detalle (nombreLugar l ++ " = " ++ mostrarVal v)

  CadaJugador efs -> do
    e <- get
    forM_ (enJuego j e) $ \n -> ejecutarTodos c { cJugador = Just n } efs

  Destacado crit ex efs empate -> do
    e <- get
    let puntajes = [(n, evalNum c { cJugador = Just n } e ex) | n <- enJuego j e]
    case destacados crit puntajes of
      [n] -> ejecutarTodos c { cJugador = Just n } efs
      _   -> ejecutarTodos c empate
  where
    j = cJuego c
    cambiarSituacion s =
      modify (\e -> e { situaciones = M.insert (actualDe c) s (situaciones e) })
    mostrarVal (VNum n) = mostrarNum n
    mostrarVal (VPalo "") = "ninguno"
    mostrarVal (VPalo p) = p

-- | Los elementos con el mayor (o menor) puntaje.
destacados :: Criterio -> [(a, Double)] -> [a]
destacados _ [] = []
destacados crit ps = [x | (x, v) <- ps, v == mejor]
  where
    mejor = (if crit == Mayor then maximum else minimum) (map snd ps)

-- ---------------------------------------------------------------------
-- Elección de jugada según la estrategia
-- ---------------------------------------------------------------------

-- | Una jugada concreta: la jugada, la carta elegida y de dónde sale.
type Opcion = (Jugada, Maybe Carta, Maybe Lugar)

-- | Elige entre las jugadas legales. Se usa la primera regla de la
-- estrategia que se pueda aplicar; si ninguna se aplica (o la estrategia
-- es "azar"), se elige al azar.
elegir :: Ctx -> [Opcion] -> Sim Opcion
elegir c legales = do
  e <- get
  let j = cJuego c
      reglas = fromMaybe [] $ do
        nomEst <- lookup (actualDe c) (jugadores j)
        eReglas <$> find ((== nomEst) . eNombre) (estrategias j)
      candidatas rg = [o | o@(jg, _, _) <- legales, jNombre jg == rJugada rg]
      aplicable rg =
        maybe True (evalCond c e) (rCuando rg) && not (null (candidatas rg))
  case find aplicable reglas of
    Nothing -> unoAlAzar legales
    Just rg -> case rPreferencia rg of
      Nothing -> unoAlAzar (candidatas rg)
      Just (crit, ex) ->
        unoAlAzar (destacados crit
          [ (o, evalNum c { cCarta = k } e ex) | o@(_, k, _) <- candidatas rg ])

-- ---------------------------------------------------------------------
-- La partida
-- ---------------------------------------------------------------------

-- | Simula una partida completa y devuelve la traza, línea por línea.
simularJuego :: Juego -> Int -> [String]
simularJuego j s = execWriter (evalStateT (partida j s) (estadoInicial j s))

-- | Datos de la ronda en curso.
data Ronda = Ronda
  { numero      :: Int
  , empezada    :: Bool       -- ^ ya se escribió el encabezado
  , yaJugaron   :: [String]   -- ^ jugadores que tuvieron su turno
  , huboJugadas :: Bool
  , turnos      :: Int        -- ^ total de jugadas hechas en la partida
  }

-- | Por qué terminó la partida.
data Motivo = CondicionDeFin | SinActivos | Bloqueo | Limite

partida :: Juego -> Int -> Sim ()
partida j s = do
  anotar ("== " ++ nombre j ++ " ==")
  anotar ("Semilla: " ++ show s)
  anotar ("Jugadores: "
          ++ intercalate ", " [n ++ " (" ++ e ++ ")" | (n, e) <- jugadores j])
  anotar ""
  anotar "Preparación"
  ejecutarTodos sinJugador (preparacion j)
  motivo <- bucle j (head (nombres j)) (Ronda 1 False [] False 0)
  informe j motivo
  where sinJugador = Ctx j Nothing Nothing Nothing

-- | Un turno del jugador p, y después el resto de la partida.
bucle :: Juego -> String -> Ronda -> Sim Motivo
bucle j p r0
  | turnos r0 >= limite j = return Limite
  | otherwise = do
      unless (empezada r0) $ do
        anotar ""
        anotar ("Ronda " ++ show (numero r0))
      e <- get
      let c = Ctx j (Just p) Nothing Nothing
          legales = [(jg, k, l) | jg <- jugadas j, (k, l) <- instancias c e jg]
      jugo <-
        if null legales
          then False <$ anotar ("  " ++ p ++ ": no puede jugar, pasa")
          else do
            (jg, k, l) <- elegir c legales
            anotar ("  " ++ p ++ ": " ++ jNombre jg
                    ++ maybe "" ((" " ++) . nombreCarta) k)
            modify (\s -> s { repite = False })
            ejecutarTodos c { cCarta = k, cOrigen = l } (jEfectos jg)
            return True
      let r1 = r0 { empezada = True
                  , huboJugadas = huboJugadas r0 || jugo
                  , turnos = turnos r0 + (if jugo then 1 else 0) }
      e1 <- get
      case () of
        _ | seCumpleFin e1 -> return CondicionDeFin
          | null (activos j e1) -> return SinActivos
          | jugo && repite e1 && situacion e1 p == Activo -> bucle j p r1
          | otherwise -> pasarTurno r1
  where
    seCumpleFin e = maybe False (evalCond (Ctx j Nothing Nothing Nothing) e) (termina j)

    -- Determina quién sigue, aplica los turnos perdidos y cierra la
    -- ronda si ya jugaron todos.
    pasarTurno r1 = do
      e <- get
      let n = saltos e
          salteados = take n (iterate (siguienteDe j e) (siguienteDe j e p))
          proximo = iterate (siguienteDe j e) (siguienteDe j e p) !! n
          r2 = r1 { yaJugaron = p : salteados ++ yaJugaron r1 }
      forM_ salteados $ \q -> anotar ("  " ++ q ++ " pierde el turno")
      modify (\s -> s { saltos = 0 })
      if all (`elem` yaJugaron r2) (activos j e)
        then cerrarRonda proximo r2
        else bucle j proximo r2

    cerrarRonda proximo r
      | not (huboJugadas r) = return Bloqueo
      | otherwise = do
          unless (null (cierreRonda j)) $ do
            anotar "  Cierre de ronda"
            ejecutarTodos (Ctx j Nothing Nothing Nothing) (cierreRonda j)
          e <- get
          let q = if situacion e proximo == Activo
                    then proximo else siguienteDe j e proximo
          case () of
            _ | seCumpleFin e -> return CondicionDeFin
              | null (activos j e) -> return SinActivos
              | otherwise ->
                  bucle j q (Ronda (numero r + 1) False [] False (turnos r))

-- | Escribe el final de la traza: motivo, puntajes y ganador.
informe :: Juego -> Motivo -> Sim ()
informe j motivo = do
  e <- get
  let (crit, ex) = ganador j
      puntaje n = evalNum (Ctx j (Just n) Nothing Nothing) e ex
      ancho = maximum (map length (nombres j))
      fila n = "  " ++ n ++ replicate (ancho - length n + 2) ' '
               ++ mostrarNum (puntaje n)
               ++ (if situacion e n == Eliminado then "  (fuera del juego)" else "")
  anotar ""
  anotar ("Fin de la partida: " ++ case motivo of
    CondicionDeFin -> "se cumplió la condición de fin."
    SinActivos     -> "no quedan jugadores por jugar."
    Bloqueo        -> "nadie puede jugar."
    Limite         -> "se alcanzó el límite de " ++ show (limite j) ++ " turnos.")
  anotar ""
  anotar ("Resultado (gana el " ++ (if crit == Mayor then "mayor" else "menor") ++ " puntaje)")
  mapM_ (anotar . fila) (nombres j)
  anotar ""
  anotar $ case (motivo, destacados crit [(n, puntaje n) | n <- enJuego j e]) of
    (Limite, _) -> "Empate: la partida se cortó sin definirse."
    (_, [])     -> "No gana nadie: todos quedaron fuera del juego."
    (_, [n])    -> "Gana " ++ n
    (_, ns)     -> "Empate entre " ++ intercalate " y " ns
