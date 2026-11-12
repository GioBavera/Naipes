-- | Punto de entrada de Naipes: lee un archivo de reglas, lo analiza, lo valida y
-- muestra la traza de una partida simulada.
--
-- Uso desde GHCi:
--
-- > ghci Main.hs
-- > simular "juegos/siete_y_medio.naipes"
-- > simularCon 42 "juegos/siete_y_medio.naipes"
--
-- Uso compilado:
--
-- > ghc -O Main.hs -o naipes
-- > ./naipes juegos/siete_y_medio.naipes 42
module Main where

import Control.Exception (IOException, evaluate, try)
import Data.Char (isDigit)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Environment (getArgs)
import System.IO

import Evaluador (simularJuego, validar)
import Parser (parsearJuego)

-- | Versión de Naipes.
version :: String
version = "1.0"

-- | Simula una partida con una semilla tomada del reloj.
simular :: FilePath -> IO ()
simular archivo = do
  t <- getPOSIXTime
  simularCon (floor (t * 1000) `mod` 1000000) archivo

-- | Simula una partida con una semilla dada: la misma semilla produce
-- siempre la misma partida.
simularCon :: Int -> FilePath -> IO ()
simularCon semilla archivo = do
  hSetEncoding stdout utf8
  leido <- try (leer archivo) :: IO (Either IOException String)
  case leido of
    Left _ -> putStrLn ("No se pudo leer el archivo \"" ++ archivo ++ "\".")
    Right texto -> case parsearJuego archivo texto of
      Left errorSintaxis -> putStrLn errorSintaxis
      Right juego -> case validar juego of
        [] -> mapM_ putStrLn (simularJuego juego semilla)
        errores -> do
          putStrLn "Errores en las reglas:"
          mapM_ (putStrLn . ("  - " ++)) errores

-- | Lee un archivo completo como UTF-8.
leer :: FilePath -> IO String
leer archivo = withFile archivo ReadMode $ \h -> do
  hSetEncoding h utf8
  texto <- hGetContents h
  _ <- evaluate (length texto)
  return texto

main :: IO ()
main = do
  args <- getArgs
  case args of
    [archivo] -> simular archivo
    [archivo, s] | not (null s), all isDigit s -> simularCon (read s) archivo
    _ -> do
      putStrLn ("Naipes V" ++ version)
      putStrLn "Uso: naipes ARCHIVO [SEMILLA]"
