{-# LANGUAGE RecordWildCards #-}
module Lecture08 where

import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Exception
-----------------------------------------------------------------------------
-- MONADA IO
-----------------------------------------------------------------------------

{-
Monada IO pozwala na modelowanie obliczeń z efektami ubocznymi (wejście/wyjście)
w sposób zgodny z paradygmatem czystego programowania funkcyjnego.

Koncepcyjnie można rozumieć IO jako:
newtype IO a = IO { runIO :: World -> (World, a) }

f :: a -> IO b

a   ───┐          ┌─── b
       │          │
       └──→[  f ]─┘
       │          │
World ─┘          └─── World
-}

-- Podstawowe operacje IO:
-- getLine :: IO String -- Odczytuje linię ze standardowego wejścia
-- putStrLn :: String -> IO () -- Wypisuje tekst na standardowe wyjście
-- readFile :: FilePath -> IO String -- Odczytuje zawartość pliku
-- writeFile :: FilePath -> String -> IO () -- Zapisuje tekst do pliku

-- Przykład 1: Prosty program IO
greet :: IO ()
greet = do 
  putStrLn "Witajcie!" 
  putStrLn "Miło Was widzieć"


readName :: IO ()
readName = do
  greet
  putStrLn "Podaj imię"
  -- x <- readLn :: IO String 
  x <- getLine
  putStrLn $ "Twoje imię to: " ++ x



-- program zczytujacy dwie liczby i dodajacy wyniki do siebie
readNumbers :: IO ()
readNumbers = do 
  greet 
  putStrLn "Podaj dwie liczby całkowite"
  x <- readLn :: IO Int  
  y <- readLn :: IO Int 
  let sum = x + y 
  putStrLn $ "Suma to " ++ show sum

-- program wczytujacy plik i wyswietlajacy go 
readAndDisplayFile :: IO ()
readAndDisplayFile = do 
  putStrLn "Podaj nazwę pliku do przeczytania"
  fileName <- getLine
  -- try :: Exception e => IO a -> IO (Either e a)
  tryFileContent <- try (readFile fileName) :: IO (Either IOError String)
  case tryFileContent of 
    Right fileContent -> do 
      putStrLn "Plik dobrze odczytany. Oto jego zawartosc"
      putStr fileContent 
    Left errorMsg -> do 
      putStrLn "Nastapil blad czytania pliku"
      putStr $ show errorMsg
 
-----------------------------------------------------------------------------
-- TRANSFORMATORY MONAD
-----------------------------------------------------------------------------

{-
Transformatory monad pozwalają łączyć funkcjonalności różnych monad.
Przykładowe transformatory:

1. StateT - dodaje stan do dowolnej monady
   newtype StateT s m a = StateT { runStateT :: s -> m (s, a) }

2. ReaderT - dodaje środowisko tylko do odczytu
   newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

3. WriterT - dodaje możliwość akumulowania wartości
   newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

4. ExceptT - dodaje obsługę błędów
   newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }

5. MaybeT - dodaje możliwość braku wartości
   newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
-}

-- Podstawowe operacje dla transformatorów:
-- lift :: (Monad m) => m a -> t m a

-- Operacje specyficzne dla StateT:
-- get :: Monad m => StateT s m s
-- put :: Monad m => s -> StateT s m ()
-- modify :: Monad m => (s -> s) -> StateT s m ()

-----------------------------------------------------------------------------
-- PRZYKŁAD 2: APLIKACJA Z UŻYCIEM TRANSFORMATORÓW MONAD
-----------------------------------------------------------------------------

-- Definicja stanu aplikacji
data AppState = AppState {
  userName :: String,
  score :: Int,
  gameOver :: Bool
} deriving (Show)

-- Typ reprezentujący obliczenia w naszej aplikacji
type AppM a = StateT AppState IO a

-- f :: AppM a 
-- f' :: AppState -> IO (AppState,a)
--
--                          ┌─── a

-- AppState   ───┐          ┌─── AppState
--               │          │
--               └──→[  f ]─┘
--               │          │
-- World   -----─┘          └─── World
--

-- Inicjalizacja stanu i uruchomienie aplikacji
initialState :: AppState
initialState = AppState { userName = "", score = 0, gameOver = False }


-- Wyświetla aktualny stan
displayState :: AppM ()
displayState = do 
  x <- get 
  lift $ putStrLn "Stan gry to:"
  lift $ putStr $ show x

-- Zwiększa wynik
increaseScoreBy :: Int -> AppM ()
increaseScoreBy n = do 
  AppState {..} <- get 
  put $ AppState {score = score + n, ..}


-- Pyta użytkownika o imię i aktualizuje stan
askForName :: AppM ()
askForName = do 
  lift $ putStrLn "Napisz swoje imie"
  x <- lift getLine 
  AppState {..} <- get 
  put $ AppState {userName = x, ..}



-- Przykładowe użycie
runApp :: IO ()
runApp = evalStateT (do 
    askForName
    increaseScoreBy 10 
    increaseScoreBy 20
    displayState
  ) initialState


-----------------------------------------------------------------------------
-- PRZYKŁAD 3: ŁĄCZENIE WIELU TRANSFORMATORÓW
-----------------------------------------------------------------------------

-- Definicja typów błędów
data AppError = InvalidInput String | GameError String 
  deriving (Show)

-- Definicja stanu aplikacji
data GameState = GameState {
  playerHealth :: Int,
  playerScore :: Int
} deriving (Show)

-- Stos monadyczny łączący IO, State i Either
type GameM a = ExceptT AppError (StateT GameState IO) a

-- Funkcje pomocnicze
throwError :: AppError -> GameM a
throwError = undefined

liftIO :: IO a -> GameM a
liftIO = undefined

getState :: GameM GameState
getState = undefined

modifyState :: (GameState -> GameState) -> GameM ()
modifyState = undefined

-- Przykład użycia
takeDamage :: Int -> GameM ()
takeDamage = undefined

addScore :: Int -> GameM ()
addScore = undefined

-- Uruchomienie przykładu
runGame :: IO ()
runGame = undefined
