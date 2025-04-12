module Lecture08 where

import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

-----------------------------------------------------------------------------
-- MONADA IO
-----------------------------------------------------------------------------

{-
Monada IO pozwala na modelowanie obliczeń z efektami ubocznymi (wejście/wyjście)
w sposób zgodny z paradygmatem czystego programowania funkcyjnego.

Koncepcyjnie można rozumieć IO jako:
newtype IO a = IO { runIO :: World -> (World, a) }
-}

-- Podstawowe operacje IO:
-- getLine :: IO String -- Odczytuje linię ze standardowego wejścia
-- putStrLn :: String -> IO () -- Wypisuje tekst na standardowe wyjście
-- readFile :: FilePath -> IO String -- Odczytuje zawartość pliku
-- writeFile :: FilePath -> String -> IO () -- Zapisuje tekst do pliku

-- Przykład 1: Prosty program IO
greet :: IO ()
greet = undefined

main :: IO ()
main = undefined

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

-- Wyświetla aktualny stan
displayState :: AppM ()
displayState = undefined

-- Zwiększa wynik
increaseScore :: Int -> AppM ()
increaseScore = undefined

-- Pyta użytkownika o imię i aktualizuje stan
askForName :: AppM ()
askForName = undefined

-- Inicjalizacja stanu i uruchomienie aplikacji
initialState :: AppState
initialState = AppState { userName = "", score = 0, gameOver = False }

-- Przykładowe użycie
runApp :: IO ()
runApp = undefined

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
