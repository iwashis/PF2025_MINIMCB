module Lecture06 where 



newtype Writer w b = Writer {runWriter :: (w,b)}
-- ^ runWriter wydobywa wynik i log z obliczeń Writer
{- 
Diagram dla Monady Writer:
  
  Funkcja f: a → Writer w b może być reprezentowana jako:
  
     ┌───┐  B
  A  │   │━━━>
  ━━━▷ f │
     │   │  w
     └───┘━━━▷


Kompozycja f >=> g wygląda tak:
 
     ┌───┐  B  ┌───┐  C
  A  │   │━━━━━▷   │━━━>
  ━━━▷ f │     │ g │
     │   │  w  │   │w      w
     └───┘━━━┓ └───┘━━━O━━━━━▷
             ┃         ┃
             ┗━━━━━━━━━┛
-}



instance Functor (Writer w) where
  -- fmap :: (a -> b) -> Writer w a -> Writer w b
  fmap = undefined  -- Przekształci wartość, zachowując log

{-
  Instancja Funktor mapuje wartość, pozostawiając log bez zmian
  
  Diagram fmap f :: Writer w a -> Writer w b
     ┌────┐    
  A  │    │  B  
  ━━━▷ f  │━━━>
     │    │    
     └────┘    
  ━━━━━━━━━━━━▷
    w bez zmian
-}
  -- Instancja Applicative do stosowania funkcji w kontekście Writer
  -- 'pure' tworzy Writer z pustym logiem (mempty)
 
instance (Monoid w) => Applicative (Writer w) where
{-
  -- pure :: a -> Writer w a
  Diagram pure:
       ┌─────┐    
    A  │pure │  A  
    ━━━│     │━━━>
       │     │    
       └─────┘━━━▷
              mempty (pusty log)
-}
  pure = undefined  -- Tworzy Writer z wartością i pustym logiem
  
  -- (<*>) :: Writer w (a -> b) -> Writer w a -> Writer w b
  (<*>) = undefined  -- Zastosuje funkcję do wartości i połączy logi


instance (Monoid w) => Monad (Writer w) where
  
  {-
    >>= (bind) diagram:
          ┌───────┐    
  A       │       │  C  
   ━━━━━━━▷   f   │━━━━━━━━━>
       w  │       │  w    w
   ━━┓    └───────┘━━━━O━━━━▷
     ┃                 ┃
     ┗━━━━━━━━━━━━━━━━━┛
               połączone
  -}
  -- (>>=) :: Writer w a -> (a -> Writer w b) -> Writer w b
  (>>=) = undefined  -- Połączy obliczenia i zgromadzi logi

-- Podstawowe operacje Writer

{-
  'tell' dodaje wpis do logu bez zmiany wartości obliczenia
  
  Diagram tell:
     ┌────┐    
     │tell│  ()  
     │ w  │━━━>
     │    │    
     └────┘━━━▷
              w
-}
tell :: w -> Writer w ()
tell = undefined  -- Stworzy Writer z wartością () i podanym logiem

{-
  'listen' udostępnia log obok wyniku obliczenia
  
  Diagram listen:
     ┌──────┐       
  A  │      │  (A,W)  
  ━━━▷listen│━━━━━>
     │      │       
     └──────┘━━━━━▷
               W
-}
listen :: Writer w a -> Writer w (a, w)
listen = undefined  -- Udostępni log w wartości wyniku, zachowując go również w logu



-- Przykład systemu bankowego - pokazuje praktyczne zastosowanie monady Writer

{-
  Struktura danych Account:
  - accountId: Unikalny identyfikator konta
  - balance: Aktualny stan konta
-}
data Account = Account 
  { accountId :: String  -- Identyfikator konta
  , balance :: Int       -- Aktualny stan
  } deriving (Show)

{-
  deposit: Dodaje pieniądze do konta z logowaniem
  
  Przepływ:
  1. Loguje próbę wpłaty
  2. Sprawdza poprawność kwoty (musi być dodatnia)
  3. Aktualizuje saldo jeśli kwota jest poprawna, w przeciwnym razie pozostawia bez zmian
  4. Loguje wynik (sukces/błąd)
  5. Zwraca zaktualizowane konto
  
  Diagram deposit:
     ┌────────┐              
  Acc│        │  Acc          
  ━━━▷deposit │━━━━>
 amt │        │              
  ━━━▷        │              
     └────────┘━━━━▷
                logi
-}
deposit :: Account -> Int -> Writer [String] Account
deposit = undefined  -- Doda kwotę do salda konta z odpowiednim logowaniem

{-
  withdraw: Pobiera pieniądze z konta z logowaniem
  
  Przepływ:
  1. Loguje próbę wypłaty
  2. Sprawdza poprawność kwoty (musi być dodatnia)
  3. Sprawdza czy dostępne są wystarczające środki
  4. Aktualizuje saldo jeśli kwota jest poprawna, w przeciwnym razie pozostawia bez zmian
  5. Loguje wynik (sukces/błąd)
  6. Zwraca zaktualizowane konto
  
  Diagram withdraw:
     ┌─────────┐              
  Acc│         │  Acc          
  ━━━▷withdraw │━━━━>
 amt │         │              
  ━━━▷         │              
     └─────────┘━━━━▷
                 logi
-}
withdraw :: Account -> Int -> Writer [String] Account
withdraw = undefined  -- Odejmie kwotę z salda konta z odpowiednim logowaniem

{-
  transfer: Przenosi pieniądze między kontami z logowaniem
  
  Przepływ:
  1. Loguje próbę przelewu
  2. Próbuje wypłacić z konta źródłowego
  3. Jeśli wypłata się powiedzie, wpłaca na konto docelowe
  4. Loguje ogólny wynik
  5. Zwraca oba zaktualizowane konta
  
-}
transfer :: Account -> Account -> Int -> Writer [String] (Account, Account)
transfer = undefined  -- Przeniesie kwotę między kontami z odpowiednim logowaniem

