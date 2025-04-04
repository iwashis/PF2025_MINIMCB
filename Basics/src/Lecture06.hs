{-# LANGUAGE RecordWildCards #-}
module Lecture06 where 


 -- a |-> (w,a) 
 -- W - ustalone z gory 
 -- T(A) = W x A
 -- data, newtype, type
newtype Writer w a =  Writer {runWriter :: (w,a)} 

instance (Show a, Show w) => Show (Writer w a) where 
  show (Writer (w,a)) = "Value:\n" ++ show a ++ "\nLogs:\n" ++ show w

exampleWriter :: Writer Int String 
exampleWriter = Writer (1,"aaa")

-- newtype Writer w a = Writer {runWriter :: (w,a)}
-- ^ runWriter wydobywa wynik i log z obliczeń Writer
{- 
Diagram dla Monady Writer:
  
  Funkcja f: a → Writer w b może być reprezentowana jako:
  
     ┌───┐  B
  A  │   │━━━>
  ━━━▷ f │
     │   │  w
     └───┘━━━▷

struktura monady na Writer w a da nam mozliwosc skladania: 
f : a -> Writer w b 
g : b -> Writer w c
------------------------
f >=> g : a -> Writer w c
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
  fmap f (Writer (w,x)) = Writer (w, f x) -- Przekształci wartość, zachowując log
  -- fmap f writer = Writer $ (\(w,x) -> (w, f x)) (runWriter writer)  

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
  pure x = Writer (mempty, x) -- Tworzy Writer z wartością i pustym logiem
  
  -- (<*>) :: Writer w (a -> b) -> Writer w a -> Writer w b
  ( Writer (w1, f) ) <*> (Writer (w2, x))  = Writer (w1 <> w2, f x)-- Zastosuje funkcję do wartości i połączy logi
  -- (<*>) = ap


instance (Monoid w) => Monad (Writer w) where
  {-
    >>= (bind) diagram:
          ┌───────┐    
  A       │       │  B  
   ━━━━━━━▷   f   │━━━━━━━━━>
       w  │       │  w    w
   ━━┓    └───────┘━━━━O━━━━▷
     ┃                 ┃
     ┗━━━━━━━━━━━━━━━━━┛
               połączone
  -}
  -- (>>=) :: Writer w a -> (a -> Writer w b) -> Writer w b
  (Writer (w1,x)) >>= f = Writer (w1 <> w2, y )-- Połączy obliczenia i zgromadzi logi
    where
      (w2, y) = runWriter $ f x
      

-- Podstawowe operacje Writer

{-
  'tell' dodaje wpis do logu bez zmiany wartości obliczenia
  
  Diagram tell:
     ┌────┐    
   w │tell│  ()  
   -->    │━━━>
     │    │    
     └────┘━━━▷
              w
-}
-- data () = ()
tell :: w -> Writer w ()
tell x = Writer (x,())-- Stworzy Writer z wartością () i podanym logiem

{-
  'listen' udostępnia log obok wyniku obliczenia
  
  Diagram listen:
     ┌──────┐       
  A  │      │  (A,W)  
  ━━━▷listen│━━━━━>
     │      │       
  -->└──────┘━━━━━▷
               W
-}
listen :: Writer w a -> Writer w (a, w)
listen (Writer (w,x)) = Writer (w, (x,w)) -- Udostępni log w wartości wyniku, zachowując go również w logu



-- Przykład systemu bankowego - pokazuje praktyczne zastosowanie monady Writer

{-
  Struktura danych Account:
  - accountId: Unikalny identyfikator konta
  - balance: Aktualny stan konta
-}

type AccountID = String 

type Balance = Int
data Account = Account 
  { accountId :: AccountID  -- Identyfikator konta
  , balance :: Balance -- Aktualne saldo 
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

data LogType = Info | Warning | Error

data LogEntry = LogEntry LogType String

instance Show LogEntry where
  show (LogEntry logType message) = coloredLogType logType ++ ": " ++ message
    where
      coloredLogType Info = "\ESC[32mINFO\ESC[0m"    -- Green
      coloredLogType Warning = "\ESC[33mWARNING\ESC[0m"  -- Yellow
      coloredLogType Error = "\ESC[31mERROR\ESC[0m"   -- Red


newtype Logs = Logs [LogEntry]
exampleLogs = Logs [LogEntry Info "aaa", LogEntry Error "no nie"]
-- newtype Logs = Logs [String]

instance Show Logs where
  show (Logs []) = ""
  show (Logs (lg:logs)) = show lg ++ "\n" ++ show (Logs logs)

instance Semigroup Logs where 
  (Logs l1) <> (Logs l2) = Logs (l1 <> l2) 
instance Monoid Logs where 
  mempty = Logs mempty
  
type BankRegister = Writer Logs 

logMe :: LogType -> String -> BankRegister () 
logMe typ lg = tell $ Logs [LogEntry typ lg]


deposit :: Account -> Int -> BankRegister Account
deposit account@(Account id balance) value = do -- Add amount to account balance with appropriate logging
  logMe Info $ "Attempting to deposit " ++ show value ++ " to account " ++ show account
  if value > 0 then do 
    logMe Info "Valid amount, deposit completed"
    pure $ Account id (balance + value) -- balance = balance + value 
  else do 
    logMe Error "Negative amount"
    pure account


exampleAccount :: Account 
exampleAccount = Account "Magic Account" 11
  

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
--
-- withdraw :: Account -> Int -> Writer [String] Account
-- withdraw account@(Account id balance) value = do
--   tell ["Attempting to withdraw money from account " ++ show account] 
--   if balance >= value 
--     then do 
--      tell ["Amount deducted from account"]
--      pure (Account id (balance - value))
--     else do 
--       tell ["Error: insufficient balance in account " ++ show account]
--       pure account


-- przepisane withdraw uzywajac RecordWildCards
withdraw :: Account -> Int ->BankRegister Account
withdraw account@(Account {..}) value = do
  logMe Info $ "Attempting to withdraw money from account " ++ show account
  if balance >= value 
    then do 
      logMe Info "Amount deducted from account"
      pure (account { balance = balance - value })
    else do 
      logMe Error $ "Insufficient balance in account " ++ show account
      pure account
 -- Odejmie kwotę z salda konta z odpowiednim logowaniem


-- | TODO: Zaimplementować funkcję, która pobiera saldo konta i loguje operację.
-- | Funkcja powinna używać monady BankRegister do śledzenia operacji.
getBalance :: Account -> BankRegister Int 
getBalance account@(Account {..}) = (logMe Info $ "Reading balance from account " ++ show account) >>= \x -> return balance 

fullWithdraw :: Account -> Int ->BankRegister Account
fullWithdraw account@(Account {..}) value = do 
  logMe Info $ "Attempting to withdraw money from account " ++ show account
  b <- getBalance account
  if b >= value 
    then do 
      let message = "Amount deducted from account" -- tylko ze by pokazac syntax let w do oraz poza do 
      logMe Info message
      pure (let newBalance = b - value in account { balance = newBalance }) -- tu przyklad let poza do-block
    else do 
      logMe Error $ "Insufficient balance in account " ++ show account
      pure account
 
-- | Funkcja powinna zwracać identyfikator konta w kontekście BankRegister.
getAccountId :: Account -> BankRegister String
getAccountId a@(Account {..}) = do
  logMe Info $ "Reading account id of " ++ show a
  pure accountId


-- | TODO: Zaimplementować funkcję audytu operacji wypłaty, która sprawdza 
-- | czy wypłata jest możliwa i loguje wynik tej weryfikacji.
-- | Funkcja powinna używać operacji `listen` aby przechwycić logi podczas weryfikacji.
auditWithdraw :: Account -> Int -> BankRegister Bool
auditWithdraw account value = do
  b <- getBalance account
  let isPossible = b >= value 
  if isPossible then logMe Info $  "Withdrawal from " ++ show account ++ " possible."
  else logMe Error $ "Withdrawal from " ++ show account ++ " not possible."
  pure isPossible


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

