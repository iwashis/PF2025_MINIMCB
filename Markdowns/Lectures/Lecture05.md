# Monada Writer i Implementacja Systemu Bankowego

## Monada Writer

Monada Writer pozwala na "dopisywanie" dodatkowych informacji podczas wykonywania obliczeń. Można to rozumieć jako funkcję, która oprócz wyniku zwraca także "dziennik" operacji.

### Definicja Monady Writer

Zdefiniujmy najpierw strukturę danych Writer:

```haskell
data Writer w a = Writer {runWriter :: (w, a)}
```

Typ `Writer w a` reprezentuje obliczenie, które produkuje wartość typu `a` oraz dodatkową informację (log) typu `w`. 
Funkcja `runWriter` pozwala na wydobycie pary (log, wartość) z obiektu Writer.

Dla wygody, dodajmy instancję Show dla naszej monady:

```haskell
instance (Show a, Show w) => Show (Writer w a) where 
  show (Writer (w, a)) = "Value:\n" ++ show a ++ "\nLogs:\n" ++ show w

exampleWriter :: Writer Int String 
exampleWriter = Writer (1, "aaa")
```

### Diagram Monady Writer

Monada Writer może być reprezentowana za pomocą następującego diagramu:

```
Funkcja f: a → Writer w b może być reprezentowana jako:
  
     ┌───┐  B
  A  │   │━━━>
  ━━━▷ f │
     │   │  w
     └───┘━━━▷
```

Struktura monady Writer pozwala na składanie funkcji typu `a -> Writer w b` i `b -> Writer w c` w nową funkcję `a -> Writer w c`:

```
Kompozycja f >=> g wygląda tak:
 
     ┌───┐  B  ┌───┐  C
  A  │   │━━━━━▷   │━━━>
  ━━━▷ f │     │ g │
     │   │  w  │   │w      w
     └───┘━━━┓ └───┘━━━O━━━━━▷
             ┃         ┃
             ┗━━━━━━━━━┛
```

### Implementacja Instancji Funktor

Najpierw zaimplementujmy instancję Funktor dla naszej monady Writer:

```haskell
instance Functor (Writer w) where
  -- fmap :: (a -> b) -> Writer w a -> Writer w b
  fmap f (Writer (w, x)) = Writer (w, f x) -- Przekształci wartość, zachowując log
```

Funkcja `fmap` przekształca wartość wewnątrz monady Writer, pozostawiając log bez zmian:

```
Diagram fmap f :: Writer w a -> Writer w b
     ┌────┐    
  A  │    │  B  
  ━━━▷ f  │━━━>
     │    │    
     └────┘    
  ━━━━━━━━━━━━▷
    w bez zmian
```

### Implementacja Instancji Applicative

Teraz zaimplementujmy instancję Applicative dla Writer:

```haskell
instance (Monoid w) => Applicative (Writer w) where
  -- pure :: a -> Writer w a
  pure x = Writer (mempty, x) -- Tworzy Writer z wartością i pustym logiem
  
  -- (<*>) :: Writer w (a -> b) -> Writer w a -> Writer w b
  (Writer (w1, f)) <*> (Writer (w2, x)) = Writer (w1 <> w2, f x) -- Zastosuje funkcję do wartości i połączy logi
```

Funkcja `pure` tworzy Writer z pustym logiem (reprezentowanym przez `mempty` z monoidu) i podaną wartością:

```
Diagram pure:
       ┌─────┐    
    A  │pure │  A  
    ━━━│     │━━━>
       │     │    
       └─────┘━━━▷
              mempty (pusty log)
```

Operator `<*>` stosuje funkcję zawartą w pierwszym Writerze do wartości z drugiego Writera, łącząc jednocześnie ich logi za pomocą operacji `<>` (łączenia monoidów).

### Implementacja Instancji Monad

Wreszcie, zaimplementujmy instancję Monad dla Writer:

```haskell
instance (Monoid w) => Monad (Writer w) where
  -- (>>=) :: Writer w a -> (a -> Writer w b) -> Writer w b
  (Writer (w1, x)) >>= f = Writer (w1 <> w2, y) -- Połączy obliczenia i zgromadzi logi
    where
      (w2, y) = runWriter $ f x
```

Operator `>>=` (bind) łączy obliczenia monadyczne, aplikując funkcję `f` do wartości wewnątrz Writera i łącząc logi:

```
>>= (bind) diagram:
          ┌───────┐    
  A       │       │  B  
   ━━━━━━━▷   f   │━━━━━━━━━>
       w  │       │  w    w
   ━━┓    └───────┘━━━━O━━━━▷
     ┃                 ┃
     ┗━━━━━━━━━━━━━━━━━┛
               połączone
```

### Podstawowe Operacje Writer

Teraz zdefiniujmy kilka podstawowych operacji dla monady Writer:

```haskell
-- tell dodaje wpis do logu bez zmiany wartości obliczenia
tell :: w -> Writer w ()
tell x = Writer (x, ()) -- Stworzy Writer z wartością () i podanym logiem

-- listen udostępnia log obok wyniku obliczenia
listen :: Writer w a -> Writer w (a, w)
listen (Writer (w, x)) = Writer (w, (x, w)) -- Udostępni log w wartości wyniku, zachowując go również w logu
```

Funkcja `tell` pozwala na dodanie informacji do logu bez zmiany wyniku obliczenia:

```
Diagram tell:
     ┌────┐    
   w │tell│  ()  
   -->    │━━━>
     │    │    
     └────┘━━━▷
              w
```

Funkcja `listen` udostępnia log jako część wyniku, co może być przydatne do analizy logów w trakcie wykonywania obliczeń:

```
Diagram listen:
     ┌──────┐       
  A  │      │  (A,W)  
  ━━━▷listen│━━━━━>
     │      │       
  -->└──────┘━━━━━▷
               W
```

## Praktyczne Zastosowanie - System Bankowy

Teraz zobaczmy, jak wykorzystać monadę Writer w praktycznym przykładzie - prostym systemie bankowym z logowaniem operacji.

### Definicja Struktury Danych

Zdefiniujmy najpierw struktury danych potrzebne dla naszego systemu:

```haskell
type AccountID = String 
type Balance = Int

data Account = Account 
  { accountId :: AccountID  -- Identyfikator konta
  , balance :: Balance -- Aktualne saldo 
  } deriving (Show)
```

Typ `Account` reprezentuje konto bankowe z identyfikatorem i saldem.

### System Logowania

Teraz zdefiniujmy system logowania, który będzie używany przez nasz system bankowy:

```haskell
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
```

Stworzyliśmy typy `LogType` i `LogEntry` do reprezentowania rodzajów i zawartości logów, a także typ `Logs` jako kolekcję wpisów w logu. Dzięki instancjom `Semigroup` i `Monoid` dla `Logs`, możemy łączyć logi za pomocą operatora `<>`.

Definiujemy także alias typu `BankRegister` dla `Writer Logs` oraz pomocniczą funkcję `logMe` do łatwego tworzenia wpisów w logu.

### Operacje Bankowe

Teraz zaimplementujmy podstawowe operacje bankowe z logowaniem:

```haskell
deposit :: Account -> Int -> BankRegister Account
deposit account@(Account id balance) value = do
  logMe Info $ "Attempting to deposit " ++ show value ++ " to account " ++ show account
  if value > 0 then do 
    logMe Info "Valid amount, deposit completed"
    pure $ Account id (balance + value)
  else do 
    logMe Error "Negative amount"
    pure account

exampleAccount :: Account 
exampleAccount = Account "Magic Account" 11
```

Funkcja `deposit` próbuje dodać pieniądze do konta, logując każdy krok operacji. Jeśli kwota jest dodatnia, wykonuje wpłatę i loguje sukces. W przeciwnym razie loguje błąd i zwraca konto bez zmian.

Zaimplementujmy teraz operację wypłaty:

```haskell
withdraw :: Account -> Int -> BankRegister Account
withdraw account@(Account {..}) value = do
  logMe Info $ "Attempting to withdraw money from account " ++ show account
  if balance >= value 
    then do 
      logMe Info "Amount deducted from account"
      pure (account { balance = balance - value })
    else do 
      logMe Error $ "Insufficient balance in account " ++ show account
      pure account
```

Funkcja `withdraw` używa składni `RecordWildCards` dla łatwiejszego dostępu do pól rekordu. Próbuje ona odjąć pieniądze z konta, logując każdy krok. Jeśli saldo jest wystarczające, wykonuje wypłatę i loguje sukces. W przeciwnym razie loguje błąd i zwraca konto bez zmian.

Dodajmy funkcję do sprawdzania salda konta:

```haskell
getBalance :: Account -> BankRegister Int 
getBalance account@(Account {..}) = 
  (logMe Info $ "Reading balance from account " ++ show account) >>= \_ -> return balance
```

Funkcja `getBalance` loguje operację sprawdzania salda i zwraca aktualne saldo konta.

Teraz zaimplementujmy funkcję pobierającą identyfikator konta:

```haskell
getAccountId :: Account -> BankRegister String
getAccountId a@(Account {..}) = do
  logMe Info $ "Reading account id of " ++ show a
  pure accountId
```

Funkcja `getAccountId` loguje operację odczytywania identyfikatora konta i zwraca ten identyfikator.

Zaimplementujmy teraz funkcję do audytu operacji wypłaty, która sprawdza, czy wypłata jest możliwa:

```haskell
auditWithdraw :: Account -> Int -> BankRegister Bool
auditWithdraw account value = do
  b <- getBalance account
  let isPossible = b >= value 
  if isPossible then 
    logMe Info $ "Withdrawal from " ++ show account ++ " possible."
  else 
    logMe Error $ "Withdrawal from " ++ show account ++ " not possible."
  pure isPossible
```

Funkcja `auditWithdraw` najpierw pobiera saldo konta, sprawdza czy wypłata jest możliwa, loguje odpowiedni komunikat, a następnie zwraca wynik sprawdzenia.

Zaimplementujmy bardziej złożoną operację - pełną wypłatę, która najpierw sprawdza saldo konta:

```haskell
fullWithdraw :: Account -> Int -> BankRegister Account
fullWithdraw account@(Account {..}) value = do 
  logMe Info $ "Attempting to withdraw money from account " ++ show account
  b <- getBalance account
  if b >= value 
    then do 
      let message = "Amount deducted from account"
      logMe Info message
      pure (let newBalance = b - value in account { balance = newBalance })
    else do 
      logMe Error $ "Insufficient balance in account " ++ show account
      pure account
```

Funkcja `fullWithdraw` pokazuje, jak łączyć kilka operacji w sekwencję w monadzie Writer. Najpierw loguje próbę wypłaty, potem sprawdza saldo za pomocą `getBalance`, a następnie wykonuje wypłatę lub loguje błąd w zależności od wyniku sprawdzenia.

Na koniec, zaimplementujmy operację transferu środków między kontami:

```haskell
transfer :: Account -> Account -> Int -> BankRegister (Account, Account)
transfer fromAcc toAcc amount = do
  logMe Info $ "Attempting to transfer " ++ show amount ++ " from " ++ show fromAcc ++ " to " ++ show toAcc
  
  -- Sprawdzamy, czy przelew jest możliwy
  canWithdraw <- auditWithdraw fromAcc amount
  
  if canWithdraw then do
    -- Jeśli przelew jest możliwy, wykonujemy operacje wypłaty i wpłaty
    updatedFromAcc <- withdraw fromAcc amount
    updatedToAcc <- deposit toAcc amount
    logMe Info "Transfer completed successfully"
    pure (updatedFromAcc, updatedToAcc)
  else do
    -- Jeśli przelew nie jest możliwy, zwracamy oryginalne konta
    logMe Error "Transfer failed due to insufficient funds"
    pure (fromAcc, toAcc)
```

Funkcja `transfer` pokazuje, jak połączyć kilka operacji bankowych w złożoną transakcję. 
Najpierw sprawdza, czy przelew jest możliwy za pomocą `auditWithdraw`, a następnie, jeśli jest, wykonuje operacje wypłaty i wpłaty. Operacje te są logowane na każdym kroku, co daje pełny ślad transakcji.

