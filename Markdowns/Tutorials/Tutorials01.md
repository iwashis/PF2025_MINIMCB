# List comprehensions

1. **Trójki Pitagorejskie**  
   Napisz funkcję `pythagoreanTriples :: Int -> [(Int, Int, Int)]`, która zwraca wszystkie trójki `(a, b, c)` spełniające warunki:  
   - `1 ≤ a < b < c ≤ n`  
   - `a² + b² == c²`  
   Wykorzystaj list comprehensions do wygenerowania wyniku.

2. **Pary liczb, których suma jest liczbą pierwszą**  
   Napisz funkcję `primeSumPairs :: [Int] -> [(Int, Int)]`, która przyjmuje listę liczb 
   całkowitych i zwraca wszystkie unikalne pary `(x, y)` (przy założeniu, że `x < y`), 
   dla których suma `x + y` jest liczbą pierwszą.  

3. **Wyodrębnianie podłańcuchów**  
   Napisz funkcję `substrings :: String -> [String]`, która zwraca listę wszystkich niepustych 
   podłańcuchów danego ciągu znaków. Na przykład dla ciągu `"abc"` wynik powinien 
   zawierać `"a"`, `"ab"`, `"abc"`, `"b"`, `"bc"` oraz `"c"`. Wykorzystaj 
   list comprehensions do wygenerowania wszystkich podłańcuchów.

4. **Pary dzielników**  
   Napisz funkcję `divisorPairs :: [Int] -> [(Int, Int)]`, która 
   przyjmuje listę liczb całkowitych i zwraca wszystkie różne pary `(x, y)` 
   (przy założeniu, że `x ≠ y`), dla których `x` dzieli `y` bez reszty (tj. `y mod x == 0`). 

5. **Kombinacje**  
    Napisz funkcję  
    ```haskell
    combinations :: Int -> [a] -> [[a]]
    ```
    która generuje wszystkie kombinacje k-elementowe z danej listy. 
    Na przykład, dla `k = 2` i listy `[1,2,3]` wynikiem powinno być `[[1,2], [1,3], [2,3]]`.

# Leniwa/gorliwa ewaluacja, `seq` i bang patterns

6. **Ścisła suma z użyciem `seq`**  
   Napisz funkcję `strictSum :: [Int] -> Int`, która oblicza sumę listy liczb całkowitych, 
   używając `seq` do wymuszenia ewaluacji akumulatora na każdym kroku. 
   Porównaj jej działanie z naiwną, leniwą implementacją sumowania.

7. **Rekurencyjna funkcja silnia z użyciem bang patterns**  
   Napisz rekurencyjną funkcję `factorial :: Int -> Int`, która oblicza silnię danej liczby. 
   Użyj bang patterns w akumulatorze. 

8. **Wymuszanie ewaluacji elementów krotki**  
   Napisz funkcję `forceTuple :: (Int, Int) -> Int`, która przyjmuje krotkę dwóch liczb całkowitych, 
   wymusza ewaluację obu elementów za pomocą `seq`, a następnie zwraca ich sumę. 
   Wyjaśnij, dlaczego wymuszanie ewaluacji może być konieczne w niektórych sytuacjach.

9. **Liczby Fibonacciego z `seq` vs. bang patterns**  
   Zaimplementuj dwie wersje generatora liczb Fibonacciego:  
   - Pierwsza wersja wykorzystuje `seq` do wymuszenia ewaluacji w funkcji pomocniczej.  
   - Druga wersja używa bang patterns w argumentach funkcji pomocniczej.  

10. **Unikanie wycieków pamięci w funkcji rekurencyjnej**  
    Napisz funkcję `strictRecursive :: Int -> Int`, która oblicza wynik przy użyciu rekurencji, 
    gdzie leniwa ewaluacja mogłaby prowadzić do wycieków pamięci. 
    Zrefaktoryzuj funkcję, używając `seq` lub bang patterns, aby wymusić ścisłą ewaluację. 

# Rekurencja ogonkowa 

11. **Rekurencja ogonkowa i NWD z rozszerzonym przetwarzaniem danych wejściowych**  
    Napisz funkcję `tailGCD :: Integral a => a -> a -> a`, która oblicza największy 
    wspólny dzielnik (NWD) dwóch liczb całkowitych.

12. **Rekurencja ogonkowa i quicksort z użyciem jawnego stosu**  
    Zaimplementuj wersję algorytmu quicksort: `tailQuickSort :: Ord a => [a] -> [a]`, 
    która unika głębokiej rekurencji, używając akumulatora lub jawnego stosu do 
    zarządzania podlistami, które trzeba posortować.

13. **Rekurencja ogonkowa i obliczanie zbioru potęgowego (power set)**  
    Napisz funkcję `tailPowerSet :: [a] -> [[a]]`, która oblicza zbiór potęgowy 
    (power set) danej listy, wykorzystując tail recursion. 
    Upewnij się, że:  
    - Używasz akumulatora, który stopniowo buduje zbiór potęgowy.  
    - Unikasz tworzenia się pośrednich wyrażeń (thunks) podczas łączenia podzbiorów.  
    - Funkcja działa efektywnie nawet dla list o umiarkowanym rozmiarze.

14. **Rekurencja ogonkowa i sumowanie zagnieżdżonej struktury list**  
    Zdefiniuj typ rekurencyjny dla zagnieżdżonych list:  
    ```haskell
    data NestedList a = Elem a | List [NestedList a]
    ```
    Następnie napisz tail-recursive funkcję: 
    `sumNested :: Num a => NestedList a -> a`, 
    która oblicza sumę wszystkich elementów w zagnieżdżonej liście. 

15. **Rekurencja ogonkowa i przeglądanie drzewa**  
    Dla drzewa binarnego zdefiniowanego jako:  
    ```haskell
    data Tree a = Empty | Node a (Tree a) (Tree a)
    ```
    napisz funkcję: `preorder :: Tree a -> [a]`, 
    która odwiedza węzły drzewa w następującej kolejności: 
    najpierw bieżący węzeł, potem jego lewe poddrzewo, a na końcu prawe poddrzewo, zwracając listę wartości w tej kolejności.

# ADT i typeclassy

16. **Słownik oparty na drzewie binarnym z balansowaniem**  
    Zdefiniuj algebryczny typ danych reprezentujący drzewo wyszukiwań binarnych (BST), 
    które będzie służyło jako słownik mapujący klucze na wartości. Następnie zaimplementuj 
    następujące operacje:  
    - *Wstawianie*: Dodanie pary klucz-wartość.  
    - *Wyszukiwanie*: Pobranie wartości przypisanej do klucza.  
    - *Usuwanie*: Usunięcie klucza (oraz odpowiadającej mu wartości) z drzewa.  
    - *Aktualizacja*: Modyfikacja wartości przypisanej do klucza.  
    - *Balansowanie*: Zaimplementuj procedurę balansowania (np. wykorzystując algorytm drzewa 
        AVL lub czerwono-czarnego), aby drzewo pozostało zbalansowane po operacjach wstawiania i usuwania.  

17. **Interpreter wyrażeń z różniczkowaniem i upraszczaniem**  
    Zdefiniuj algebraiczny typ danych reprezentujący wyrażenia arytmetyczne 
    (uwzględniający zmienne, stałe, dodawanie, mnożenie i potęgowanie). Napisz funkcje, które:  
    - *Ewaluacja*: Obliczają wartość numeryczną wyrażenia, korzystając z mapowania zmiennych na liczby.  
    - *Różniczkowanie*: Symbolicznie różniczkują wyrażenie względem danej zmiennej.  
    - *Upraszczanie*: Redukują wyrażenie do prostszej formy poprzez stosowanie uproszczeń algebraicznych 
    (np. eliminowanie składników zerowych, łączenie wyrazów podobnych).  
    
18. **Własna leniwa lista z obsługą nieskończoności**  
    Stwórz własny typ listy (np. `data MyList a = Nil | Cons a (MyList a)`), który wspiera leniwą ewaluację. 
    Zaimplementuj następujące funkcje:  
    - `myMap`: Funkcję analogiczną do `map`.  
    - `myFoldr`: Funkcję złożenia prawego (`foldr`), która potrafi działać na nieskończonych listach, jeśli to możliwe.  
    - `myFilter`: Funkcję analogiczną do `filter`.  

19. **Reprezentacja grafu i algorytmy**  
    Zdefiniuj algebryczny typ danych reprezentujący graf nieskierowany, w którym wierzchołki mogą 
    przechowywać dowolne dane. Napisz funkcje, które:  
    - *Przeszukiwanie w głąb (DFS)*: Przemierzają graf, zaczynając od danego wierzchołka.  
    - *Wykrywanie cykli*: Sprawdzają, czy graf zawiera cykle.  
    - *Znajdowanie ścieżki*: Znajdują ścieżkę między dwoma wierzchołkami (jeśli taka istnieje).  
