# Rekurencja ogonkowa 

1. **Rekurencja ogonkowa i NWD z rozszerzonym przetwarzaniem danych wejściowych**  

    Napisz funkcję `tailGCD :: Integral a => a -> a -> a`, która oblicza największy 
    wspólny dzielnik (NWD) dwóch liczb całkowitych.

2. **Rekurencja ogonkowa i quicksort z użyciem jawnego stosu**  

    Zaimplementuj wersję algorytmu quicksort: `tailQuickSort :: Ord a => [a] -> [a]`, 
    która unika głębokiej rekurencji, używając akumulatora lub jawnego stosu do 
    zarządzania podlistami, które trzeba posortować.

3. **Rekurencja ogonkowa i obliczanie zbioru potęgowego (power set)**  

    Napisz funkcję `tailPowerSet :: [a] -> [[a]]`, która oblicza zbiór potęgowy 
    (power set) danej listy, wykorzystując tail recursion. 
    Upewnij się, że:  
    - Używasz akumulatora, który stopniowo buduje zbiór potęgowy.  
    - Unikasz tworzenia się pośrednich wyrażeń (thunks) podczas łączenia podzbiorów.  
    - Funkcja działa efektywnie nawet dla list o umiarkowanym rozmiarze.

4. **Rekurencja ogonkowa i sumowanie zagnieżdżonej struktury list**  

    Zdefiniuj typ rekurencyjny dla zagnieżdżonych list:  
    ```haskell
    data NestedList a = Elem a | List [NestedList a]
    ```
    Następnie napisz tail-recursive funkcję: 
    `sumNested :: Num a => NestedList a -> a`, 
    która oblicza sumę wszystkich elementów w zagnieżdżonej liście. 

5. **Rekurencja ogonkowa i przeglądanie drzewa**  

    Dla drzewa binarnego zdefiniowanego jako:  
    ```haskell
    data Tree a = Empty | Node a (Tree a) (Tree a)
    ```
    napisz funkcję: `preorder :: Tree a -> [a]`, 
    która odwiedza węzły drzewa w następującej kolejności: 
    najpierw bieżący węzeł, potem jego lewe poddrzewo, a na końcu prawe poddrzewo, zwracając listę wartości w tej kolejności.

# ADT i typeclassy

6. **Słownik oparty na drzewie binarnym z balansowaniem**  

    Zdefiniuj algebryczny typ danych reprezentujący drzewo wyszukiwań binarnych (BST), 
    które będzie służyło jako słownik mapujący klucze na wartości. Następnie zaimplementuj 
    następujące operacje:  
    - *Wstawianie*: Dodanie pary klucz-wartość.  
    - *Wyszukiwanie*: Pobranie wartości przypisanej do klucza.  
    - *Usuwanie*: Usunięcie klucza (oraz odpowiadającej mu wartości) z drzewa.  
    - *Aktualizacja*: Modyfikacja wartości przypisanej do klucza.  
    - *Balansowanie*: Zaimplementuj procedurę balansowania (np. wykorzystując algorytm drzewa 
        AVL lub czerwono-czarnego), aby drzewo pozostało zbalansowane po operacjach wstawiania i usuwania.  

7. **Interpreter wyrażeń z różniczkowaniem i upraszczaniem**  

    Zdefiniuj algebraiczny typ danych reprezentujący wyrażenia arytmetyczne 
    (uwzględniający zmienne, stałe, dodawanie, mnożenie i potęgowanie). Napisz funkcje, które:  
    - *Ewaluacja*: Obliczają wartość numeryczną wyrażenia, korzystając z mapowania zmiennych na liczby.  
    - *Różniczkowanie*: Symbolicznie różniczkują wyrażenie względem danej zmiennej.  
    - *Upraszczanie*: Redukują wyrażenie do prostszej formy poprzez stosowanie uproszczeń algebraicznych 
    (np. eliminowanie składników zerowych, łączenie wyrazów podobnych).  
    
8. **Własna leniwa lista z obsługą nieskończoności**  

    Stwórz własny typ listy (np. `data MyList a = Nil | Cons a (MyList a)`), który wspiera leniwą ewaluację. 
    Zaimplementuj następujące funkcje:  
    - `myMap`: Funkcję analogiczną do `map`.  
    - `myFoldr`: Funkcję złożenia prawego (`foldr`), która potrafi działać na nieskończonych listach, jeśli to możliwe.  
    - `myFilter`: Funkcję analogiczną do `filter`.  
    Następnie zdefiniuj instancje `Functor, Foldable` dla `Mylist`.

9. **Reprezentacja grafu i algorytmy**  

    Zdefiniuj algebryczny typ danych reprezentujący graf nieskierowany, w którym wierzchołki mogą 
    przechowywać dowolne dane. Napisz funkcje, które:  
    - *Przeszukiwanie w głąb (DFS)*: Przemierzają graf, zaczynając od danego wierzchołka.  
    - *Wykrywanie cykli*: Sprawdzają, czy graf zawiera cykle.  
    - *Znajdowanie ścieżki*: Znajdują ścieżkę między dwoma wierzchołkami (jeśli taka istnieje).  

10. **Znane typeclassy dla drzew** 

    Zdefiniuj instancje `Show, Eq, Semigroup, Monoid, Foldable, Functor` dla parametrycznego typu danych 
    `data T a = EmptyT | LeafT a | InnerT (T a) (T a)`.

# Pytania dodatkowe

a. **Rekurencja ogonkowa i wyszukiwanie w drzewie BST**
Dla drzewa BST zdefiniowanego jako `data BST a = Empty | Node a (BST a) (BST a)`
napisz funkcję `tailSearch :: Ord a => a -> BST a -> Bool`, która wyszukuje podany element w drzewie, 
wykorzystując rekurencję ogonkową z zastosowaniem jawnego stosu lub akumulatora do zarządzania stanem przeszukiwania.

b. **Rekurencja ogonkowa i znajdowanie najmniejszego elementu**
Napisz funkcję `tailMinimum :: Ord a => [a] -> a`, 
która zwraca najmniejszy element niepustej listy, 
wykorzystując rekurencję ogonkową z akumulatorem w celu eliminacji zbędnych wyrażeń opóźnionych.

c. **Rekurencja ogonkowa i ewaluacja wyrażeń arytmetycznych**
Zdefiniuj abstrakcyjny typ danych dla wyrażeń arytmetycznych:
`data Expr = Val Int | Add Expr Expr | Mul Expr Expr | Sub Expr Expr`
Następnie napisz funkcję `tailEval :: Expr -> Int`, 
która ocenia dane wyrażenie, stosując rekurencję ogonkową z odpowiednimi 
akumulatorami do przechowywania częściowych wyników. 
