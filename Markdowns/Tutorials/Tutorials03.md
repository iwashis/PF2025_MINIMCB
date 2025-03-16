# ADT i typeclassy

1. **Słownik oparty na drzewie binarnym z balansowaniem**  

    Zdefiniuj algebryczny typ danych reprezentujący drzewo wyszukiwań binarnych (BST), 
    które będzie służyło jako słownik mapujący klucze na wartości. Następnie zaimplementuj 
    następujące operacje:  
    - *Wstawianie*: Dodanie pary klucz-wartość.  
    - *Wyszukiwanie*: Pobranie wartości przypisanej do klucza.  
    - *Usuwanie*: Usunięcie klucza (oraz odpowiadającej mu wartości) z drzewa.  
    - *Aktualizacja*: Modyfikacja wartości przypisanej do klucza.  
    - *Balansowanie*: Zaimplementuj procedurę balansowania (np. wykorzystując algorytm drzewa 
        AVL lub czerwono-czarnego), aby drzewo pozostało zbalansowane po operacjach wstawiania i usuwania.  

2. **Interpreter wyrażeń z różniczkowaniem i upraszczaniem**  

    Zdefiniuj algebraiczny typ danych reprezentujący wyrażenia arytmetyczne 
    (uwzględniający zmienne, stałe, dodawanie, mnożenie i potęgowanie). Napisz funkcje, które:  
    - *Ewaluacja*: Obliczają wartość numeryczną wyrażenia, korzystając z mapowania zmiennych na liczby.  
    - *Różniczkowanie*: Symbolicznie różniczkują wyrażenie względem danej zmiennej.  
    - *Upraszczanie*: Redukują wyrażenie do prostszej formy poprzez stosowanie uproszczeń algebraicznych 
    (np. eliminowanie składników zerowych, łączenie wyrazów podobnych).  
    
3. **Własna leniwa lista z obsługą nieskończoności**  

    Stwórz własny typ listy (np. `data MyList a = Nil | Cons a (MyList a)`), który wspiera leniwą ewaluację. 
    Zaimplementuj następujące funkcje:  
    - `myMap`: Funkcję analogiczną do `map`.  
    - `myFoldr`: Funkcję złożenia prawego (`foldr`), która potrafi działać na nieskończonych listach, jeśli to możliwe.  
    - `myFilter`: Funkcję analogiczną do `filter`.  
    Następnie zdefiniuj instancje `Functor, Foldable` dla `Mylist`.

4. **Reprezentacja grafu i algorytmy**  

    Zdefiniuj algebryczny typ danych reprezentujący graf nieskierowany, w którym wierzchołki mogą 
    przechowywać dowolne dane. Napisz funkcje, które:  
    - *Przeszukiwanie w głąb (DFS)*: Przemierzają graf, zaczynając od danego wierzchołka.  
    - *Wykrywanie cykli*: Sprawdzają, czy graf zawiera cykle.  
    - *Znajdowanie ścieżki*: Znajdują ścieżkę między dwoma wierzchołkami (jeśli taka istnieje).  

5. **Znane typeclassy dla drzew** 

    Zdefiniuj instancje `Show, Eq, Foldable, Functor` dla parametrycznego typu danych 
    `data T a = EmptyT | LeafT a | InnerT (T a) (T a)`.
