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


