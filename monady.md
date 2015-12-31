# Monady - droga do oświecenia

Na postawie artykułów:

* [http://bartoszmilewski.com/2011/01/09/monads-for-the-curious-programmer-part-1/](http://bartoszmilewski.com/2011/01/09/monads-for-the-curious-programmer-part-1/)
* [http://bartoszmilewski.com/2011/03/14/monads-for-the-curious-programmer-part-2/](http://bartoszmilewski.com/2011/03/14/monads-for-the-curious-programmer-part-2/)

## Po co to komu
W programowaniu funkcjnym wywołanie funkcji z takimi samymi argumentami musi zawsze zwrócić ten sam wynik. Kompilator wykrywa taką sytuację i zapamiętuje wynik za pierwszym razem, żeby skrócić czas obliczeń. Obliczenia ze stanem mogą zwrócić inny wynik podczas każdego wywołania, np. poprzez dostęp do zmiennej globalnej albo zmiennych statycznych, które mogą być dodatkowo modyfikowane wewnątrz tych obliczeń, powodując powstanie efektów ubocznych.
W programowaniu często napotykamy na problemy, których nie da się w naturalny sposób przetłumaczyć na język funkcyjny. Wtedy najlepiej użyć programowania imperatywnego.
Monady umożliwiają elegancką implementację stanów, mutowalności, IO i innych za pomocą programowania funkcyjnego. Monady nie zabraniają powstawania skutków ubocznych funkcji ani dzielenia danych między wątkami, ale jednocześnie, dzięki systemowi typów, umożliwiają kompilatorowi sprawdzenie poprawności.

## Wstęp matematyczny
### Teoria kategorii
*Obiekt* - generalizacja zbioru.

*Morfizm* - generalizacja funkcji.

Morfizm przeprowadza jeden obiekt na inny. Np. sin przeprowadza zbiór liczb R na zbiór R. Ale można także zdefiniować funkcję jestPierwsza która przeprowadza zbiór Z na Bool, albo funkcję cena która przeprowadza zbiór produktów na zbiów liczb.
Morfizmy można składać, tzn. jeśli A -> B oraz B -> C, to A -> C.
Ponadto dla każdego obiektu musi istnieć specjalny morfizm (identyczność), który przeprowadza ten obiekt na siebie.

*Kategoria* - zbiór obiektów i morfizmów.

##### Kategorie a Haskell
Hask to haskellowa kategoria typów i funkcji.
Mamy skończony zbiór typów podstawowych (Bool, Int) oraz nieskończony zbiór typów pochodnych ([Int], Int -> Bool, itd).
Typ w Hask to po prostu zbiór wartości. Np typ Char to zbiór {'a', 'b', ...}.
Zatem w Hask typy to obiekty, a funkcje to morfizmy, funkcja mapuje jeden typ na inny (funkcje wielu parametrów można opisać w ten sposób za pomocą currying'u).

**Esencją programowania jest możliwość składania programów z mniejszych części, a tych części z jeszcze mniejszych części.**

*Funktor* to mapa z jednej kategorii do innej; funktor przeprowadza obiekty w obiekty oraz morfizmy w morfizmy. Musi przy tym zachować następujące ograniczenia:

* morfizm między dwoma obiektami pierwszej kategorii musi być przemapowany na morfizm między odpowiadającymi im obiektami z drugiej kategorii. (jeśli mamy obiekt *A* mapowany na *F(A)* oraz *B* mapowany na *F(B)* i funkcję *f : A -> B*, to *f* jest mapowana na *F(f) : F(A) -> F(B))*
* morfizmy nadal powinny zachować swoje zasady składania: jeśli *h* jest złożeniem *f* i *g*, to *F(h)* musi być złożeniem *F(f)* i *F(g)*

##### Funktor w Hask
Zdefiniujmy funktor w Hask, który mapuje Hask na siebie (to jest endofunktor). Obiekt w Hask to typ, więc funktor mapuje typ na typ. To znaczy, że funktor w Hask tworzy jeden typ z drugiego- czyli jest *konstruktorem typu* (type constructor). Konstruktor typu tworzy nowy typ w programie z typu, który już istniał w Hask. Np. konstruktor listy: Int jest mapowany na [Int] (mapa nie jest zdefiniowana na wartościach). Nowy typ nie jest dodawany do Hask, bo lista już tam istniała.
Co z mapowaniem funkcji? To jest dokładnie funkcja *map* z haskella.

**Najważniejsza własność programowania to możliwość ponownego wykorzystania kodu. Mamy funkcję, która działa na Int, to z użyciem map mamy też za darmo funkcję, która działa na [Int].**

*Endofunktor* - funktor który przeprowadza kategorię na nią samą.

*Monada* - endofunktor i dwie rodziny morfizmów: unit i join.
Funktor mapuje obiekty (tutaj typy, czyli zbiory wartości). Funktor nie widzi co jest w środku w obiekcie- to wie morfizm. Morfizm to funkcja, która mapuje wartości jednego typu na wartości innego typu. Nasze funktory zwykle mapują uboższe typy na bogatsze.
Unit bierze wartość uboższego typu i przypisuje jej analogiczną wartość wyższego typu. Np. dla True to będzie [True], dla 5 to [5].
Join usuwa jedną warstwę list poprzez spłaszczenie. Np. dla [[a,b], [c], [d,e]] zwróci [a,b,c,d,e].

Ten przykład opisywał monadę listy.

## Problemy programowania fukncyjnego
Funkcja zawsze zwraca te same wyniki dla wywowałania z takimi samymi argumentami.
Często nie da się w ten sposób opisać zadań programistycznych- powinniśmy mówić o nich raczej jako o procedurach z efektami ubocznymi (np. funkcje rzucające wyjątki, interaktywny input typu getchar, funkcje które dla pewnych argumentów wchodzą w nieskończoną pętlę).

### Propagacja błędów, wyjątki
Funkcje czasem zwracają poprawny rezultat tylko dla pewnego podzbioru wszystkich możliwych argumentów. Jak sobie z tym radzić?
W językach typu C można zwracać -1 czy null, ale to tylko hacki.
W Haskellu można użyć Maybe. To pozwala na zwrócenie nowego, bogatszego typu z funkcji, która nie jest określona na całej dziedzinie. Ale zmienia sposób łączenia funkcji- inne funkcje teraz muszą obsługiwać Maybe. Powstaje kod składający się z kaskady instrukcji warunkowych. Jeśli dana instukcja nie jest spełniona, to kod w całej dalszej gałęzi jest omijany. Można go umieścić wewnątrz funkcji bind:

```haskell
  bind m f =
    case m of
      Nothing -> Nothing
      Just n -> f n
```

W Haskellu bind jest reprezentowane przez infixowy operator >>= następująco:

```haskell
  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  Nothing >>= cont = Nothing
  (Just v) >>= cont = cont v
```

Słowo kluczowe return w Haskellu nie zwraca wyniku z funkcji, tylko opakowuje go w bogatszy typ: tutaj dla funkcji zwracającej a, return da Maybe a.

### Monada Maybe
Zaczęliśmy od funkcji, która nie była funkcją z definicji- nie była zdefiniowana dla części swoich argumentów. Znaleźliśmy cwaną metodę, żeby zamienić ją w funkcję poprzez wzbogacenie zwracanego przez nią typu (to jest konstruktor typu, nazwijmy go M).

```haskell
  f :: a -> M b
```

Aby zdefiniować endofukntor, potrzeba jeszcze mapowania morfizmów. Dla każdej funkcji potrzebujemy jej odpowiednika działającego na rozszerzonych typach. Mapowanie funkcji można opisać poprzez funkcję wyższego rzędu: fmap.

```haskell
  fmap :: (a -> b) -> (M a -> M b)
```

Zatem ogólna metoda bind musi w tym wypadku pobierać 2 argumenty: rezultat poprzeniej rozszerzonej funkcji oraz resztę obliczeń do wykonania.

```haskell
  bind :: M a -> (a -> M b) -> M b
```

Potrzebujemy jeszcze jednej funkcji, return:

```haskell
  return :: a -> M a
```

Używając bind i return można podnieść każdą funckję f w g:

```haskell
  f :: a -> b
  g :: M a -> M b

  g ma = ma >>= (return . f)
```

Takie mapowanie f w g to fmap. Mamy funktor. Potrzeba jeszcze unit i join.
Return to unit.

Join powinno mieć następującą sygnaturę:

```haskell
  join :: M (M a) -> M a
```

Join powinno w tym przypadku mapować Just (Just v) na (Just v), a cokolwiek innego na Nothing.
Jest jednak bardziej ogólna metoda definicji join z użyciem identyczności:

```haskell
  id :: M a -> M a

  join :: M (M a) -> M a
  join mmx = mmx >>= id
```

Zatem Maybe razem z bind i return to monada w znaczeniu teorii kategorii. Ponadto każda trójka składająca się z konstruktora typu oraz funkcji bind i return definiuje monadę.

### Cukier składniowy
Notacja "do".

```haskell
  compose n =
      f n >>= \n1 ->
          g n1 >>= \n2 ->
              h n2 >>= \n3 ->
                  return n3
```

jest równoważne:

```haskell
  compose n = do
      n1 <- f n
      n2 <- g n1
      n3 <- h n2
      return n3
```

Teraz można pokusić się o znalezienie podobnego konceptu w innych językach, np. C++:

```cpp
  int compose(Foo * n)
  {
      auto n1 = f(n);
      auto n2 = g(n1);
      auto n3 = h(n2);
      return n3;
  }

  ...

  try {
      compose(pFoo);
  }
  catch(...) {
      // error handling
  }
```
W C++ dostajemy identyczną funkcjonalność nie poprzez modyfikację zwracanego typu, ale poprzez rzucenie wyjątku!

### Monada listy - obliczenia niedeterministyczne
Monadę listy zdefiniowaliśmy przez funktor i funkcje unit i join.
Konstruktor typu dla funktora mapuje typ a w listę: [a], mapowanie funkcji realizowane jest za pomocą map poprzez aplikację funkcji do każdego elementu listy. Ponadto:

```haskell
  unit x = [x]
  join = concat
```

Co to ma wspólnego z obliczeniami niedeterministycznymi?
Wyobraźmy sobie parsowanie niedeterministycznej gramatyki. Możemy otrzymać wiele alternatywnych drzew dla tego samego wejścia. Takie obliczenia można symulować za pomocą funkcji, która zwraca listę możliwych rezultatów. Typ zwracany przez deterministyczny parser (drzewo) jest rozszerzony (lista drzew).

Aby połączyć ze sobą funkcje, które zwracają listy, użyjemy bind w następującej postaci:

```haskell
  bind :: [a] -> (a -> [b]) -> [b]
```
Bind bierze listę i kontynuację obliczeń, i tworzy nową listę. Jeśli zaaplikujemy kontynuację do każdego elementu wejściowej listy, dostaniemy [[b]]. Trzeba ją spłaszczyć.

```hs
  xs >>= cont = concat (map cont xs)
```

** ogólny przepis na monadę: **

* wziąć konstruktor typu z funktora i nazwać fmap
* zdefiniować bind jako:

  ```haskell
    bind x f = join ((fmap f) x)
  ```

* zdefiniować return jako unit

## Podsumowanie
Jest duża klasa problemów, które przeprowadzają wejście na wyjście w sposób niefunkcyjny. Część z nich można opisać jako funkcje o rozszerzonym wyjściu; rozszerzenie wyjścia można opisać jako konstruktor typu, który definiuje pierwszy komponent monady. Ponieważ obliczenia muszą być składalne, potrzebujemy metody składania rozszerzonych funkcji- to jest drugi komponent monady, bind. Musimy być w stanie tworzyć funkcje, które zwracają rozszerzony typ, i do tego potrzeba trzeciego elementu, return.
