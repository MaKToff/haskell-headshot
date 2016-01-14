module While where

import Control.Monad

infixl 7 :*:, :/:, :%:
infixl 6 :+:, :-:
infix  5 :=:, :/=:, :<:, :>:
infixl 4 :/\:
infixl 3 :\/:

data E = X String | -- переменная
         C Int    | -- константа
         E :+:  E | -- сложение
         E :-:  E | -- вычитание
         E :*:  E | -- умножение
         E :/:  E | -- частное
         E :%:  E | -- остаток
         E :=:  E | -- сравнение на "равно"
         E :/=: E | -- сравнение на "не равно"
         E :<:  E | -- сравнение на "меньше"
         E :>:  E | -- сравнение на "больше"
         E :/\: E | -- конъюнкция
         E :\/: E   -- дизъюнкция
 deriving Show

-- Примечание: операции сравнения возвращают 1 (истина) или 0 (ложь), 
-- логические связки определены, только если оба их аргумента --- 1 или 0.

infixr 1 :>>:
infix  2 ::=:

data S = SKIP          | -- пустой оператор (ничего не делает)

         String ::=: E | -- присваивание; "X" ::=: e вычисляет выражение e и 
                         -- присваивает его значение в переменную "X"

         READ String   | -- чтение из входного потока; READ "X" читает очередное 
                         -- значение из входного потока и записывает его в
                         -- переменную "X"; не определено, если входной поток пуст;
                         -- прочитанное значение удаляется из входного потока

         WRITE E       | -- WRITE e записывает значение выражения e в конец выходного 
                         -- потока

         S :>>: S      | -- s1 :>>: s2 последовательно исполняет сначала s1, потом s2

         IF E S S      | -- IF e s1 s2 вычисляет значение e; если это значение равно 1,
                         -- то исполняется s1, если 0 --- то s2, иначе всё неопределено

         WHILE E S       -- WHILE e s повторяет s, пока значение e равно 1; если значение
                         -- e равно 0, то всё, если ни 0, ни 1 --- всё не определено
  deriving Show

-- Пример: факториал
fact = READ "n" :>>:
       "f" ::=: C 1 :>>:
       WHILE (X "n" :>: C 0) 
         ("f" ::=: X "f" :*: X "n" :>>:
          "n" ::=: X "n" :-: C 1 
         ) :>>:
       WRITE (X "f")

prog = WRITE (C 5 :/: C 0)

-- Написать интерпретатор int, который получает программу и
-- входной поток в виде списка целых, и возвращает результат: либо 
-- сообщение об ошибке, либо выходной поток в виде списка целых.
-- Например: 
--  int fact [5] => Right 120
--  int fact []  => Left "empty input"
int :: S -> [Int] -> Either String [Int] 
int program input = do
    (_, _, output) <- make [] input [] program
    return $ reverse output

-- Выполняет команду языка
make var inp out SKIP = return (var, inp, out)
make var inp out (s ::=: v) = do
    temp <- eval var v
    return ((s, temp) : var, inp, out)
make var [] out (READ s) = Left "empty input"
make var (x:xs) out (READ s) = return ((s,x) : var, xs, out)
make var inp out (WRITE v) = do
    temp <- eval var v
    return (var, inp, temp : out)
make var inp out (s1 :>>: s2) = do
    (var', inp', out') <- make var inp out s1
    make var' inp' out' s2
make var inp out (IF e s1 s2) = do
    temp <- eval var e
    case temp of
        1         -> make var inp out s1
        0         -> make var inp out s2
        otherwise -> undefined
make var inp out (WHILE e s) = do
    temp <- eval var e
    case temp of
        1         -> make var inp out (s :>>: WHILE e s)
        0         -> return (var, inp, out)
        otherwise -> undefined

-- Вычисляет значение выражения
eval var (X s) = find s var
eval var (C v) = return v
eval var (l :+: r) = do
    a <- eval var l
    b <- eval var r
    return $ a + b
eval var (l :-: r) = do 
    a <- eval var l
    b <- eval var r
    return $ a - b
eval var (l :*: r) = do
    a <- eval var l
    b <- eval var r
    return $ a * b
eval var (l :/: r) = do 
    a <- eval var l
    b <- eval var r
    if (b == 0) then Left "Division by zero" else return $ a `div` b
eval var (l :%: r) = do 
    a <- eval var l
    b <- eval var r
    if (b == 0) then Left "Division by zero" else return $ a `rem` b
eval var (l :=: r) = do
    a <- eval var l
    b <- eval var r
    return $ boolToInt $ a == b
eval var (l :/=: r) = do 
    a <- eval var l
    b <- eval var r
    return $ boolToInt $ a /= b
eval var (l :<: r) = do 
    a <- eval var l
    b <- eval var r
    return $ boolToInt $ a < b
eval var (l :>: r) = do 
    a <- eval var l
    b <- eval var r
    return $ boolToInt $ a > b
eval var (l :/\: r) = do 
    a <- eval var l
    b <- eval var r
    return $ (a `rem` 2) * (b `rem` 2)
eval var (l :\/: r) = do 
    a <- eval var l
    b <- eval var r
    return $ if ((a `rem` 2) + (b `rem` 2) > 0) then 1 else 0

find x [] = Left $ "Undefined variable " ++ x
find x ((s,v):xs) = if x == s then Right v else find x xs

boolToInt expr = if expr then 1 else 0

-- Написать на While проверку простоты числа isPrime. Например,
--   int isPrime [5] => Right 1
--   int isPrime [8] => Right 0
isPrime =
    READ "x" :>>:
    "temp" ::=: C 2 :>>:
    "result" ::=: C 1 :>>:
    WHILE (X "temp" :<: X "x") (
        IF (X "x" :%: X "temp" :=: C 0) ("result" ::=: C 0) SKIP :>>:
        "temp" ::=: (X "temp" :+: C 1)
    ) :>>:
    WRITE (X "result")