module Graph where

import Data.List

-- График целочисленной функции f --- это список пар (x, y), такой, что 
-- f (x) = y <=> пара (x, y) входит в этот список. Гарантируется, что
-- в списке нет двух пар с одинаковыми первыми компонентами; никаких
-- предположений о порядке следования пар в списке не делается. 
-- Далее везде считается, что все графики конечны.
newtype Graph = G {unG :: [(Int, Int)]} deriving Show

-- fromFun f m n строит график функции f в области определения от
-- m до n включительно c шагом 1.
fromFun :: (Int -> Int) -> Int -> Int -> Graph
fromFun f m n = G [(x, f x) | x <- [m..n]]

-- toFun получает график и возвращает функцию. 
toFun :: Graph -> (Int -> Int)
toFun (G g) = (\x -> find x g) where
    find e ((x,fx):xs) = if e == x then fx else find e xs

-- Графики можно сравнивать на равенство
instance Eq Graph where
  (==) (G f) (G g) = sort f == sort g

-- Графики упорядочены по теоретико-множественному включению
instance Ord Graph where
  (<=) (G f) (G g) = and [elem x g | x <- f]

-- dom g возвращает область определения графика
dom :: Graph -> [Int]
dom = map fst . unG

-- compose g1 g2 возвращает график суперпозиции функций с графиками
-- g1 и g2 (сначала применяется g1, потом g2)
compose :: Graph -> Graph -> Graph
compose (G g1) g2 = let (f,l) = (toFun g2, map fst $ unG g2) in 
    G [(x, f y) | (x, y) <- g1, elem y l]

-- restrict g l строит сужение графика g на l. Не предполагается,
-- что l --- подмножество dom g.
restrict :: Graph -> [Int] -> Graph
restrict (G g) l = G [x | x <- g, elem (fst x) l]

-- isIncreasing g == True <=> g --- график (нестрого) возрастающей функции
isIncreasing :: Graph -> Bool
isIncreasing (G g) = let l = map snd $ sort g in l == sort l

-- isInjective g == True <=> g --- график инъективной функции
isInjective :: Graph -> Bool
isInjective (G g) = let l = map snd g in length (nub l) == length l

-- areMutuallyInverse g1 g2 == True <=> g1 и g2 --- графики взаимно-обратных
-- функций
areMutuallyInverse :: Graph -> Graph -> Bool
areMutuallyInverse g1 (G g2) = g1 == G (map swap g2) where swap (x,y) = (y,x)