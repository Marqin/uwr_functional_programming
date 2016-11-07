import Control.Monad
-- replicateM n x = sequence (replicate n x)

type Aut = ([Char], Int, Int -> Char -> Int, Int -> Bool)
-- alfabet; stan poczatkowy; f tże jesli f i c = j; to aut po przecz C bedac w
-- i zmieni na j; g, tze sprawdza czy i jest akceptujace

example_automate :: Aut
example_automate = (['a', 'b'], 0, \_ c -> if c == 'a' then 0 else 1, \i-> i == 1)

all_words :: [Char] -> [[Char]]
all_words alphabet = [1..] >>= (`replicateM` alphabet)
-- all_words alphabet = [ x:xs | xs <- "":all_words, x <- alphabet ]

aut_words :: Aut -> [([Char], Bool)]
aut_words (alphabet, s, f, g) = map (\word -> (word, g $ foldl f s word)) $ all_words alphabet

accepted_words :: Aut -> [[Char]]
accepted_words a = map fst $ filter snd $ aut_words a

test1 :: [([Char], Bool)]
test1 = take 10 $ aut_words example_automate

test2 :: [[Char]]
test2 = take 10 $ accepted_words example_automate
