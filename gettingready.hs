--Prve Vezbe

uklanjaPoslednji :: [a] -> [a]
uklanjaPoslednji [] = []
uklanjaPoslednji [x] = []
uklanjaPoslednji (x : xs) = x : uklanjaPoslednji(xs)

uklanjaPretposlednji :: [a] -> [a]
uklanjaPretposlednji [] = []
uklanjaPretposlednji [x] = [x]
uklanjaPretposlednji [x, y] = [y]
uklanjaPretposlednji (x : xs) = x : uklanjaPretposlednji(xs)

fakt :: Int -> Int
fakt 0 = 1
fakt n = n * fakt (n - 1)

fakt' :: Int -> Int
fakt' n = faktUtil n 1
    where
        faktUtil :: Int -> Int -> Int
        faktUtil n a
            | n == 0 = a
            | otherwise = faktUtil (n - 1) (a * n)

imaVelikaSlova :: String -> Bool
imaVelikaSlova [] = False
imaVelikaSlova (x : xs) 
    | x <= 'Z' && x >= 'A' = True
    | otherwise = imaVelikaSlova (xs)

spljosti :: [Int] -> [Int]
spljosti [] = []
spljosti (x : xs) = x : (spljostiUtil xs x)
    where 
        spljostiUtil :: [Int] -> Int -> [Int]
        spljostiUtil [] _ = []
        spljostiUtil (x : xs) a
            | x == a = spljostiUtil (xs) a
            | otherwise = x : spljostiUtil (xs) x

spljosti2 :: [Int] -> [Int]
spljosti2 [] = []
spljosti2 [x] = [x]
spljosti2 (x : y : xs) 
    | x == y = spljosti2 (y : xs)
    | otherwise = x : spljosti2 (y : xs)

filterNemaVelikaSlova :: [String] -> [String]
filterNemaVelikaSlova [] = []
filterNemaVelikaSlova (x : xs)
    | imaVelikaSlova x == False = filterNemaVelikaSlova (xs)
    | otherwise = x : filterNemaVelikaSlova (xs)

kvadrirajElemente :: [Int] -> [Int]
kvadrirajElemente a = [x * x | x <- a]

jeDeljiv :: Int -> Int -> Bool
jeDeljiv 0 _ = False
jeDeljiv a b = mod b a == 0

jeDeljivSa3 :: Int -> Bool
jeDeljivSa3 a = jeDeljiv 3 a

filter' :: (a -> Bool) -> [a] -> [a]
filter' f l = [x | x <- l, f x == True]

deljiviSa3 :: [Int] -> [Int]
deljiviSa3 l = filter' jeDeljivSa3 l

-- Prvi Domaci

ukloniSvakiK :: [a] -> Int -> [a]
ukloniSvakiK l n = ukloniSvakiKUtil l n 1
    where
        ukloniSvakiKUtil :: [a] -> Int -> Int -> [a]
        ukloniSvakiKUtil [] _ _ = []
        ukloniSvakiKUtil (x : xs) n cur
            | cur == n = ukloniSvakiKUtil xs n 1
            | otherwise = x : ukloniSvakiKUtil xs n (cur + 1)

sviDelioci :: Int -> [Int]
sviDelioci x 
    | x <= 0 = []
    | otherwise = [y | y <- [1 .. x], mod x y == 0]

sumira :: [Int] -> Int
sumira [] = 0
sumira (x : xs) = x + sumira xs

zip' :: [Int] -> [Int] -> [Int]
zip' a [] = a
zip' [] a = a
zip' (x : xs) (y : ys) = (x + y) : zip' xs ys

sumirajCifre :: Int -> Int
sumirajCifre x
    | x <= 0 = 0
    | otherwise = mod x 10 + sumirajCifre (div x 10)

sumirajCifreRepno :: Int -> Int
sumirajCifreRepno x = sumirajCifreUtil x 0
    where
        sumirajCifreUtil :: Int -> Int -> Int
        sumirajCifreUtil x s
            | x <= 0 = s
            |otherwise = sumirajCifreUtil (div x 10) (s + mod x 10)

sumirajParneCifre :: Int -> Int
sumirajParneCifre x
    | x <= 0 = 0
    | mod cifra 2 == 0 = cifra + sumirajParneCifre (div x 10)
    | otherwise = sumirajParneCifre (div x 10)
        where 
            cifra = mod x 10

--Druge Vezbe

ispeglaj :: [[Int]] -> [Int]

ispeglaj l = foldr (\x y -> x ++ y) [] l

listaSuma :: [[Int]] -> [Int]
listaSuma l = map (\x -> foldr (+) 0 x) l

listaSuma2 :: [[Int]] -> [Int]
listaSuma2 [] = []
listaSuma2 (x : xs) = suma x : listaSuma2 xs
    where
        suma :: [Int] -> Int
        suma x = foldr (+) 0 x

filtrirajParne :: [[Int]] -> [[Int]]
filtrirajParne [] = []
filtrirajParne (x : xs)
    | length after == 0 = filtrirajParne xs
    | otherwise = after : filtrirajParne xs
    where
        after = filter odd x

okreni :: [String] -> [String]
okreni l = map reverse l

filtrirajDeljiveSa3 :: [[Int]] -> [[Int]]
filtrirajDeljiveSa3 l = map (\x -> filter (\y -> mod y 3 /= 0) x) l

filtrirajDeljiveSa3Harder :: [[Int]] -> [[Int]]
filtrirajDeljiveSa3Harder l = filter (\x -> length x >= 5) (filtrirajDeljiveSa3 l)

--Drugi Domaci

parnostListe :: [Int] -> [Int]
parnostListe l 
    | even (length l) = map (\x -> x * x) l
    | otherwise = map (*10) l 

toUpper :: Char -> Char
toUpper c
    | c < 'a' || c > 'z' = c
    | otherwise = toEnum (fromEnum c - fromEnum 'a' + fromEnum 'A')

izbaciVelikaSteroid :: [Char] -> [Char]
izbaciVelikaSteroid l = map toUpper (filter (\x -> x >= 'a' && x <= 'z') l)

primeni :: ([[Int]] -> [[Int]]) -> ([[Int]] -> [Int]) -> [[Int]] -> [Int]
primeni f1 f2 l = f2 (f1 l)

izbaciParneSumiraj :: [[Int]] -> [Int]
izbaciParneSumiraj l = primeni filtrirajParne listaSuma l

prosecnaDuzina :: [String] -> Double
prosecnaDuzina [] = 0
prosecnaDuzina l = fromIntegral (sum (map length l)) / fromIntegral (length l)

--Trece vezbe

splitString :: Char -> String -> [String]
splitString a l = splitStringUtil a l []

splitStringUtil :: Char -> String -> String -> [String]
splitStringUtil _ [] x = [x]
splitStringUtil a (x : xs) cur 
    | x == a = cur : splitStringUtil a xs []
    | otherwise = splitStringUtil a xs (cur ++ [x])

spojiStringove :: [String] -> String
spojiStringove [] = []
spojiStringove (x : xs) = foldl (\x y -> x ++ "," ++ y) x xs

razdvojiPaSpoji :: [String] -> String
razdvojiPaSpoji l = spojiStringove (foldl (++) [] (map (splitString ' ') l))

matricaInt :: Num a => [[a]] -> a
matricaInt l = product l2
    where 
        kvadrati = (map (\x -> map (^2) x) l)
        l2 = map sum kvadrati

radnici :: [String] -> [Int] -> [(String, Int)]

radnici l1 l2 
    | length l1 /= length l2 = []
    | otherwise = filtrirani
    where
        spojeni = zip l1 l2
        filtrirani = filter (\x -> even (snd x)) spojeni

type Tacka = (Int, Int)
geometrija :: Tacka -> [Tacka] -> Double -> [Tacka]
geometrija a l d = filter (\x -> fromIntegral ((fst a - fst x) ^ 2 + (snd a - snd x) ^ 2) <= d ^ 2) l

--Cetvrte vezbe

data Lista e = Empty
    | Cvor e (Lista e)
    deriving Show

kreirajListu :: [Int] -> Lista Int
kreirajListu [] = Empty
kreirajListu (x : xs) = Cvor x (kreirajListu xs)

duzinaListe :: Lista a -> Int
duzinaListe Empty = 0
duzinaListe (Cvor a b) = 1 + duzinaListe b

a = kreirajListu [1, 2, 3]

uListi :: Eq a => Lista a -> a -> Bool
uListi Empty _ = False
uListi (Cvor a b) x = a == x || (uListi b x)

data Planeta = Prazna
    | Planeta {
        ime :: String, 
        precnik :: Double, 
        gasovita :: Bool
    }
    deriving Show

type Planete = [Planeta]

b = [Planeta {ime = "Vanja", precnik = 2.5, gasovita = True}, Planeta {ime = "Oliverko", precnik = 5.2, gasovita = True}, Prazna]

nadjiPoImenu :: String -> Planete -> Planeta
nadjiPoImenu _ [] = Prazna
nadjiPoImenu s (Prazna : xs) = nadjiPoImenu s xs
nadjiPoImenu s (x : xs) 
    |ime x == s = x
    |otherwise = nadjiPoImenu s xs  

vratiGasovite :: Planete -> Planete

vratiGasovite [] = []
vratiGasovite (Prazna : xs) = vratiGasovite xs
vratiGasovite (x : xs) 
    | gasovita x = x : vratiGasovite xs
    | otherwise = vratiGasovite xs

--Cetvrti domaci

data Stablo e = Nista
    | Node (Stablo e) e (Stablo e)
    deriving Show

stablo :: Stablo Int
stablo = Node (Node Nista 3 (Node Nista 10 Nista)) 5 (Node Nista 2 (Node Nista 0 Nista))

sadrzi :: Eq e => e -> Stablo e -> Bool
sadrzi _ Nista = False
sadrzi x (Node a b c) =  x == b || (sadrzi x a) || (sadrzi x c)

uListu :: Stablo e -> [e]

uListu Nista = []
uListu (Node a b c) = uListu a ++ [b] ++ uListu c

stabloSa3Ili5 :: Stablo Int -> [Int]

stabloSa3Ili5 Nista = []
stabloSa3Ili5 (Node a b c) 
    | t = (stabloSa3Ili5 a) ++ [b] ++ (stabloSa3Ili5 c)
    | otherwise = (stabloSa3Ili5 a) ++ (stabloSa3Ili5 c)
    where
        t = mod b 3 == 0 || mod b 5 == 0

preslikaj :: Stablo e -> Stablo e
preslikaj Nista = Nista
preslikaj (Node a b c) = Node (preslikaj c) b (preslikaj a)

preslikano :: Stablo Int
preslikano = preslikaj stablo

filterStablo :: (e -> Bool) -> Stablo e -> [e]
filterStablo f Nista = []
filterStablo f (Node a b c) 
    | f b = (filterStablo f a) ++ [b] ++ (filterStablo f c)
    | otherwise = (filterStablo f a) ++ (filterStablo f c)