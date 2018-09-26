{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances #-}

module GAS where

import ProblemState
import qualified Data.Map.Strict as M
import qualified Data.Maybe as My

{-
    Pozițiile tablei de joc, în formă (linie, coloană), unde ambele coordonate
    pot fi negative.
-}
type Position = (Int, Int)

{-
    Culorile pătratelor și cercurilor.
-}
data Color = Red | Blue | Gray
    deriving (Eq, Ord, Show)


{-
    Orientările pătratelor și săgeților.
-}
data Heading = North | South | East | West
    deriving (Eq, Ord)

instance Show Heading where
    show North = "^"
    show South = "v"
    show East  = ">"
    show West  = "<"

{-
    *** TODO ***

    Un obiect de pe tabla de joc: pătrat/ cerc/ săgeată.
-}
data Object = Square Color Heading | Circle Color | Arrow Heading 
    deriving (Eq, Ord)

{-
    *** TODO ***
    Reprezetarea textuală a unui obiect.
-}
instance Show Object where
    show (Square color heading)
        | color == Red = "R" ++ show heading
        | color == Blue = "B" ++ show heading
        | otherwise = "G" ++ show heading
    show (Circle color) 
        | color == Red = "r"
        | color == Blue = "b"
        | otherwise = "g"
    show (Arrow heading) = show heading
{-
    *** TODO ***

    Un nivel al jocului.

    Recomandăm Data.Map.Strict.
-}
data Level = ConsLevel (M.Map Position (Maybe Object, Maybe Object)) 
    deriving (Eq, Ord)

{-
    *** TODO ***

    Reprezetarea textuală a unui nivel.
-}

fromLevel :: Level -> M.Map Position (Maybe Object, Maybe Object)
fromLevel (ConsLevel map) = map

headingFromObj :: Object -> Heading
headingFromObj (Square _ h) = h
headingFromObj (Arrow h) = h
headingFromObj (Circle _) = North

colorFromObj :: Object -> Color
colorFromObj (Square c _) = c
colorFromObj (Circle c) = c
colorFromObj (Arrow _) = Red


{-
    Generez toate pozitiile din matricea minimizata, cea delimitata de obiecte
-}
twoDigitCombinations :: Int -> Int -> Int -> Int -> [(Int, Int)]
twoDigitCombinations minI maxI minJ maxJ = [(x, y) | x <- [minI..maxI], y <- [minJ..maxJ]]


maximY :: (Ord a, Ord b) => [(a,b)] -> b
maximY l = snd $ swap $ maximum $ map swap l where
        swap (x, y) = (y, x) 

minimY :: (Ord a, Ord b) => [(a,b)] -> b
minimY l = snd $ swap $ minimum $ map swap l where
        swap (x, y) = (y, x) 

maximX :: (Ord a, Ord b) => [(a,b)] -> a
maximX l = fst $ maximum $ map swap l where
        swap (x, y) = (x, y)

fromMaybe :: Maybe (Maybe Object, Maybe Object) -> (Maybe Object, Maybe Object)
fromMaybe (Just o ) = o
fromMaybe Nothing = (Nothing, Nothing)

{-
    Foldul parcurge pozitiile din matricea minimizata.
    
    Daca pozitia din matricea minimizata se afla si in nivel, atunci trebuie afisata;
    Se verifica pozitia (pe ultima linia, pe ultima coloana, in mijloc, ultimul element)
    pentru a sti daca afisam "\n", "|" sau nimic.
-}
instance Show Level where
    show (ConsLevel map) = 
        foldl (\acc (a,b) -> if M.member (a, b) map then
                                if b == maximY (M.keys map) && maximX (M.keys map) == a then 
                                    acc ++ 
                                    (case (fst (fromMaybe (M.lookup (a,b) map))) of
                                        (Just o) -> show o
                                        _ -> "  ")
                                    ++  
                                    (case (snd (fromMaybe (M.lookup (a,b) map))) of
                                        (Just o) -> show o
                                        _ -> " ")

                                else if b == maximY (M.keys map) && a < maximX (M.keys map)  then 
                                      acc ++ 
                                      (case (fst (fromMaybe (M.lookup (a,b) map))) of
                                         (Just o) -> show o
                                         _ -> "  ")
                                      ++  
                                      (case (snd (fromMaybe (M.lookup (a,b) map))) of
                                         (Just o) -> show o
                                         _ -> " ")
                                      ++ "\n"
                                    else 
                                     acc ++
                                     (case (fst (fromMaybe (M.lookup (a,b) map))) of
                                        (Just o) -> show o
                                        _ -> "  ")
                                     ++ 
                                     (case (snd (fromMaybe (M.lookup (a,b) map))) of
                                        (Just o) -> show o
                                        _ -> " ")
                                     ++ "|"
                             else if a < (maximX (M.keys map)) && b == (maximY (M.keys map)) then
                                    acc ++ "   \n"
                                  else 
                                    if a <= (maximX (M.keys map)) && b < (maximY (M.keys map)) then
                                        acc ++ "   |"
                                    else
                                        acc ++ "   ")
        "" (twoDigitCombinations (fst(minimum (M.keys map))) (fst(maximum(M.keys map))) (minimY (M.keys map)) (maximY (M.keys map)))

{-
    *** TODO ***
    Nivelul vid, fără obiecte. " r" "  r"

-}

emptyLevel :: Level
emptyLevel = ConsLevel M.empty

{-
    *** TODO ***

    Adaugă un pătrat cu caracteristicile date la poziția precizată din nivel.
-}

addSquare :: Color -> Heading -> Position -> Level -> Level
addSquare color heading position level = if M.member position (fromLevel level) then 
                                            ConsLevel $ M.update f position (fromLevel level) 
                                         else
                                            ConsLevel $ M.insert position (Just(Square color heading), Nothing) (fromLevel level) where 
        f (_, val2) = Just (Just(Square color heading), val2)

{-
    *** TODO ***

    Adaugă un cerc cu caracteristicile date la poziția precizată din nivel.
-}
addCircle :: Color -> Position -> Level -> Level
addCircle color position level = if M.member position (fromLevel level) then 
                                        ConsLevel $ M.update f position (fromLevel level)
                                   else
                                        ConsLevel $ M.insert position (Nothing, Just(Circle color)) (fromLevel level) where 
        f(val1,_) = Just(val1, Just(Circle color))

{-
    *** TODO ***

    Adaugă o săgeată cu caracteristicile date la poziția precizată din nivel.
-}
addArrow :: Heading -> Position -> Level -> Level
addArrow heading position level = if M.member position (fromLevel level) then 
                                        ConsLevel $ M.update f position (fromLevel level)
                                   else
                                        ConsLevel $ M.insert position (Nothing, Just(Arrow heading)) (fromLevel level) where 
        f(val1,_) = Just(val1, Just(Arrow heading))
        
        
{-
    Auxiliar pentru adaugarea unui obiect peste altul (cerc sau sageata).
-}        
addOver :: (Maybe Object, Maybe Object)  -> Position -> Level -> (Maybe Object, Maybe Object)
addOver obiect position level = (case secund of
                                 Just(Circle _) -> (fst obiect, secund)
                                 Just(Arrow heading) -> (Just(Square (colorFromObj (My.fromJust (fst obiect))) heading), secund)
                                 Nothing -> (fst obiect, Nothing)
                                 _ -> (fst obiect, secund))    
                                where secund = snd $ fromMaybe $ M.lookup position $ fromLevel level

{-
    Auxiliar pentru stergerea unui patrat atunci cand se pozitioneaza peste cerc/sageata.
-}
delSq :: M.Map Position (Maybe Object, Maybe Object) -> Position -> M.Map Position (Maybe Object, Maybe Object)
delSq map position = (case (snd obiect) of
                            Just _ -> M.update f position map
                            Nothing -> M.delete position map)
        
                      where obiect = fromMaybe $ M.lookup position map
                            f(_, val1) = Just(Nothing, val1)

{-
    *** TODO ***

    Mută pătratul de la poziția precizată din nivel. Dacă la poziția respectivă
    nu se găsește un pătrat, întoarce direct parametrul.
-}
move :: Position  -- Poziția
     -> Level     -- Nivelul inițial
     -> Level     -- Nivelul final
move position level = if not $ M.member position $ fromLevel level then
                         level
                      else
                         if My.isNothing $ fst $ fromMaybe obiect  then
                            level
                         else
                                (case heading of 
                                    North -> do
                                          case obiect of
                                               Nothing -> level
                                               Just e  -> ConsLevel $ M.insert pozimin (addOver e pozimin level) (delSq (fromLevel level) position)
                                    South -> do
                                          case obiect of
                                               Nothing -> level
                                               Just e  -> ConsLevel $ M.insert poziplus (addOver e poziplus level) (delSq (fromLevel level) position)
                                        
                                    East -> do
                                          case obiect of
                                               Nothing -> level
                                               Just e  -> ConsLevel $ M.insert pozjplus (addOver e pozjplus level) (delSq (fromLevel level) position)
                                    West -> do 
                                          case obiect of
                                               Nothing -> level
                                               Just e  -> ConsLevel $ M.insert pozjmin (addOver e pozjmin level) (delSq (fromLevel level) position))
                                               
                                    where heading = headingFromObj $ My.fromJust $ fst $ fromMaybe $ M.lookup position $ fromLevel level
                                          obiect = M.lookup position (fromLevel level)
                                          pozimin = ((fst position) - 1, snd position)
                                          poziplus = ((fst position) + 1, snd position)
                                          pozjplus = (fst position, (snd position) + 1)
                                          pozjmin = (fst position, (snd position) - 1)
                                                           
{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru.
-}
instance ProblemState Level Position where
    successors level = map (\position -> (position, move position level)) squares
                       where squares = filter (\position -> (case fst (fromMaybe (M.lookup position (fromLevel level))) of
                                                            Just (Square _ _) -> True
                                                            Just (Circle _) -> False
                                                            Just (Arrow _) -> False
                                                            Nothing -> False)) (M.keys (fromLevel level))
                                                             

    isGoal level = if length check == length squares then True
                   else False
                   where check = filter (\(a, b) -> (case a of
                                                     Just (Square color _) -> (case b of
                                                                             Just (Circle color2) -> color == color2
                                                                             Just (Arrow _) -> False
                                                                             Nothing -> False
                                                                             Just (Square _ _) -> False)
                                                     Nothing -> False
                                                     Just (Circle _) -> False
                                                     Just (Arrow _) -> False            
                                                                             )) (M.elems (fromLevel level))
                         squares = filter(\(a,_)-> (case a of
                                                    Just(Square color _) -> True
                                                    Just(Circle _) -> False
                                                    Just(Arrow _) -> False
                                                    Nothing -> False))  (M.elems (fromLevel level))

    -- Doar petru BONUS
    -- heuristic =
