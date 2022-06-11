-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

-- ==================================================

module Tasks where

import Dataset
import Data.List
import Text.Printf
import Data.Array

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]


-- Prerequisities
split_by :: Char -> String -> [String]
split_by x = foldr op [""]
  where op char acc
            | char == x = "":acc
            | otherwise = (char:head(acc)):tail(acc)

read_csv :: CSV -> Table
read_csv = (map (split_by ',')) . (split_by '\n')

write_csv :: Table -> CSV
write_csv = (foldr (++) []).
            (intersperse "\n").
            (map (foldr (++) [])).
            (map (intersperse ","))

-- Functie de afisat numere
floatToStr :: Float -> String
floatToStr = printf "%.2f"

-- Transformare de la Int la Float
intToFloat :: Int -> Float
intToFloat n = fromInteger (toInteger n)

toFloat :: Value -> Float
toFloat x = read x :: Float

toInt :: Value -> Int
toInt x = read x :: Int

{-
    TASK SET 1
-}


-- Task 1
compute_average_steps :: Table -> Table
compute_average_steps [] = []
compute_average_steps (("Name":value):xs) = ("Name":["Average Number of Steps"]) : compute_average_steps xs
compute_average_steps ((name:values):xs) = (name : [get_avg values]) : compute_average_steps xs
compute_average_steps _ = []

get_avg :: [Value] -> Value
get_avg x = floatToStr (foldr ((+).toFloat) 0 x/8)

-- Task 2

-- Number of people who have achieved their goal:
get_passed_people_num :: Table -> Int
get_passed_people_num x = calc_passed_people x 0

calc_passed_people :: Table -> Int -> Int
calc_passed_people [] nr = nr
calc_passed_people ((name:values):xs) nr
  | (foldr ((+).toFloat) 0 values/1) >= 1000 = calc_passed_people xs (nr+1)
  | otherwise  = calc_passed_people xs nr
calc_passed_people _ nr = nr


-- Percentage of people who have achieved their:
get_passed_people_percentage :: Table -> Float
get_passed_people_percentage xs = intToFloat (get_passed_people_num xs) / intToFloat (length xs-1)


-- Average number of daily steps
get_steps_avg :: Table -> Float
get_steps_avg xs = calc_avg_all xs 0

calc_avg_all :: Table -> Float -> Float 
calc_avg_all [] nr = nr/132
calc_avg_all (("Name":values):xs) nr = calc_avg_all xs nr
calc_avg_all ((name:values):xs) nr = calc_avg_all xs (calc_sum values + nr)
calc_avg_all _ nr = nr

calc_sum :: Row -> Float 
calc_sum xs =  (foldr ((+).toFloat) 0 xs/1)

-- Task 3

get_avg_steps_per_h :: Table -> Table
get_avg_steps_per_h [] = []
get_avg_steps_per_h ([]:xs) = get_avg_steps_per_h xs
get_avg_steps_per_h (("Name":value):xs) = ((["H10","H11","H12","H13","H14","H15","H16","H17"]): (get_avg_column_per_h (transpose_table xs)))
get_avg_steps_per_h _ = []

get_avg_column_per_h :: Table -> Table
get_avg_column_per_h (("Olivia Noah":names):xs) = get_avg_column_per_h xs
get_avg_column_per_h xs = formTable [] (calcAvgSteps (xs))
  where
      formTable :: Row -> Table -> Table
      formTable row [] = [row]
      formTable row ((x:value):xs) = formTable (row ++ [x]) xs
      formTable row xs = formTable row xs

transpose_table :: Table -> Table
transpose_table ([]:_) = []
transpose_table m = (map head m):(transpose_table (map tail m))

calcAvgSteps :: Table -> Table
calcAvgSteps [] = []
calcAvgSteps ([]:xs) = calcAvgSteps xs
calcAvgSteps (value:xs) = ([sumAvg value]):(calcAvgSteps xs)

sumAvg :: [Value] -> Value
sumAvg x = printf "%.2f" (foldr ((+) . toFloat) 0 x/132)


-- Task 4

prima_linie = ["column", "range1", "range2", "range3"]

get_activ_summary :: Table -> Table
get_activ_summary [] = []
get_activ_summary (("Name":value):xs) = ["column","range1","range2","range3"] : formTable [["VeryActiveMinutes"],["FairlyActiveMinutes"],["LightlyActiveMinutes"]] (tail(tail(tail(transpose xs))))
get_activ_summary (x:xs) = xs

formTable :: Table -> Table -> Table
formTable (["VeryActiveMinutes"]:xs) (values:ys) = ("VeryActiveMinutes" : very_active values 0 0 0) : formTable xs ys
formTable (["FairlyActiveMinutes"]:xs) (values:ys) = ("FairlyActiveMinutes" : very_active values 0 0 0) : formTable xs ys
formTable (["LightlyActiveMinutes"]:xs) (values:ys) = ("LightlyActiveMinutes" : very_active values 0 0 0) : formTable xs ys
formTable _ _ = []

very_active :: Row -> Int -> Int -> Int -> Row
very_active [] a b c = append_values a b c
very_active (x:xs) a b c
  | ((toInt x) < 50) = very_active xs (a+1) b c
  | ((toInt x) < 100) = very_active xs a (b+1) c
  | ((toInt x) < 500) = very_active xs a b (c+1)
very_active _ _ _ _ = []

append_values :: Int -> Int -> Int -> Row
append_values a b c = [(show a), (show b), (show c)]

-- Task 5

get_ranking :: Table -> Table
get_ranking [] = []
get_ranking (("Name":value):xs) = ["Name", "Total Steps"] : bubbleSort (sort_table1 (map init (map init (map init (map init xs)))))
get_ranking (x:xs) = []

sort_table1 :: Table -> Table
sort_table1 [] = []
sort_table1 xs = x : sort_table1 (delete x xs)
  where x = minimum' xs

minimum' :: Table -> Row
minimum' ([a,b]:xs) = foldl' (\ [a,b] [c,d] -> if (toInt b) <= (toInt d) then [a,b] else [c,d]) [a,b] xs
minimum' _ = []

bubbleSortImpl :: Int -> Table -> Table
bubbleSortImpl 0 xs = xs
bubbleSortImpl n xs = bubbleSortImpl (n - 1) (bubble xs)
  where
    bubble :: Table -> Table
    bubble [] = []
    bubble ([a,b] : []) = [a,b] : []
    bubble ([a,b] : [c,d] : ys) = if a <= c && b == d
    then [a,b] : (bubble ([c,d] : ys))
    else [c,d] : (bubble ([a,b] : ys))
    bubble _ = []
bubbleSort :: Table -> Table
bubbleSort xs = let n = length xs
  in bubbleSortImpl n xs

-- Task 6

get_steps_diff_table :: Table -> Table
get_steps_diff_table [] = []
get_steps_diff_table (("Name":value):xs) = ["Name","Average first 4h","Average last 4h","Difference"]:bubbleSort' (sort_by_difference(get_difference (get_average xs)))
get_steps_diff_table (x:xs) = []

get_average :: Table -> Table
get_average [] = []
get_average ((name:a:b:c:d:e:f:g:h:ys):xs) = [name,calc_average ([a,b,c,d]),calc_average ([e,f,g,h])]:get_average xs
get_average _ = []

calc_average :: Row -> Value
calc_average (a:b:c:d:xs) = floatToStr (((toFloat a) + (toFloat b) + (toFloat c) + (toFloat d))/4)
calc_average _ = []

get_difference :: Table -> Table
get_difference ((name:x:y:ys):xs) = [name,x,y,calc_difference x y] : get_difference xs
get_difference _ = []

calc_difference :: Value -> Value -> Value
calc_difference x y 
  | (toFloat x) >= (toFloat y) = floatToStr((toFloat x) - (toFloat y))
  | (toFloat x) < (toFloat y) = floatToStr((toFloat y) - (toFloat x))
calc_difference _ _ = []

sort_by_difference :: Table -> Table
sort_by_difference [] = []
sort_by_difference xs = x : sort_by_difference (delete x xs)
  where x = minimum'' xs

minimum'' :: Table -> Row
minimum'' ([name,a,b,c]:xs) = foldl' (\ [name,a,b,c] [name2,d,e,f] -> if (toFloat c) <= (toFloat f) then [name,a,b,c] else [name2,d,e,f]) [name,a,b,c] xs
minimum'' _ = []

bubbleSortImpl' :: Int -> Table -> Table
bubbleSortImpl' 0 xs = xs
bubbleSortImpl' n xs = bubbleSortImpl' (n - 1) (bubble xs)
  where
    bubble :: Table -> Table
    bubble [] = []
    bubble ([name,a,b,c] : []) = [name,a,b,c] : []
    bubble ([name,a,b,c] : [name2,d,e,f] : ys) = if name <= name2 && toFloat(c) == toFloat(f)
    then [name,a,b,c] : (bubble ([name2,d,e,f] : ys))
    else [name2,d,e,f] : (bubble ([name,a,b,c] : ys))
    bubble _ = []
bubbleSort' :: Table -> Table
bubbleSort' xs = let n = length xs
  in bubbleSortImpl' n xs



-- Task 7

-- Applies the given function to all the values
vmap :: (Value -> Value) -> Table -> Table
vmap f = map (map f)


-- Task 8

-- Applies the given function to all the entries
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap get_sleep_total row xs = map get_sleep_total xs


get_sleep_total :: Row -> Row
get_sleep_total (name:xs) = [name,floatToStr((foldr ((+) . toFloat) 0 xs/1))]
get_sleep_total _ = []