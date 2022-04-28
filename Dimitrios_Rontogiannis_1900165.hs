import Data.List

-- Useful functions

my_max a b = if (a < b) then b else a
my_maximum [x] = x
my_maximum (x:xs) = my_max x (my_maximum xs)
my_group [] = []
my_group (x:xs) = groupx [x] x xs 
groupx r x [] = [r]
groupx r x (y:ys) = if (y == x) then (groupx (x:r) x ys) else (r:(groupx [y] y ys))
my_map f [] = []
my_map f (x:xs) = (f x):(my_map f xs)
my_unique x = my_map head (my_group (sort x))
my_member x [] = False
my_member x (y:xs) = if(x == y) then True else (my_member x xs)
 
sortGT (a1, b1) (a2, b2)
        | (a1 == a2) = compare b1 b2
        | (a1 < a2)  = LT
        | otherwise  = GT

-- Problem 1 - Champions

champions x = solve_champ x (length x) (my_maximum (add_val (sort x) (length x)))
add_val [] n = []
add_val (x:xs) n = (x + n):(add_val xs (n - 1))
solve_champ [] n m = 0
solve_champ (x:xs) n m
        | (x + n) < m = solve_champ xs n m
        | otherwise   = ((solve_champ xs n m) + 1)

-- Problem 2 - Beautiful

beautiful x = solve_blists x
solve_blists [] = 0
solve_blists (x:xs) = (solve_blists xs) + (solve_b x [])
solve_b [] [] = 1
solve_b [] (x:xs) = 0
solve_b (x:xs) [] = solve_b xs [x]
solve_b (x:xs) (y:ys)
        | (x == y)  = solve_b xs ys
        | otherwise = solve_b xs (x:(y:ys))

-- Problem 3 - Changes

changes (x:xs) = solve_changes xs (if (x == 'a') then 0 else 1) (if (x == 'b') then 0 else 1)
solve_changes [] n m = n
solve_changes (x:xs) n m 
        | (x == 'a') = solve_changes xs n (min (n + 1) (m + 1))
        | otherwise  = solve_changes xs (min (n + 1) (m + 1)) m
 
-- Problem 4 - Alphabet

alphabet x = solve_alpha t d c o
        where
             (t, d1, c, o) = (count_alphabet x)
             d2 = (floyd t t t d1)
             d = (my_unique d2) 
count_alphabet x = (t, d, c, o)
        where
             t = my_unique (concat x) 
             c = length(t)
             (d, o) = fix_order x
fix_order [x] = ([], 1)
fix_order (x:(y:xs))
        | (o == 0)  = ([], 0)
        | (d == []) = (d1, c)
        | otherwise = ((d ++ d1), c)
        where
             (d1, c) = fix_order (y:xs)
             (d, o)  = forder x y
forder [] x = ([], 1)
forder (x:xs) [] = ([], 0)
forder (x:xs) (y:ys)
        | (x == y)  = forder xs ys
        | otherwise = ([(x, y)], 1)  
floyd [] y z d = d
floyd (x:xs) y z d = floyd xs y z (my_unique ((floyd_K x y z d) ++ d))
floyd_K x [] z d = []
floyd_K x (y:ys) z d = ((floyd_KI x y z d) ++ (floyd_K x ys z d))
floyd_KI x y [] d = []
floyd_KI x y (z:zs) d = ((transit x y z d) ++ (floyd_KI x y zs d))
member (x, y) [] = False
member (x, y) ((a, b):zs) = if (x == a && y == b) then True else (member (x, y) zs) 
transit x y z d
        | ((member (y, x) d) && (member (x, z) d)) = [(y, z)]
        | otherwise                                = []
solve_alpha t d c o
        | (o == 0 || (exists_dub d)) = "0"
        | (check_many t d c)         = "1"
        | otherwise                  = transf (sort (build_array t d c))
exists_dub [] = False
exists_dub ((a, b):xs) = if (a == b) then True else (exists_dub xs)
check_many [] d c = False
check_many (x:xs) d c
        | ((a + b) /= (c - 1)) = True
        | otherwise            = (check_many xs d c)
        where
             (a, b) = (count_degrees x d)
count_degrees x [] = (0, 0)
count_degrees x ((a, b):xs)
        | (x == b)  = (c1, c2 + 1)
        | (x == a)  = (c1 + 1, c2)
        | otherwise = (c1, c2)
        where
             (c1, c2) = (count_degrees x xs)
build_array [] d c = []
build_array (x:xs) d c = a
        where
             (c1, c2) = (count_degrees x d)
             a = ([(c2, x)] ++ (build_array xs d c))
transf [] = []
transf ((a, b):xs) = (b:(transf xs))

-- Problem 5 - Booksort

booksort x = solve_books (reverse x) (length x)
solve_books [] n = n
solve_books (x:xs) n = if (x == n) then (solve_books xs (n - 1)) else (solve_books xs n)

-- Problem 6 - Countsquares

countsquares x = count_sq ((0, 0):s)
        where
             (y, s) = (update (sortBy sortGT x))
count_sq [(a, b)] = 0
count_sq ((a, b):(a1, b1):s) = (a1 - a) * b1 + count_sq ((a1, b1):s)
update [(a, b)] = (b, [(a, b)])
update ((a, b):xs)
        | (b > y)   = (ny, ((a, b):s))
        | otherwise = (ny, s)
        where
             (y, s) = (update xs)
             ny = (my_max y b)
max_y [s] = [s]
max_y (s:ss) = ((my_max s (head ys)):ys)
        where
            ys = (max_y ss)

-- Problem 7 - Complete

complete n s
        | (2 * length(s) == n * (n - 1)) = "yes"
        | otherwise                      = solve_complete n (sortBy sortGT (add_dir s)) 
add_dir [] = []
add_dir ((a, b):s) = ((a, b):(b, a):(add_dir s))
make_list [] = []
make_list ((a, b):x) = (a:b:(make_list x))
count_nodes x = length (my_unique x)
solve_complete n [] = if (n <= 2 ) then "yes" else "no"
solve_complete n ((a, b):d)
        | ((count_nodes (make_list ((a, b):d))) < (n - 1)) = "no"
        | otherwise                                        = check_complete (make_graph ((a, b):d) a [])
make_graph [] x adjl = [(x, adjl)]
make_graph ((a, b):d) x adjl
        | (x == a)  = make_graph d x (b:adjl)
        | otherwise = ((x, adjl):(make_graph d a [b]))
check_complete ((x, adjl):graph) = my_check graph (sort(x:adjl)) (take_first_cond (sort(x:adjl)) graph)
take_first_cond x [] = []
take_first_cond x ((y, adjl):graph)
        | (x == ys) = (take_first_cond x graph)
        | otherwise = ys
        where
             ys = (sort (y:adjl))
my_check [] x y = "yes"
my_check ((a, adjl):graph) x y
        | (x == c || y == c) = my_check graph x y
        | otherwise          = "no"
        where
             c = (sort (a:adjl))
