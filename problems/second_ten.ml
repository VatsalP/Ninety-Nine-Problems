(** Functions for the second set of ten problems *)

exception Invalid_argument of string

type 'a rle = One of 'a | Many of int * 'a

(** [encode'] returns the run-length encoding of the passed list using the type
 ['a rle] *)
let encode' lst =
  let rec helper curr acc lst =
    let rle_type x = function 1 -> One x | _ as count -> Many (count, x) in
    match lst with
    | [] -> acc
    | [ x ] -> rle_type x (curr + 1) :: acc
    | fst :: snd :: rst ->
        if fst = snd then helper (curr + 1) acc (snd :: rst)
        else helper 0 (rle_type fst (curr + 1) :: acc) (snd :: rst)
  in
  lst |> helper 0 [] |> First_ten.rev'

(** [decode] returns an uncompressed version rle *)
let rec decode =
  let rec decompress x = function
    | 0 -> []
    | _ as c ->
        if c < 0 then raise (Failure "Count of elem cannot be (-)tive")
        else x :: decompress x (c - 1)
  in
  function
  | [] -> []
  | Many (count, x) :: xs -> decompress x count @ decode xs
  | One x :: xs -> [ x ] @ decode xs

(** [encode''] returns the run-length encoding of the passed list using the type
 ['a rle]
  Same as [encode'].
 *)
let encode'' lst =
  let rec helper curr acc lst =
    let rle_type x = function 1 -> One x | _ as count -> Many (count, x) in
    match lst with
    | [] -> acc
    | [ x ] -> rle_type x (curr + 1) :: acc
    | fst :: snd :: rst ->
        if fst = snd then helper (curr + 1) acc (snd :: rst)
        else helper 0 (rle_type fst (curr + 1) :: acc) (snd :: rst)
  in
  lst |> helper 0 [] |> First_ten.rev'


(** [duplicate] takes a list and returns with each element duplicated 
*) 
let duplicate lst =
  let rec helper acc = function
    | [] -> acc
    | x :: xs -> helper (x :: x :: acc) xs
  in
  lst |> helper [] |> First_ten.rev'

(** [replicate] takes a list and [times] and returns a new list
    with each element in list replicated [times] *)
let replicate lst times =
  let rec dup x = function
    | 0 -> []
    | n -> x :: (dup x (n - 1)) 
  in
  let rec helper acc times = function
    | [] -> acc
    | x :: xs -> helper (acc @ (dup x times)) times xs
  in
  if times < 1 then raise (Failure "times should be greater than 1 or greater")
  else lst |> helper [] 3

(** [drop] returns a new list with every nth element dropped from 
    the original passed list *)
let drop lst n =
  let rec aux lst n curr acc = 
    match (lst, curr) with
    | [], _ -> acc
    | _ :: xs, 1 -> aux xs n n acc
    | x :: xs, curr -> aux xs n (curr - 1) (x :: acc)
  in
  if n < 1 then raise (Failure "n should be greater than 1 or greater")
  else First_ten.rev' (aux lst n n [])  

(** [split] returns a split pair from list [lst] passed with
    split at [n] *)
let split lst n =
  let rec aux lst n acc =
    match (lst, n) with
    | [], _ -> First_ten.rev' acc, []
    | x :: xs, 1 -> First_ten.rev' (x :: acc),  xs
    | x :: xs, n -> aux xs (n - 1) (x :: acc)
  in
  aux lst n []

(** [slice] takes two indices [i] [k] and returns a slice
    from list [lst]. List is zero indexed. [i] and [k] should be positive
    and [i] <= [k] *)
let slice lst i k =
  let rec helper acc i k curr = function
    | [] -> acc
    | _ when curr > k -> First_ten.rev' acc
    | x :: xs when curr >= i && curr <= k -> helper (x :: acc) i k (curr + 1) xs
    | _ :: xs -> helper acc i k (curr + 1) xs
  in
  if i > k then raise (Invalid_argument "i  should be <= k")
  else if i < 0 || k < 0 then raise (Invalid_argument "i and k should be positive") 
  else helper [] i k 0 lst

(** [rotate] takes a list [lst] and rotates it to left by [n] places

    To rotate by right use negative [n]
    
    Raises: [Invalid_argument] if abs of n is greater than length of [lst]
*)
let rotate lst n =
  let len = List.length lst in
  let join (a, b) = b @ a
  in
  if (abs n) > len then raise (Invalid_argument "abs of n can't be greater than length of lst")
  else
    match n with
    | 0 -> lst
    | _  when n < 0 -> join (split lst @@ len - (abs n))
    | _ -> join @@ split lst n

(** [remove_at] takes a list [lst] and removes the element at [index] from the list *)
let remove_at pos lst =
  let join_result = function
    | a, [] -> a
    | a, (_ :: xs) -> a @ xs
  in
  match (pos, lst) with
  | 0, _ :: xs -> xs
  | _, _ -> join_result @@ split lst pos
