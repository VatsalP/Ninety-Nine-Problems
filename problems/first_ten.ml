(** [last] returns the last element of a list *)
let rec last = function
  | [] -> None
  | [x] -> Some x
  | _::lst -> last lst


(** [last_two] returns the last two elemetns of a list as a tuple *)
let rec last_two = function
  | [] -> None
  | [_] -> None
  | x :: y :: [] -> Some (x, y)
  | _ :: lst -> last_two lst


exception Invalid_k of string

(** [at] returns kth element of a list
  @raise Invalid_k if k is less than 
  @raise Failure if list is out of bounds for k
 *)
let rec at k lst = 
  match lst with
  [] -> raise (Failure "Out of bounds")
  | hd :: rst -> 
      match k with
      1 -> hd
      | _ -> if k < 1 then raise (Invalid_k "k should be >= 1") else at (k - 1) rst 


(** [length] returns length of a list. naive implementation *)
let rec length = function
  | [] -> 0
  | _ :: rst -> 1 + length rst

(** [length'] returns length of a list. tail recursive *)
let length' lst =     
  let rec aux acc = function
    | [] -> acc
    | _ :: rst -> aux (acc + 1) rst
  in aux 0 lst
    

(** [rev] returns a list reversed. naive implementaion *)
let rec rev = function
  | [] -> []
  | [hd] -> [hd]
  | hd :: rst -> (rev rst) @ [hd]

(** [rev'] returns a list reversed. tail recursive *)
let rev' lst =
  let rec aux acc = function
    | [] -> acc
    | hd :: rst -> aux (hd :: acc) rst
  in aux [] lst


(** [is_palindrome] returns if list is true or false *)
let is_palindrome lst =
  let reversed = rev' lst in
  let rec comp x y =
    match (x, y) with
    | [], [] -> true
    | (x :: xs, y :: ys) -> if x = y then comp xs ys else false
    | (_, _) -> raise (Failure "Something went horribly wrong")
  in comp lst reversed    


type 'a node =
  | One of 'a
  | Many of 'a node list

(** [flatten] returns flattened nested list *)
let rec flatten = function
  | [] -> []
  | x :: xs -> 
      (match x with
      | One x -> x :: flatten xs
      | Many node_list -> flatten node_list @ flatten xs)
    

(** [compress] returns a the list passed with consecutives 
    duplicates removed *)
let compress lst = 
  let rec aux acc = function
    | [] -> acc
    | [x] -> x :: acc
    | x :: y :: xs -> 
        if x = y then aux acc (y :: xs)
        else aux (x :: acc) (y :: xs)
  in lst |> (aux []) |> rev'



(** [pack] returns a list with sublists packed from the passed
    list of consecutive duplicates *)
let pack lst = 
  let rec helper curr acc = function
    | [] -> acc
    | [x] -> (x :: curr) :: acc
    | fst :: snd :: rst ->
        if fst = snd then helper (fst :: curr) acc @@ snd :: rst
        else helper [] ((fst :: curr) ::  acc) @@ snd :: rst
  in lst |> (helper [] []) |> rev'


(** [encode] returns the run-length encoding of the passed list *)
let encode lst =
  let rec helper curr acc = function
    | [] -> acc
    | [x] -> (curr + 1, x) :: acc
    | fst :: snd :: rst ->
        if fst = snd then helper (curr + 1) acc (snd :: rst)
        else helper 0 ((curr + 1, fst) :: acc) (snd :: rst)
  in lst |> (helper 0 []) |> rev'

