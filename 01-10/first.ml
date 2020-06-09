(* last returns the last element of a list *)
let rec last = function
  | [] -> None
  | [x] -> Some x
  | _::lst -> last lst


(* last_two returns the last two elemetns of a list as a tuple *)
let rec last_two = function
  | [] -> None
  | [x] -> None
  | x :: y :: [] -> Some (x, y)
  | x :: lst -> last_two lst


exception Invalid_k of string

(* at returns kth element of a list
 * @raises Invalid_k if k is less than 1
 * @raises Failure if list is out of bounds for kj
 *)
let rec at k lst = 
  match lst with
  [] -> raise (Failure "Out of bounds")
  | hd :: rst -> 
      match k with
      1 -> hd
      | _ -> if k < 1 then raise (Invalid_k "k should be >= 1") else at (k - 1) rst 


(* length returns length of a list. naive implementation *)
let rec length = function
  | [] -> 0
  | _ :: rst -> 1 + length rst

(* length' returns length of a list. tail recursive *)
let length' lst =     
  let rec aux acc = function
    | [] -> acc
    | _ :: rst -> aux (acc + 1) rst
  in aux 0 lst
    

(* rev returns a list reversed. naive implementaion *)
let rec rev = function
  | [] -> []
  | [hd] -> [hd]
  | hd :: rst -> (rev rst) @ [hd]

(* rev' returns a list reversed. tail recursive *)
let rev' lst =
  let rec aux acc = function
    | [] -> acc
    | hd :: rst -> aux (hd :: acc) rst
  in aux [] lst
