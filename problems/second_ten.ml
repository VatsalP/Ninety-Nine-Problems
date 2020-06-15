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
