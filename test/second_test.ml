open OUnit2
open Problems.Second_ten

let suite = "Test for Second Ten Problems" >::: [
  "encode_modified" >:: (
     fun _ -> assert_equal 
     [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
      Many (4, "e")]
     (encode'
          ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]));
  "decode" >:: (
   fun _ -> assert_equal 
   ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
   (decode 
    [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many
    (4,"e")]));
]

let () = 
  run_test_tt_main suite
