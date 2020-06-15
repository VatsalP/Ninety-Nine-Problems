open OUnit2
open Problems

let suite =
  "Tests for Ten Problems"
  >::: [
         ( "last_some" >:: fun _ ->
           assert_equal (Some "d") (First_ten.last [ "a"; "b"; "c"; "d" ]) );
         ("last_none" >:: fun _ -> assert_equal None (First_ten.last []));
         ( "last_two_some" >:: fun _ ->
           assert_equal
             (Some ("c", "d"))
             (First_ten.last_two [ "a"; "b"; "c"; "d" ]) );
         ( "last_two_none" >:: fun _ ->
           assert_equal None (First_ten.last_two [ "a" ]) );
         ( "at" >:: fun _ ->
           assert_equal "c" (First_ten.at 3 [ "a"; "b"; "c"; "d"; "e" ]) );
         ( "at_error" >:: fun _ ->
           assert_raises (First_ten.Invalid_k "k should be >= 1") (fun () ->
               First_ten.at (-1) [ "a" ]) );
         ( "at_error_failure" >:: fun _ ->
           assert_raises (Failure "Out of bounds") (fun () ->
               First_ten.at 4 [ "a" ]) );
         ( "length_1" >:: fun _ ->
           assert_equal 3 (First_ten.length [ "a"; "b"; "c" ]) );
         ("length_2" >:: fun _ -> assert_equal 0 (First_ten.length []));
         ( "rev" >:: fun _ ->
           assert_equal [ "c"; "b"; "a" ] (First_ten.rev [ "a"; "b"; "c" ]) );
         ( "palindrome_true" >:: fun _ ->
           assert_equal true
             (First_ten.is_palindrome [ "x"; "a"; "m"; "a"; "x" ]) );
         ( "palindrome_false" >:: fun _ ->
           assert_equal false (First_ten.is_palindrome [ "x"; "a"; "m"; "a" ])
         );
         ( "flatten" >:: fun _ ->
           assert_equal
             [ "a"; "b"; "c"; "d"; "e" ]
             (First_ten.flatten
                [
                  One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ];
                ]) );
         ( "compress" >:: fun _ ->
           assert_equal
             [ "a"; "b"; "c"; "a"; "d"; "e" ]
             (First_ten.compress
                [
                  "a";
                  "a";
                  "a";
                  "a";
                  "b";
                  "c";
                  "c";
                  "a";
                  "a";
                  "d";
                  "e";
                  "e";
                  "e";
                  "e";
                ]) );
         ( "pack" >:: fun _ ->
           assert_equal
             [
               [ "a"; "a"; "a"; "a" ];
               [ "b" ];
               [ "c"; "c" ];
               [ "a"; "a" ];
               [ "d"; "d" ];
               [ "e"; "e"; "e"; "e" ];
             ]
             (First_ten.pack
                [
                  "a";
                  "a";
                  "a";
                  "a";
                  "b";
                  "c";
                  "c";
                  "a";
                  "a";
                  "d";
                  "d";
                  "e";
                  "e";
                  "e";
                  "e";
                ]) );
         ( "encode" >:: fun _ ->
           assert_equal
             [ (4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e") ]
             (First_ten.encode
                [
                  "a";
                  "a";
                  "a";
                  "a";
                  "b";
                  "c";
                  "c";
                  "a";
                  "a";
                  "d";
                  "e";
                  "e";
                  "e";
                  "e";
                ]) );
       ]

let () = run_test_tt_main suite
