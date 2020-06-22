open OUnit2
open Problems.Second_ten

let print_list = String.concat " ; "

let suite =
  "Test for Second Ten Problems"
  >::: [
         ( "encode_modified" >:: fun _ ->
           assert_equal
             [
               Many (4, "a");
               One "b"; Many (2, "c");
               Many (2, "a");
               One "d";
               Many (4, "e");
             ]
             (encode'
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
         ( "decode" >:: fun _ ->
           assert_equal
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
             ]
             (decode
                [
                  Many (4, "a");
                  One "b";
                  Many (2, "c");
                  Many (2, "a");
                  One "d";
                  Many (4, "e");
                ]) );
         ( "encode_modified_direct" >:: fun _ ->
           assert_equal
             [
               Many (4, "a");
               One "b";
               Many (2, "c");
               Many (2, "a");
               One "d";
               Many (4, "e");
             ]
             (encode''
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
        ( "duplicate" >:: fun _ ->
          assert_equal
            ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]
            (duplicate ["a";"b";"c";"c";"d"])
        );
        ( "replicate" >:: fun _ ->
          assert_equal
            ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
            (replicate ["a";"b";"c"] 3)
        );
        ( "drop_nth" >:: fun _ ->
          assert_equal 
            ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
            (drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3)
        );
        ( "split" >:: fun _ ->
          assert_equal
            (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
            (split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3)
        );
        ( "split_greater" >:: fun _ ->
          assert_equal
            (["a"; "b"; "c"; "d"], [])
            (split ["a";"b";"c";"d"] 5)
        );
        ( "slice" >:: fun _ ->
          assert_equal
            ["c"; "d"; "e"; "f"; "g"]
            (slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6)
        );
        ( "rotate" >:: fun _ ->
          assert_equal
            ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]
            (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3)
           ~printer:print_list
        );
        ( "rotate_negative" >:: fun _ ->
          assert_equal
            ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"]
            (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2))
            ~printer:print_list
        );
        ( "remove_at" >:: fun _ ->
          assert_equal
            ["a"; "c"; "d"]
            (remove_at 1 ["a";"b";"c";"d"])
            ~printer:print_list
        );
       ]

let () = run_test_tt_main suite
