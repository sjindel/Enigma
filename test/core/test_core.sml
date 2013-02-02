open Com
open Test
open Core
open Emb
open TestEmb

structure A = Array

structure TestCore =
struct

  fun array_eq a1 a2 =
      rake 0 (A.length a1)
      (fn (p, v) => v andalso A.sub (a1, p) = A.sub (a2, p)) true

  fun test_step () =
      let val a1 = A.fromList [0, 0, 1]
          val a2 = A.fromList [0, 0, 1]
          val asch = A.fromList [1, 0, 0]
          val ar = A.array (3, 0)
          val desc90 =
              { state_number = 2
              , neighborhood = 3
              , rule_function =
                (fn l => case l of
                           [x, y, z] => (x + z) mod 2
                         | _ => (print (Int.toString (L.length l)); 0)) }
          val () = step desc90 a1 asch ar
      in assert "step test 1" (fn () => array_eq a2 ar) end

  fun full_test () =
      let val g = make_graph [ (0, [(0, 0), (1, 0), (2, 0), (3, 0)])
                             , (1, [(0, 1), (1, 0), (2, 3), (3, 2)])
                             , (2, [(0, 2), (1, 3), (2, 0), (3, 1)])
                             , (3, [(0, 3), (1, 3), (2, 3), (3, 3)]) ]
          val g' = imperativize_graph
                   (generate_graph (generate_elementary_description 90) 2)
      in assert "full test 1" (fn () => graph_eq g g') end

  fun run_tests () =
      ( print "Running tests for structure Core: "
      ; test_step ()
      ; full_test ()
      ; print " done.\n" )

end
