open Com
open Test

structure TestCom =
struct

  (* [int_exp] is tested. *)

  fun int_exp_test_1 () = (int_exp 2 10) = 1024

  fun test_int_exp () = assert "int_exp test 1" int_exp_test_1

  (* [rake] is tested. *)

  fun rake_test_1 () = (rake 0 0 add 0) = 0

  fun rake_test_2 () = (rake 3 3 add 3) = 3

  fun rake_test_3 () = (rake 3 5 add 1) = 8

  fun test_rake () =
      ( assert "rake test 1" rake_test_1
      ; assert "rake test 2" rake_test_2
      ; assert "rake test 3" rake_test_3 )

  (* [drag] is tested. *)

  fun drag_test_1 () =
      let val sum = ref 1
          fun add_to_sum x = sum := (!sum + x)
      in (drag 0 3 add_to_sum; !sum = 4) end

  fun test_drag () =
      ( assert "drag test 1" drag_test_1 )

  (* [adf] is tested. *)

  fun adf_test_1 () = (adf 1 [2, 5, 4]) = [(1, 2), (1, 5), (1, 4)]

  fun test_adf () = assert "adf test 1" adf_test_1

  (* [ads] is tested. *)

  fun ads_test_1 () = (ads 1 [3, 1, 2]) = [(3, 1), (1, 1), (2, 1)]

  fun test_ads () = assert "ads test 1" ads_test_1

  (* [repeat] is tested. *)

  fun repeat_test_1 () = (repeat 2 3) = [2, 2, 2]

  fun test_repeat () = assert "repeat test 1" repeat_test_1

  (* [range] is tested. *)

  fun range_test_1 () = (range 10 10) = []

  fun range_test_2 () = (range 4 7) = [4, 5, 6]

  fun test_range () =
      ( assert "range test 1" range_test_1
      ; assert "range test 2" range_test_2 )

  fun run_tests () =
      ( print "Running tests for structure Com: "
      ; test_int_exp ()
      ; test_rake ()
      ; test_drag ()
      ; test_adf ()
      ; test_ads ()
      ; test_repeat ()
      ; test_range ()
      ; print " done.\n" )

end
