structure Test =
struct

  (* Testing *)

  val test_succ_num = ref 0
  val test_fail_num = ref 0

  (* [assert] takes a string (test name) and a function f of type unit -> bool,
   * and tests that f () is true. If it is, it prints a dot ("."), otherwise it
   * reports "Assertion failed: <test name>". *)

  fun assert s f = if f ()
                   then ( test_succ_num := !test_succ_num + 1; print "." )
                   else ( test_fail_num := !test_fail_num + 1
                        ; print ("Assertion failed: " ^ s ^ ".\n") )

  fun print_results () =
      ( print ("Tests succeeded: " ^ (Int.toString (!test_succ_num)) ^ ".\n")
      ; print ("Tests failed: " ^ (Int.toString (!test_fail_num)) ^ ".\n") )

end
