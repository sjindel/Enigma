open Test

structure S = IntSet
structure SU = SetUtilFn (IntSet)

structure TestSet =
struct

  fun forall_test_1 () =
      SU.forall (fn x => (x mod 2) = 0) (S.addList (S.empty, [2, 4, 8, 20]))
  fun forall_test_2 () =
      not
        (SU.forall (fn x => (x mod 2) = 0) (S.addList (S.empty, [2, 3, 8, 20])))

  fun test_forall () = assert "forall test 1" forall_test_1

  fun run_tests () =
      ( print "Running tests for functor SetUtilFn: "
      ; test_forall ()
      ; print " done.\n" )

end
