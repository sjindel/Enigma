open Com
open Test
open Emb
open Core

structure L = List
structure A = Array

structure S = IntSet
structure M = IntMap

structure TestEmb =
struct

  (* Testing utilities. *)

  fun from_valid x =
      case x of VALID x => x | _ => raise (Invariant "from_valid")

  fun is_valid x = case x of VALID _ => true | _ => false

  fun is_term x = case x of TERM _ => true | _ => false

  fun is_dead x = case x of DEAD => true | _ => false


  (* Make a map of sets from a list of pairs of (key, list). *)

  fun make_map l =
      foldr' M.empty l
      (fn ((a, al), m)
          => M.insert (m, a, (List.foldl S.add' S.empty al)))

  fun make_graph l =
      imperativize_graph
        (foldr' M.empty l
                (fn ((a, al), m)
                    => M.insert (m, a, (List.foldl ST.add' ST.empty al))))

  (* Determine whether two maps are equal. *)

  (* Test if a map is contained within another map. *)

  fun map_contained seq m m' =
      L.all (fn (k, s) => L.exists (fn (k', s') =>
                                       k = k'
                                       andalso
                                       seq (s, s'))
                                   (M.listItemsi m')) (M.listItemsi m)

  (* [map_contained] is tested. *)

  fun test_map_contained () =
      let val m1 = make_map [(1, [2, 3]), (3, [2])]
          val m2 = make_map [(1, [2, 3]), (3, [2]), (2, [4])]
      in ( assert "map_contained test 1" (fn () => map_contained S.equal m1 m2)
         ; assert "map_contained test 2"
                  (fn () => not (map_contained S.equal m2 m1)) )
      end

  (* Determine whether two sets represented as arrays are equal. *)

  fun array_contained m m' =
      array_forall (fn x => A.exists (fn y => x = y) m') m

  fun array_eq (m, m') = (array_contained m m') andalso (array_contained m' m)

  (* [map_eq] is tested. *)

  fun map_eq m1 m2 = (map_contained S.equal m1 m2) andalso
                     (map_contained S.equal m2 m1)

  fun graph_eq m1 m2 = (map_contained array_eq m1 m2) andalso
                       (map_contained array_eq m2 m1)

  fun map_eq_test_1 () =
      let val m1 = make_map [(1, [2, 3]), (3, [2])]
          val m2 = make_map [(3, [2]), (1, [3, 2])]
      in map_eq m1 m2 end

  fun map_eq_test_2 () =
      let val m1 = make_map [(1, [2, 3]), (3, [4, 5])]
          val m2 = make_map [(1, [2]), (2, [3])]
      in not (map_eq m1 m2) end

  fun test_map_eq () =
      ( assert "map_eq test 1" map_eq_test_1
      ; assert "map_eq test 2" map_eq_test_2 )

  (* [stand] is tested. *)

  fun stand_test_1 () = stand (VALID 3) (VALID 2) = VALID (3, 2)

  fun stand_test_2 () = stand (TERM 3) (VALID 2) = VALID (3, 2)

  fun stand_test_3 () = stand (VALID 3) (TERM 2) = VALID (3, 2)

  fun stand_test_4 () = stand (TERM 3) (TERM 2) = TERM (3, 2)

  fun stand_test_5 () = stand DEAD (TERM 2) = DEAD

  fun stand_test_6 () = stand (TERM 3) DEAD = DEAD

  fun stand_test_7 () = stand DEAD DEAD = DEAD

  fun test_stand () =
      ( assert "stand test 1" stand_test_1
      ; assert "stand test 2" stand_test_2
      ; assert "stand test 1" stand_test_3
      ; assert "stand test 1" stand_test_4
      ; assert "stand test 2" stand_test_5
      ; assert "stand test 2" stand_test_6
      ; assert "stand test 2" stand_test_7 )

  (* [check] is tested. *)

  fun check_test_1 () =
      let val m = make_map [(0, [1, 2]), (1, [1, 2])]
      in ( assert "check test 1a" (fn () => check true m = VALID m)
         ; assert "check test 1b" (fn () => check false m = VALID m) ) end

  fun check_test_2 () =
      let val m = make_map [(1, [1]), (2, [1])]
      in ( assert "check test 2a" (fn () => check true m = DEAD)
         ; assert "check test 2b" (fn () => check false m = TERM m) ) end

  fun check_test_3 () =
      let val m = make_map [(4, [1]), (5, [2])]
      in ( assert "check test 3a" (fn () => check true m = TERM m)
         ; assert "check test 3b" (fn () => check false m = TERM m) ) end

  fun check_test_4 () =
      let val m = make_map [(4, [3]), (5, []), (2, [7, 4])]
      in ( assert "check test 4a" (fn () => check true m = DEAD)
         ; assert "check test 4b" (fn () => check false m = DEAD) ) end

  fun test_check () =
      ( check_test_1 ()
      ; check_test_2 ()
      ; check_test_3 ()
      ; check_test_4 () )

  (* [test_mapping] is tested. *)

  fun check_test_mapping_1 () =
      let val m = make_map [(4, [2, 6]), (3, [1])]
          fun p (x, y) = (x mod 2) = (y mod 2)
      in test_mapping p m = m end

  fun check_test_mapping_2 () =
      let val m = make_map [(4, [2, 4]), (3, [8])]
          fun p (x, y) = (x mod 2) = (y mod 2)
      in (test_mapping p m; false) handle Change _ => true end

  fun test_test_mapping () =
      ( assert "test_mapping test 1" check_test_mapping_1
      ; assert "test_mapping test 2" check_test_mapping_2 )

  (* [rel] is tested. *)

  fun test_rel () =
      let val m = make_map [(1, [1, 2]), (2, [])]
          val n' = make_map [(0, [1]), (1, [0])]
      in ( assert "rel test 1" (fn () => (rel m (1, 2)))
         ; assert "rel test 2" (fn () => (not (rel m (2, 1))))
         ; assert "rel test 3" (fn () => rel n' (0, 1))
         ; assert "rel test 4" (fn () => rel n' (1, 0))
         ; assert "rel test 5" (fn () => not (rel n' (0, 0)))
         ; assert "rel test 6" (fn () => not (rel n' (1, 1))) ) end

  (* [alternate] is tested. *)

  fun test_alternate () =
      let val alt2 = alternate (make_map [(1, [2]), (2, [3, 1]), (3, [1, 4])]) 2
      in ( assert "alternate test 1"
                  (fn () =>
                      not
                        (is_none
                           (List.find
                              (map_eq (make_map [ (1, [2])
                                                , (2, [3])
                                                , (3, [1, 4]) ])) alt2)))
         ; assert "alternate test 2"
                  (fn () =>
                      not
                        (is_none
                           (List.find
                              (map_eq (make_map [ (1, [2])
                                                , (2, [1])
                                                , (3, [1, 4]) ])) alt2)))

         ;  assert "alternate test 3"
                   (fn () =>
                       is_none
                         (List.find
                            (map_eq (make_map [ (1, [2])
                                              , (2, [3, 1])
                                              , (3, [1, 4]) ])) alt2)) ) end

  (* [test_node_relation] is tested. *)

  fun test_test_node_relation () =
      let val a = make_graph [(0, [(0, 1)]), (1, [(0, 0)])]
          val b = make_graph [(0, [(0, 1)]), (1, [])]
          val n = make_map [(0, [0, 1]), (1, [0, 1])]
          val n' = make_map [(0, [1]), (1, [0])]
          val s = make_map [(0, [0])]
      in ( assert "test_node_relation test 1a"
                  (fn () => test_node_relation (a, b) (n, s) (0, 0))
         ; assert "test_node_relation test 1b"
                  (fn () => test_node_relation (a, b) (n, s) (1, 0))
         ; assert "test_node_relation test 1c"
                  (fn () => not (test_node_relation (a, b) (n, s) (0, 1)))
         ; assert "test_node_relation test 1d"
                  (fn () => not (test_node_relation (a, b) (n, s) (1, 1)))
         ; assert "test_node_relation test 1e"
                  (fn () => test_node_relation (b, a) (n', s) (0, 1))
         ; assert "test_node_relation test 1f"
                  (fn () => test_node_relation (b, a) (n', s) (1, 0)) ) end

  (* [refine] is tested. *)

  fun test_refine () =
      let val a = make_graph [(0, [(0, 1)]), (1, [(0, 0)])]
          val b = make_graph [(0, [(0, 1)]), (1, [])]
          val n = make_map [(0, [0, 1]), (1, [0, 1])]
          val s = make_map [(0, [0])]
          val n' = make_map [(0, [1]), (1, [0])]
      in ( assert "refine test 1" (fn () => is_dead (refine (a, b) (n, s)))
         ; assert "refine test 2" (fn () => is_term (refine (b, a) (n', s)))
         ) end


  (* [count_symbols] is tested. *)

  fun test_count_symbols () =
      let val g = make_graph [(0, [(0, 1)]), (1, [(1, 0)])]
          val h = make_graph [(0, [(0, 1)]), (1, [(0, 0)])]
      in ( assert "count_symbols test 1" (fn () => 2 = count_symbols g)
         ; assert "count_symbols test 2" (fn () => 1 = count_symbols h) ) end

  (* [embed] is tested. *)

  fun test_embed () =
      let val a = make_graph [(0, [(0, 1)]), (1, [(0, 0)])]
          val b = make_graph [(0, [(0, 1)]), (1, [])]
          val c = make_graph [ (0, [(0, 2), (1, 1)]), (1, [(0, 0), (1, 2)])
                             , (2, [(0, 0), (1, 1)]) ]
          val d = make_graph [ (0, [(0, 1), (1, 2)]), (1, [(0, 1), (1, 1)])
                             , (2, [(0, 2), (1, 2)]) ]
          val e = make_graph [ (0, [(0, 1), (1, 2), (2, 3)])
                             , (1, [(0, 1), (1, 1), (2, 2)])
                             , (2, [(0, 2), (1, 2), (2, 1)])
                             , (3, [(0, 3), (1, 3), (2, 3)]) ]
      in ( assert "embed test 1" (fn () => L.null (embed false (a, b)))
         ; assert "embed test 2" (fn () => L.null (embed true (a, b)))
         ; assert "embed test 3" (fn () => 2 = L.length (embed false (b, a)))
         ; assert "embed test 4" (fn () => 1 = L.length (embed true (b, a)))
         ; assert "embed test 5" (fn () => L.null (embed false (c, a)))
         ; assert "embed test 6" (fn () => L.null (embed true (c, a)))
         ; assert "embed test 7" (fn () => 4 = L.length (embed false (a, c)))
         ; assert "embed test 8" (fn () => 1 = L.length (embed true (a, c)))
         ; assert "embed test 9" (fn () => L.null (embed false (e, d)))
         ; assert "embed test 10" (fn () => L.null (embed true (e, d)))
         ; assert "embed test 11" (fn () => 2 = L.length (embed false (d, e)))
         ; assert "embed test 12" (fn () => 1 = L.length (embed true (d, e)))
         ) end

  (* Trace tests. *)

  fun trace_tests () =
      let val a = make_graph [(0, [(0, 1)]), (1, [(0, 0)])]
          val b = make_graph [(0, [(0, 1)]), (1, [])]
          val c = make_graph [ (0, [(0, 1), (1, 2)])
                             , (1, [(0, 2), (1, 0)])
                             , (2, [(0, 0), (1, 1)])]

          (* Test embedding [a] into [b] (no solutions). *)

          val n = make_map [(0, [0, 1]), (1, [0, 1])]
          val s = make_map [(0, [0])]
          val r = refine (a, b) (n, s)
          val () = ( assert "trace test 1a" (fn () => is_dead r)
                   ; assert "trace test 1b"
                            (fn () => L.null (embed false (a, b)))
                   ; assert "trace test 1c"
                            (fn () => L.null (embed true (a, b)))
                   ; assert "trace test 1d"
                            (fn () => List.null (finalize false (a, b) (n, s)
                                                          (0, 0))) )

          (* Test embedding [b] into [a] (two solutions. *)

          val r0 = refine (b, a) (n, s)
          val () = assert "trace test 2a" (fn () => is_valid r0)
          val (n1, _) = from_valid r0
          val () = assert "trace test 2b" (fn () => map_eq n n1)
          val a1 = alternate n1 0
          val () = assert "trace test 2c" (fn () => 2 = L.length a1)
          val (n2, n3) = case a1 of [n2, n3] => (n2, n3)
                                  | _ => raise (Invariant "list length")
          val () = ( assert "trace test 2d"
                            (fn () => map_eq n2 (make_map [ (0, [0])
                                                          , (1, [0, 1])]))
                   ; assert "trace test 2e"
                            (fn () => map_eq n3 (make_map [ (0, [1])
                                                          , (1, [0, 1])])) )
          val (r1, r2) = (refine (b, a) (n2, s), refine (b, a) (n3, s))
          val (n4, _) = from_valid r1
          val (n5, _) = from_valid r2
          val () = ( assert "trace test 2f" (fn () => map_eq n2 n4)
                   ; assert "trace test 2g" (fn () => map_eq n3 n5) )
          val (a2, a3) = (alternate n4 1, alternate n5 1)
          val (n6, n7) = case a2 of [n6, n7] => (n6, n7)
                                  | _ => raise (Invariant "list length")
          val (n8, n9) = case a3 of [n8, n9] => (n8, n9)
                                  | _ => raise (Invariant "list length")
          val () = ( assert "trace test 2h"
                            (fn () => map_eq n6 (make_map [(0, [0]), (1, [0])]))
                   ; assert "trace test 2i"
                            (fn () => map_eq n7 (make_map [(0, [0]), (1, [1])]))
                   ; assert "trace test 2j"
                            (fn () => map_eq n8 (make_map [(0, [1]), (1, [0])]))
                   ; assert "trace test 2k"
                            (fn () => map_eq n9 (make_map [ (0, [1])
                                                          , (1, [1])])) )
          val (r3, r4, r5, r6) = mt4 (refine (b, a)) ( (n6, s), (n7, s), (n8, s)
                                                     , (n9, s))
          val () = ( assert "trace test 2l" (fn () => is_dead r3)
                   ; assert "trace test 2m" (fn () => is_dead r6)
                   ; assert "trace test 2n" (fn () => is_term r4)
                   ; assert "trace test 2o" (fn () => is_term r5) )

      (* Working back upwards to check [finalize]. *)

      in ( assert "trace test 2p"
                  (fn () => L.null (finalize false (b, a) (n6, s) (2, 0)))
         ; assert "trace test 2q"
                  (fn () => L.null (finalize false (b, a) (n9, s) (2, 0)))
         ; assert "trace test 2r"
                  (fn () => 1 = L.length (finalize false (b, a) (n7, s) (2, 0)))
         ; assert "trace test 2s"
                  (fn () => 1 = L.length (finalize false (b, a) (n8, s) (2, 0)))
         ; assert "trace test 2t"
                  (fn () => 1 = L.length (finalize false (b, a) (n5, s) (1, 0)))
         ; assert "trace test 2u"
                  (fn () => 1 = L.length (finalize false (b, a) (n7, s) (1, 0)))
         ; assert "trace test 2v"
                  (fn () => 2 = L.length (finalize false (b, a) (n1, s) (0, 0)))
         ; assert "trace test 2w"
                  (fn () => 2 = L.length (finalize false (b, a) (n, s) (0, 0)))
         ) end

  fun run_tests () =
      ( print "Running tests for structure Emb: "
      ; test_map_contained ()
      ; test_stand ()
      ; test_check ()
      ; test_test_mapping ()
      ; test_rel ()
      ; test_alternate ()
      ; test_test_node_relation ()
      ; test_refine ()
      ; test_count_symbols ()
      ; test_embed ()
      ; trace_tests ()
      ; print " done.\n" )

end
