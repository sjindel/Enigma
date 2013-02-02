open Com

structure L = List
structure A = Array

structure S = IntSet
structure M = IntMap

structure SU = SetUtilFn (S)

structure Emb =
struct

  structure ORD_TUP = OrdTwoTupleFn ( structure P = OrdInt
                                    ; structure Q = OrdInt )

  datatype 'a state = VALID of 'a | TERM of 'a | DEAD

  exception Malformed_input

  (* Perform an ``and'' operation on states. *)

  fun stand x y =
      case (x, y) of
        (DEAD, _) => DEAD
      | (_, DEAD) => DEAD
      | (TERM x, TERM y) => TERM (x, y)
      | (VALID x, TERM y) => VALID (x, y)
      | (TERM x, VALID y) => VALID (x, y)
      | (VALID x, VALID y) => VALID (x, y)

  exception Dead

  fun validate x =
      case x of
        DEAD => DEAD
      | TERM x => VALID x
      | VALID x => VALID x

  (* Check if a mapping is an inner node, a terminal, or dead (no injective (if
   * [inj] is true) functions can be formed as subsets of it). *)

  fun check inj m =
      let fun folder (set, (img, st)) =
              case S.numItems set of
                0 => raise Dead
              | 1 => (S.union (set, img), st)
              | _ => (S.union (set, img), validate st)
      in let val (img, fin) = M.foldl folder (S.empty, TERM m) m
         in if inj then (if S.numItems img < M.numItems m then DEAD else fin)
            else fin end
         handle Dead => DEAD end

  fun has_function m = check false m
  fun has_injection m = check true m

  exception Change of S.set M.map

  fun test_mapping p m =
      M.foldli
        (fn (key, set, map) =>
            S.foldl
              (fn (elem, map) =>
                  if p (key, elem) then m
                  else raise (Change (M.insert (map, key,
                                                S.delete (set, elem)))))
              map set) m m

  fun rel r (x, y) =
      case M.find (r, x) of NONE => false | SOME s => S.member (s, y)

  fun find_input (m, x) =
      case M.find (m, x) of NONE => raise Malformed_input | SOME x => x

  fun test_node_relation (graph_a, graph_b)
                         (node_relation, symbol_relation)
                         (node_a, node_b) =
      array_forall
        (fn (sym_a, nb_a) =>
            A.exists
              (fn (sym_b, nb_b) =>
                  (rel node_relation (nb_a, nb_b))
                  andalso
                  (rel symbol_relation (sym_a, sym_b)))
              (find_input (graph_b, node_b)))
        (find_input (graph_a, node_a))

  fun test_sym_relation (graph_a, graph_b) (node_relation, symbol_relation)
                        (node_in_a, node_in_b) = true

  exception Restart of S.set M.map * S.set M.map

  fun refine (a, b) (n, s) =
      (stand (has_injection (test_mapping (test_node_relation (a, b) (n, s)) n)
              handle Change n => raise (Restart (n, s)))
             (has_function (test_mapping (test_sym_relation (a, b) (n, s)) s)
              handle Change s => raise (Restart (n, s))))
      handle Restart (n, s) => refine (a, b) (n, s)

  fun alternate n rn =
      L.map
        (fn e => M.insert (n, rn, S.singleton e))
        (S.listItems (from_some "Can not alternate on non existent row."
                                (M.find (n, rn))))

  exception Found of S.set M.map * S.set M.map

  fun finalize f (a, b) (n, s) (rn, rs) =
      let val state = refine (a, b) (n, s)
      in case state of
           DEAD => []
         | TERM x => if f then raise (Found x) else ([x])
         | VALID (n, s) =>
           case (rn = M.numItems n, rs = M.numItems s, rs > rn) of
             (true, true, _) => raise (Invariant "Valid map is terminal.")
           | (false, true, _) => nw f (a, b) (n, s) (rn, rs)
           | (true, false, _) => sw f (a, b) (n, s) (rn, rs)
           | (false, false, true) => nw f (a, b) (n, s) (rn, rs)
           | (false, false, false) => sw f (a, b) (n, s) (rn, rs) end
  and nw f (a, b) (n, s) (rn, rs) =
      let val ns = alternate n rn
      in L.concat (L.map (fn n => finalize f (a, b) (n, s) (rn + 1, rs)) ns) end
  and sw f (a, b) (n, s) (rn, rs) =
      let val ss = alternate s rs
      in L.concat (L.map (fn s => finalize f (a, b) (n, s) (rn, rs + 1)) ss) end

  fun replicate (ms, ss) =
      let val complete_set = rake 0 ss S.add' S.empty
      in rake 0 ms
              (fn (x, m) => M.insert (m, x, complete_set)) M.empty end

  exception Not_perm

  fun count_symbols m =
      S.numItems
        (M.foldli
           (fn (k, s, x) =>
               A.foldl (fn ((sym, _), y) => S.add (y, sym)) x s) S.empty m)

  fun embed fast (a, b) =
      let val (ann, bnn) = (M.numItems a, M.numItems b)
          val (asn, bsn) = (count_symbols a, count_symbols b)
          val n = replicate (ann, bnn)
          val s = replicate (asn, bsn)
      in finalize fast (a, b) (n, s) (0, 0) handle Found x => [x] end

end
