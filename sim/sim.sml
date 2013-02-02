open Com
open Core
open Emb
open TextIO
open List

fun main () = ()

structure A = Array
structure L = List

structure S = IntSet
structure M = IntMap


(** Code to compute embeddings **)

fun gr r n = imperativize_graph
               (generate_graph (generate_elementary_description r) n)

fun read_int () = let val l = from_some "bad input" (inputLine stdIn)
                  in from_some "bad input" (Int.fromString l) end

structure LP2 = PrintListFn (PrintTwoTupleFn ( structure P = PrintInt
                                             ; structure Q = PrintInt ))

structure GP = PrintMapFn ( structure KP = PrintInt
                          ; structure EP =
                            PrintArrayFn
                              (PrintTwoTupleFn ( structure P = PrintInt
                                               ; structure Q = PrintInt))
                          ; structure M = M )

structure LP = PrintListFn (PrintInt)

structure MP = PrintMapFn ( structure KP = PrintInt
                          ; structure EP = PrintSetFn ( structure KP = PrintInt
                                                      ; structure S = S )
                          ; structure M = M )

fun add_to_map_set ((x, k), m) =
    case M.find (m, k) of
      NONE => M.insert (m, k, (S.singleton x))
    | SOME s => M.insert (m, k, (S.add (s, x)))

fun transform (m : ((int * int) A.array) M.map) : S.set M.map M.map
  = M.map (fn a => A.foldl add_to_map_set M.empty a)  m

fun print_edge (k : int) (x : int) (s : S.set) =
    let val label = String.concatWith ", " (List.map Int.toString
                                                     (S.listItems s))
    in (Int.toString k) ^ " -> " ^ (Int.toString x) ^ " [label=" ^ label ^ "];"
    end

fun dot_print (name : string) (g : S.set M.map M.map) : string =
    let val body =
            M.foldli
              (fn (k, x, s) =>
                  M.foldli
                    (fn (t, lbs, s) => (print_edge k t lbs) ^ "\n" ^ s) s x) "" g
    in "digraph " ^ name ^ "\n{\n" ^ body ^ "}" end

fun print_sol (n, s) =
    ( print "^^^^^^^^\n"
    ; print "Node map:\n"
    ; print (MP.print n)
    ; print "\n"
    ; print "Symbol map:\n"
    ; print (MP.print s)
    ; print "\n"
    ; print "vvvvvvvv\n" )

fun main () =
    let val r1 = read_int ()
        val s1 = read_int ()
        val r2 = read_int ()
        val s2 = read_int ()
        val g1 = gr r1 s1
        val g2 = gr r2 s2
        val sols = embed false (g1, g2)
        val soln = length sols
    in if soln = 0
       then print "No solution found.\n"
       else ( print "Embedding\n"
            ; print (dot_print "G1" (transform g1))
            ; print "\ninto\n"
            ; print (dot_print "G2" (transform g2))
            ; print "\n--------\n"
            ; print ((Int.toString soln) ^ " solution(s) found:\n")
            ; List.app print_sol sols )
    end
(*

 (** Code to print rule equivalences **)

 fun inot 0 = 1
   | inot 1 = 0
   | inot x = x

 fun sym [p7, p6, p5, p4, p3, p2, p1, p0] = [p7, p3, p5, p1, p6, p2, p4, p0]
   | sym _ = raise (Fail "Not a rule")

 fun flip [p7, p6, p5, p4, p3, p2, p1, p0] = [ inot p0, inot p1, inot p2, inot p3
                                             , inot p4, inot p5, inot p6, inot p7
                                             ]
   | flip _ = raise (Fail "Not a rule")

 structure OrdSet =
 struct

   type ord_key = S.set

   val compare = S.compare

 end

 structure SS = BinarySetFn (OrdSet)

 fun pad x = (repeat 0 (8 - (L.length x)))@x

 fun add_rule (r, x) =
     case SS.find (fn s => S.member (s, r)) x of
       SOME _ => x
     | NONE => SS.add (x, S.addList (S.empty,
                                     [ r
                                     , int_of_bin
                                         (reverse
                                            (sym
                                               (pad
                                                  (reverse
                                                     (bin_of_int r)))))
                                     , int_of_bin
                                         (reverse
                                            (flip
                                               (pad
                                                  (reverse
                                                     (bin_of_int r)))))
                                     , int_of_bin
                                         (reverse
                                            (sym
                                               (flip
                                                  (pad
                                                     (reverse
                                                        (bin_of_int r))))))]))

 val equiv_classes = rake 0 256 add_rule SS.empty

 structure LIP = PrintListFn (PrintInt)


 fun main () =
     (*
      ( print (LIP.print (reverse (bin_of_int 40)))
      ; print "\n"
      ; print (LIP.print (pad (reverse (bin_of_int 40))))
      ; print "\n"
      ; print (LIP.print (sym (pad (reverse (bin_of_int 40)))))
      ; print "\n"
      ; print (Int.toString (int_of_bin (reverse
                                           (flip
                                              (sym
                                                 (pad
                                                    (reverse
                                                       (bin_of_int 40))))))))

              ($ reverse flip
                 sym pad reverse
                 bin_of_int 40)
      ; print "\n" )
      *)

     print (SS.foldl (fn (x, y) => (SP.print x) ^ "\n" ^ y)
                     "" equiv_classes)
 *)

; main () ;
