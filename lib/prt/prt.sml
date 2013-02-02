open Com

(* [Printable] represents a printable type. *)

signature PRINTABLE =
sig

  type t
  val print : t -> string

end

(* [PrintUnit] ascribes to PRINTABLE and provides a simple print of the unit
 * value. *)

structure PrintUnit : PRINTABLE =
struct

  type t = unit
  fun print () = "()"

end

(* [PrintInt] ascribes to PRINTABLE and provides the default print function on
 * integers. *)

structure PrintInt : PRINTABLE =
struct

  type t = int
  fun print x = Int.toString x

end

(* [PrintListFn] prints common lists of printable types. *)

functor PrintListFn (P : PRINTABLE) : PRINTABLE =
struct

  type t = P.t list

  fun print l = "[" ^ (String.concatWith ", " (List.map P.print l)) ^ "]"

end

(* [PrintTwoTupleFn] prints two-tuples over pritable types. *)

functor PrintTwoTupleFn ( structure P : PRINTABLE
                        ; structure Q : PRINTABLE ) : PRINTABLE =
struct

  type t = P.t * Q.t

  fun print (p, q) = "(" ^ (P.print p) ^ ", " ^ (Q.print q) ^ ")"

end

(* [PrintThreeTupleFn] prints three-tuples over printable types. *)

functor PrintThreeTupleFn ( structure P : PRINTABLE
                          ; structure Q : PRINTABLE
                          ; structure R : PRINTABLE ) : PRINTABLE =
struct

  type t = P.t * Q.t * R.t

  fun print (p, q, r) = "(" ^ (P.print p) ^ ", " ^ (Q.print q) ^ ", "
                        ^ (R.print r) ^ ")"

end

(* [PrintFourTupleFn] prints four-tuples over printable types. *)

functor PrintFourTupleFn ( structure P : PRINTABLE
                         ; structure Q : PRINTABLE
                         ; structure R : PRINTABLE
                         ; structure S : PRINTABLE ) : PRINTABLE =
struct

  type t = P.t * Q.t * R.t * S.t

  fun print (p, q, r, s) = "(" ^ (P.print p) ^ ", " ^ (Q.print q) ^ ", "
                           ^ (R.print r) ^ ", " ^ (S.print s) ^ ")"

end

(* [PrintSetFn] prints ordered sets. *)

functor PrintSetFn ( structure KP : PRINTABLE
                   ; structure S : ORD_SET where type Key.ord_key = KP.t )
        : PRINTABLE =
struct

  type t = S.set

  fun print x = "{" ^ (String.concatWith ", "
                                         (List.map KP.print (S.listItems x)))
                ^ "}"

end

(* [PrintMapFn] prints ordered maps. *)

functor PrintMapFn ( structure KP : PRINTABLE
                   ; structure EP : PRINTABLE
                   ; structure M : ORD_MAP where type Key.ord_key = KP.t )
        : PRINTABLE =
struct

  type t = EP.t M.map

  structure PrintTupleList = PrintListFn (PrintTwoTupleFn ( structure P = KP
                                                          ; structure Q = EP ))

  fun print x = PrintTupleList.print (M.listItemsi x)

end

(* [PrintArrayFn] prints arrays. *)

functor PrintArrayFn (P : PRINTABLE) : PRINTABLE =
struct

  type t = P.t Array.array;

  fun print x = let val l = Array.foldl cons [] x
                    val l' = List.map P.print l
                in "[|" ^ (String.concatWith ", " l') ^ "|]" end

end
