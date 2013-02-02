(* [Com] is a collection of common utility functions. *)

structure Com =
struct

  (* Common Exceptions *)

  exception Unimplemented (* unimplemented function *)
  exception Invariant of string (* violation of a programmatic invariant *)
  exception Undefined (* undefined result/case/situation *)

  (* Common Combinators *)

  (* Combinators for the option type. *)

  fun from_some s x = case x of SOME x => x | NONE => raise (Invariant s)

  fun is_none x = case x of SOME _ => false | NONE => true

  (* Prefix and Curried Versions of Common Infix Operators *)

  fun add (x, y) = x + y
  fun mul (x, y) = x * y
  fun cons (x, y) = x :: y

  fun add' x y = x + y
  fun mul' x y = x * y
  fun cons' x y = x :: y

  (* Permutation, Projection, and Map Functions on Tuples *)

  (* Projections *)

  fun fst (a, b) = a
  fun snd (a, b) = b

  val t2to1 = fst
  val t2to2 = snd

  fun t3to1 (a, b, c) = a
  fun t3to2 (a, b, c) = b
  fun t3to3 (a, b, c) = c

  (* Permutations *)

  fun tup12 (a, b) = (a, b)
  fun tup21 (a, b) = (b, a)

  fun tup123 (a, b, c) = (a, b, c)
  fun tup132 (a, b, c) = (a, c, b)
  fun tup213 (a, b, c) = (b, a, c)
  fun tup231 (a, b, c) = (b, c, a)
  fun tup312 (a, b, c) = (c, a, b)
  fun tup321 (a, b, c) = (c, b, a)

  (* Maps. *)

  fun mt2 f (a, b) = (f a, f b)
  fun mt3 f (a, b, c) = (f a, f b, f c)
  fun mt4 f (a, b, c, d) = (f a, f b, f c, f d)

  (* Common Arithmetic Combinators *)

  fun int_exp b e = Real.floor (Math.pow (Real.fromInt b, Real.fromInt e))

  (* Common Iteration Patterns *)

  (* [rake s t f init] folds [f] across the interval between [s] (inclusive) and
   * [t] (exclusive). In contrast to the common fold idiom, the function is to
   * be provided second last, because in many cases it is more clear
   * syntactically to have the start and end parameters at the beginning, as in
   * a ``for'' loop. *)

  fun rake s t f init = if s = t then init else rake (s + 1) t f (f (s, init))

  (* [drag s t f] is similar to rake, but whereas the type of the input function
   * in [rake] is [int * 'b -> 'b], the type of the input function to [drag] is
   * [int -> unit]. Consequently, the result type of the application is unit as
   * well. *)

  fun drag s t f = if s = t then () else (f s; drag (s + 1) t f)

  (* Common Mapping Patterns *)

  (* Transforming Lists of Tuples *)

  fun adf x ys = List.map (fn y => (x, y)) ys

  fun ads x ys = List.map (fn y => (y, x)) ys

  (* Other Common Combinators *)

  fun id x = x

  fun fix f x = f (fix f) x

  (* Common List Functions *)

  fun repeat x l = if l = 0 then [] else x :: repeat x (l - 1)

  (* TODO: Optimize *)

  fun reverse x =
      case x of
        [] => []
      | (x :: xs) => (reverse xs)@[x]

  fun foldl' init l f = List.foldl f init l
  fun foldr' init l f = List.foldr f init l

  fun range s t = if s = t then [] else s :: (range (s + 1) t)

  (* Common Array Functions *)

  fun array_forall f a = not (Array.exists (fn x => not (f x)) a)

  (* Laziness *)

  (* [susp] is the type of suspended computations. *)

  type 'a susp = unit -> 'a

  (* [force] and [pull] force lazy computations. Only use it in a lazy
   * evaluation context, it is intended to improve readability. *)

  fun force x = x ()

  (* Common Functions on strings. *)

  fun prp x s = x ^ s
  fun app x s = s ^ x

end

open Com

(* Other Common Signatures and Structures *)

(* [EQ] represents an type over which equality is defined. *)

signature EQ =
sig

  type t
  val eq : t -> t -> bool

end

(* [OrdInt] ascribes to [ORD_KEY] and provides the default ordering on
 * integers. *)

structure OrdInt : ORD_KEY =
struct

  type ord_key = int
  fun compare (x, y) = if x = y then EQUAL
                       else if x < y then LESS else GREATER

end

functor OrdTwoTupleFn ( structure P : ORD_KEY
                      ; structure Q : ORD_KEY ) : ORD_KEY =
struct

  type ord_key = P.ord_key * Q.ord_key

  fun compare ((p1, q1), (p2, q2)) =
      case P.compare (p1, p2) of
        LESS => LESS
      | GREATER => GREATER
      | EQUAL => Q.compare (q1, q2)

end

structure IntSet = BinarySetFn(OrdInt)
structure IntMap = BinaryMapFn(OrdInt)
