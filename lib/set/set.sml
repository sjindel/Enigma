open Com

functor SetUtilFn (S : ORD_SET) : sig

  val symdiff : S.set * S.set -> S.set
  val forall : (S.Key.ord_key -> bool) -> S.set -> bool

end =
struct

  fun symdiff (x, y) = S.difference (S.union (x, y), S.intersection (x, y))

  (* Yes, it's classical. I don't care. *)
  fun forall p s = not (S.exists (fn x => not (p x)) s)

end
