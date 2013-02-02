open Com

functor MaximumMatchFn (structure S : ORD_SET
                        structure M : ORD_MAP) : sig

  (* [find_maximum_matching] takes a map from "nodes" of type [M.Key.ord_key]
   * (nodes on the left of a bipartite graph) to sets of "nodes" of type
   * [S.Key.ord_key] (nodes on the right of the same bipartite graph). *)

  val find_maximum_matching : S.set M.map -> S.Key.ord_key M.map

end =
struct

  type u = M.Key.ord_key (* Type of vertices in left partition. *)
  type v = S.Key.ord_key (* Type of vertices in right partition. *)

  fun find_maximum_matching _ = raise Unimplemented

end
