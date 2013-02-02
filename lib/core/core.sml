open Com
open Emb
structure A = Array
structure L = List
structure M = IntMap
structure S = IntSet

structure Core =
struct

  structure ST = BinarySetFn (ORD_TUP)
  structure STU = SetUtilFn (ST)

  type ca_description =
       { state_number : int (* number of states *)
      , neighborhood : int (* neighborhood size (must be odd) *)
      , rule_function : int list -> int (* rule *) }

  fun get_neighborhood { state_number = st
                       , neighborhood = nb
                       , rule_function = rfn} = nb

  fun get_rule_function { state_number = st
                        , neighborhood = nb
                        , rule_function = rfn} = rfn

  fun get_state_number { state_number = st
                        , neighborhood = nb
                        , rule_function = rfn} = st

  fun step (desc : ca_description) config sch config' =
      let val width = A.length config
      in drag 0 width
         (fn n =>
             if A.sub (sch, n) = 1
             then let val nb =
                          rake (n - (((get_neighborhood desc) - 1) div 2))
                               (n + (((get_neighborhood desc) - 1) div 2) + 1)
                               (fn (i, nb) =>
                                   (if i < 0
                                    then 0
                                    else (if i >= width
                                          then 0 else
                                          A.sub (config, i)))
                                   :: nb) []
                  in A.update (config', n, (get_rule_function desc) nb) end
             else A.update (config', n, (A.sub (config, n)))) end

  (* Generate a list of all sequences {0,1}^n. *)

  fun bin n =
      case n of
        0 => [[]]
      | n => let val s = bin (n - 1)
             in (List.map (fn l => l @ [0]) s) @ (L.map (fn l => l @ [1]) s) end

  fun run_schedules description configuration schedules =
      L.map (fn s => (configuration, s,
                      let val x = A.fromList configuration
                          val s_array = A.fromList s
                          val y = A.array (A.length x, 0)
                      in (step description x s_array y; A.foldr cons [] y) end))
            schedules

  fun exp states list =
      let fun exp' sts lst pos
            = case lst of
                [] => 0
              | (c :: cs) =>
                c * (int_exp sts pos) + (exp' sts cs (pos + 1))
      in exp' states list 0 end

  fun generate_transition_table desc size =
      let val sts = get_state_number desc
          val configs = bin size
          val schedules = bin size
      in L.concat
         (L.map
          (fn c =>
              L.map
                (fn (c, s, c') =>
                    (exp sts c, exp sts s, exp sts c'))
                (run_schedules desc c schedules)) configs) end

  fun int_of_bin l =
      case l of
        [] => 0
      | (0 :: l) => 2 * (int_of_bin l)
      | (_ :: l) => 1 + (2 * int_of_bin l)

  fun bin_of_int n =
      if n = 0 then []
      else case n mod 2 of
             0 => 0 :: (bin_of_int (n div 2))
           | _ => 1 :: (bin_of_int (n div 2))

  fun generate_elementary_description rl =
      let val rep = bin_of_int rl
          val matrix = rep @ (repeat 0 (8 - (L.length rep )))
      in { state_number = 2
         , neighborhood = 3
         , rule_function = fn nb => L.nth (matrix, (int_of_bin nb)) } end

  fun from_adjacency l =
      L.foldl
      (fn ((c, s, c'), m) =>
          case M.find (m, c) of
            NONE => M.insert (m, c, ST.singleton (s, c'))
          | SOME set => M.insert (m, c, ST.add (set, (s, c'))))
      M.empty l

  fun imperativize_graph g = M.map (fn s => A.fromList (ST.listItems s)) g

  fun generate_graph desc size =
      from_adjacency (generate_transition_table desc size)

end
