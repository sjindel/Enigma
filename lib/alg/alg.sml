open Com

signature RING =
sig

  type t

  val zero : t
  val neg  : t -> t
  val add  : t * t -> t
  val mul  : t * t -> t

end

signature FIELD =
sig

  include RING

  val one : t
  val inv : t -> t

end

signature MATRIX =
sig

  type 'a matrix

  val make : (int * int) * 'a -> 'a matrix
  val size : 'a matrix -> int * int
  val get  : 'a matrix * (int * int) -> 'a
  val set  : 'a matrix * (int * int) * 'a -> unit

end

signature RMAT =
sig

  structure R : RING
  include MATRIX

  val add : 'a matrix * 'a matrix * 'a matrix -> unit
  val sub : 'a matrix * 'a matrix * 'a matrix -> unit
  val mul : 'a matrix * 'a matrix * 'a matrix -> unit

end

structure Matrix : MATRIX =
struct

  type 'a matrix = int * int * 'a array

  fun make ((rows, cols), x) = (rows, cols, Array.array (rows * cols, x))
  fun size (r, c, _) = (r, c)
  fun get (m, (r, c)) = raise Unimplemented
  fun set (m, (r, c), x) = raise Unimplemented

end
