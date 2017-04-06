signature STREAM =
sig
    type 'a stream
    val fromList : 'a list -> 'a stream
    val toList   : 'a stream -> 'a list

    val map      : ('a -> 'b) -> 'a stream -> 'b stream
    val filter   : ('a -> bool) -> 'a stream -> 'a stream
    val foldl    : ('a * 'b -> 'b) -> 'b -> 'a stream -> 'b
    val zipWith  : ('a * 'b -> 'c) -> 'a stream -> 'b stream -> 'c stream
    val tabulate : int * (int -> 'a) -> 'a stream
end

structure Stream :> STREAM =
struct
type existential = exn

datatype ('a, 's) step = Yield of 'a * 's
                       | Skip of 's
                       | Done

datatype 'a stream = S of (existential -> ('a, existential) step) * existential

fun map f (S(step, s)) =
  let fun step' s =
        case step s of
            Yield(e, s) => Yield(f e, s)
          | Skip s      => Skip s
          | Done        => Done
  in  S(step', s)
  end

fun filter p (S(step, s)) =
  let fun step' s =
        case step s of
            elem as Yield(e, s) => if p e then elem
                                   else Skip s
          | otherwise           => otherwise
  in  S(step', s)
  end

fun fromList xs =
  let exception E of 'a list
      fun step (E []) = Done
        | step (E(x::xs)) = Yield(x, E xs)
        | step _ = raise Fail "Impossible"
  in S(step, E xs)
  end

fun foldl f init (S(step, s)) =
  let fun loop s acc =
        case step s of
            Yield(e, s) => loop s (f(e, acc))
          | Skip s      => loop s acc
          | Done        => acc
  in  loop s init
  end

fun toList s = rev(foldl op:: [] s)

fun zipWith f (S(stepa, sa)) (S(stepb, sb)) =
  let exception Z of existential * existential * 'a option

      fun step (Z(sa, sb, NONE)) =
          (case stepa sa of
               Yield(ea, sa) => Skip(Z(sa, sb, SOME ea))
             | Skip sa       => Skip(Z(sa, sb, NONE))
             | Done          => Done)
        | step (Z(sa, sb, pending as (SOME ea))) =
          (case stepb sb of
               Yield(eb, sb) => Yield(f(ea, eb), Z(sa, sb, NONE))
             | Skip sb       => Skip(Z(sa, sb, pending))
             | Done          => Done)
        | step _ = raise Fail "Impossible"
  in S(step, Z(sa, sb, NONE))
  end

fun tabulate (n, f) =
  let exception T of int
      fun step (T i) =
          if i < n then Yield(f i, T(i+1))
          else Done
        | step _ = raise Fail "Impossible"
  in  if n < 0 then raise Size
      else S(step, T 0)
  end

end

val test =
    let val s1 = Stream.fromList [1,2,3,4,5]
        val s2 = Stream.map (fn x => 10*x) s1
        val s3 = Stream.map (fn x => x+1) s2
    in  Stream.toList s3
    end

fun big n =
    let val s1 = Stream.tabulate(n, fn x => x)
        val s2 = Stream.zipWith op+ s1 s1
        val s3 = Stream.filter (fn x => x mod 2 = 0) s2
    in  Stream.foldl op+ 0 s3
    end

(* The List functions in Moscow ML are not tail-recursive, thus
   they'll fail for large lists. Here is a safe implementation of some
   of the key functions.
*)
structure SafeList : List =
struct
open List

fun tabulate(n, f) =
  let fun loop i acc =
        if i < n then loop (i+1) (f i :: acc)
        else rev acc
  in  if n < 0 then raise Size
      else loop 0 []
  end

fun map f xs = rev(foldl (fn(x, acc) => f x :: acc) [] xs)

fun filter p xs = rev(foldl (fn(x, acc) => if p x then x :: acc else acc) [] xs)

end

fun mapw f xs =
    let val res = ref[]
        val next = ref xs
    in  while not(null(!next)) do (
            res := let val x = hd(!next) in f x :: (!res) end
          ; next := tl(!next)
        )
      ; rev (!res)
    end

fun big_list n =
    let fun zipWith f xs ys = SafeList.map f (ListPair.zip(xs, ys))
        val s1 = SafeList.tabulate(n, fn x => x)
        val s2 = zipWith op+ s1 s1
        val s3 = SafeList.filter (fn x => x mod 2 = 0) s2
    in  List.foldl op+ 0 s3
    end
