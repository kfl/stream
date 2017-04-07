signature ITERATOR =
sig
    type ('a, 's) iterator

    val make : ('s -> ('a * 's) option) * 's -> ('a, 's) iterator

    val fromList : 'a list -> ('a, 'a list) iterator
    val toList   : ('a, 'b) iterator -> 'a list

    val map        : ('a -> 'b) -> ('a, 's) iterator -> ('b, 's) iterator
    val mapPartial : ('a -> 'b option) -> ('a, 's) iterator -> ('b, 's) iterator
    val filter     : ('a -> bool) -> ('a, 's) iterator -> ('a, 's) iterator
    val foldl      : ('a * 'b -> 'b) -> 'b -> ('a, 's) iterator -> 'b

    type ('b, 'i, 'o) fmstate
    val flatMap    : ('a -> ('b, 'i) iterator) -> ('a, 'o) iterator -> ('b, ('b, 'i, 'o) fmstate) iterator

    type ('a, 'sa, 'sb) zipstate
    val zipWith  : ('a * 'b -> 'c) -> ('a, 'sa) iterator -> ('b, 'sb) iterator
                   -> ('c, ('a, 'sa, 'sb) zipstate) iterator

    type tabstate
    val tabulate : int * (int -> 'a) -> ('a, tabstate) iterator

    type ('a, 's) memstate
    val memorise : ('a, 's) iterator -> ('a, ('a, 's) memstate) iterator
end



structure Iterator :> ITERATOR =
struct

datatype ('a, 's) step = Yield of 'a * 's
                       | Skip of 's
                       | Done

datatype ('a, 's) iterator = S of ('s -> ('a, 's) step) * 's

fun make (getItem, s) =
  let fun step s =
        case getItem s of
            SOME(e, s) => Yield(e, s)
          | NONE       => Done
  in  S(step, s)
  end

fun map f (S(step, s)) =
  let fun step' s =
        case step s of
            Yield(e, s) => Yield(f e, s)
          | Skip s      => Skip s
          | Done        => Done
  in  S(step', s)
  end

fun mapPartial f (S(step, s)) =
  let fun step' s =
        case step s of
            Yield(e, s) => (case f e of
                                SOME x => Yield(x, s)
                              | NONE   => Skip s)
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

type ('b, 'i, 'o) fmstate = ('b, 'i) iterator option * 'o

fun flatMap f (S(outer, so)) =
  let fun step (SOME(S(inner, si)), so) =
        (case inner si of
             Yield(e, s) => Yield(e, (SOME(S(inner, s)), so))
           | Skip s      => Skip (SOME(S(inner, s)), so)
           | Done        => Skip (NONE, so))
        | step (NONE, so) =
          (case outer so of
               Yield(e, so) => Skip (SOME(f e), so)
             | Skip so      => Skip (NONE, so)
             | Done         => Done)
  in  S(step, (NONE, so))
  end


fun fromList xs = make(List.getItem, xs)

fun foldl f init (S(step, s)) =
  let fun loop s acc =
        case step s of
            Yield(e, s) => loop s (f(e, acc))
          | Skip s      => loop s acc
          | Done        => acc
  in  loop s init
  end

fun toList s = rev(foldl op:: [] s)

type ('a, 'sa, 'sb) zipstate = 'sa * 'sb * 'a option

fun zipWith f (S(stepa, sa)) (S(stepb, sb)) =
  let

      fun step ((sa, sb, NONE)) =
          (case stepa sa of
               Yield(ea, sa) => Skip((sa, sb, SOME ea))
             | Skip sa       => Skip((sa, sb, NONE))
             | Done          => Done)
        | step ((sa, sb, pending as (SOME ea))) =
          (case stepb sb of
               Yield(eb, sb) => Yield(f(ea, eb), (sa, sb, NONE))
             | Skip sb       => Skip((sa, sb, pending))
             | Done          => Done)
  in S(step, (sa, sb, NONE))
  end

type tabstate = int
fun tabulate (n, f) =
  let
      fun step (i) =
          if i < n then Yield(f i, (i+1))
          else Done
  in  if n < 0 then raise Size
      else S(step, 0)
  end

datatype ('a, 's) lazy = VAL of ('a, ('a, 's) memstate) step | THUNK of 's
withtype ('a, 's) memstate = ('a, 's) lazy ref

fun thunk s = ref(THUNK s)

fun memorise (S(step, s)) =
  let fun step' (s as ref lzy) =
        case lzy of
            VAL res => res
          | THUNK ss =>
            let val res = case step ss of
                              Yield(e, s) => Yield(e, thunk s)
                            | Skip s      => Skip (thunk s)
                            | Done        => Done
            in  s := VAL res
              ; res
            end
  in  S(step', thunk s)
  end

end

val test =
    let val s1 = Iterator.fromList [1,2,3,4,5]
        val s2 = Iterator.map (fn x => 10*x) s1
        val s3 = Iterator.map (fn x => x+1) s2
        val s4 = Iterator.filter(fn x => (x div 10) mod 2 <> 0) s3
    in  Iterator.toList s4 = [11, 31, 51]
    end

val i2s = Int.toString
fun println s = (print s; print "\n")

fun effects n =
    let val s0 = Iterator.tabulate(n, fn x => (println(i2s x); x))
        val s1 = Iterator.memorise s0
        val s2 = Iterator.zipWith op+ s1 s1
        val s3 = Iterator.filter (fn x => x mod 2 = 0) s2
    in  Iterator.foldl op+ 0 s3
    end

fun bigm gen mem n =
    let val s0 = Iterator.tabulate(n, gen)
        val s1 = mem s0
        val s2 = Iterator.zipWith op+ s1 s1
        val s3 = Iterator.filter (fn x => x mod 2 = 0) s2
    in  Iterator.foldl op+ 0 s3
    end
