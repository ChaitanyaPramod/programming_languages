(* Coursera Programming Languages, Homework 3, Provided Code *)

(* 1 *)
val only_capitals =
    List.filter (fn x => Char.isUpper (String.sub(x, 0) ))


(* 2 *)
val longest_string1 =
    foldl ((fn (x, acc) => if size x > size acc then x else acc)) ""


(* 3 *)
val longest_string2 =
    foldl ((fn (x, acc) => if size x >= size acc then x else acc)) ""


(* 4 *)
fun longest_string_helper f =
    foldl ((fn (x, acc) => if f(size x, size acc) then x else acc)) ""

val longest_string3 = longest_string_helper op>
val longest_string4 = longest_string_helper op>=


(* 5 *)
val longest_capitalized = longest_string3 o only_capitals


(* 6 *)
val rev_string = implode o rev o explode


exception NoAnswer

(* 7 *)
fun first_answer f xs =
    case (List.mapPartial f xs) of
        [] => raise NoAnswer
      | xs => hd xs

(* 8 *)
fun all_answers f xs =
    if List.all (isSome o f) xs
    then (SOME o List.concat o List.mapPartial f) xs
    else NONE


datatype pattern = Wildcard
         | Variable of string
         | UnitP
         | ConstP of int
         | TupleP of pattern list
         | ConstructorP of string * pattern

datatype valu = Const of int
          | Unit
          | Tuple of valu list
          | Constructor of string * valu

fun g f1 f2 p =
    let 
    val r = g f1 f2 
    in
    case p of
        Wildcard          => f1 ()
      | Variable x        => f2 x
      | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
      | ConstructorP(_,p) => r p
      | _                 => 0
    end

(* 9 *)
val count_wildcards = g (fn () => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn () => 1) String.size

fun count_some_var (s, p) =
    g (fn () => 0) (fn x => if x = s then 1 else 0) p

(* 10 *)
fun check_pat p =
    let
        fun all_variables p = case p of
            Variable x           => [x]
          | ConstructorP(_, p)   => all_variables p
          | TupleP ps            => List.concat (map all_variables ps)
          | _                    => []

        fun has_dupes xs = case xs of
            []    => false
          | x::xs => List.exists (fn s => x = s) xs orelse has_dupes xs
    in
        (not o has_dupes o all_variables) p
    end

(* 11 *)
fun match arg = case arg of
    (_, Wildcard)                             => SOME []
  | (v, Variable s)                           => SOME [(s, v)]
  | (Unit, UnitP)                             => SOME []
  | (Const x, ConstP y)                       => if x = y then SOME [] else NONE
  | (Tuple vs, TupleP ps)                     => ((all_answers match (ListPair.zipEq(vs, ps))) handle UnequalLengths => NONE)
  | (Constructor (s, v), ConstructorP (c, p)) => if s = c then match (v, p) else NONE
  | _                                         => NONE


(* 12 *)
fun first_match v ps =
    (SOME (first_answer (fn p => match (v, p)) ps)) handle NoAnswer => NONE

(**** for the challenge problem only ****)

datatype typ = Anything
         | UnitT
         | IntT
         | TupleT of typ list
         | Datatype of string
