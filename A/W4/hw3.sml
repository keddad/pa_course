(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

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

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals x =
	List.filter (fn x => Char.isUpper (String.sub(x, 0))) x

fun longest_string1 x =
	List.foldl (fn (a, b) => if String.size a > String.size b then a else b) "" x

fun longest_string2 x =
	List.foldl (fn (a, b) => if String.size a >= String.size b then a else b) "" x

fun longest_string_helper f xs =
	List.foldl (fn (a, b) => if f(String.size a, String.size b) then a else b) "" xs

fun longest_string3 x =
	let
	  fun cmp(l, r) = l > r
	in
	  longest_string_helper cmp x
	end

fun longest_string4 x =
	let
	  fun cmp(l, r) = l >= r
	in
	  longest_string_helper cmp x
	end

fun longest_capitalized xs =
	let
	  fun chck xs =
	  	case xs of
			[] => [""]
		|	h::t => h::t
	in
		(longest_string3 o chck o only_capitals) xs
	end

fun rev_string s =
	(String.implode o List.rev o String.explode) s

fun first_answer f xs =
	case xs of
		[] => raise NoAnswer
	|	x::t => case f x of
					SOME ans => ans
				|	NONE => first_answer f t

fun all_answers f xs =
	let
	  fun calc rest acc =
	  	case rest of
			[] => SOME acc
		|	x::t => case f x of
					SOME a => calc t (a @ acc)
				|	NONE => NONE
	in
	  calc xs []
	end

fun count_wildcards p =
	g (fn _ => 1) (fn _ => 0) p

fun count_wild_and_variable_lengths p =
	g (fn _ => 1) (fn s => String.size s) p

fun count_some_var (s, p) =
	g (fn _ => 0) (fn x => if x = s then 1 else 0) p

fun check_pat pattern =
	let
	  fun ex_s pt =
	  	case pt of 
			Variable s => [s]
		|	TupleP ps => List.foldl (fn (a, b) => a @ b) [] (List.map ex_s ps)
		|	ConstructorP (_, np) => ex_s np
		|	_ => [] 

	  fun ch_u xs =
	  	case xs of
		  [] => true
		| x::t => not (List.exists (fn a => a = x) t)
	in
	  (ch_u o ex_s) pattern
	end

fun match (va, pat) =
	case pat of
		Wildcard => SOME []
	|	Variable s => SOME [(s, va)]
	|	UnitP => (case va of
				Unit => SOME []
			|	_ => NONE)
	|	ConstP s => (case va of
					Const v => if s = v then SOME [] else NONE
				|	_ => NONE)
	|	ConstructorP (s1, p) => (case va of
								Constructor(s2, v) => if s1=s2 then match(v, p) else NONE
							|	_ => NONE)
	|	TupleP (ps) => (case va of
						Tuple(vs) => (all_answers match (ListPair.zipEq(vs, ps)) handle UnequalLengths => NONE)
					|	_ => NONE)
	 
fun first_match v ps =
	SOME (first_answer (fn p => match(v, p)) ps) handle NoAnswer => NONE