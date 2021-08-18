(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun all_except_option(s, xs) =
    case xs of
        [] => NONE
|   x::tail => if x=s then SOME tail else                                                         
                                            case all_except_option(s, tail) of
                                                SOME tail => SOME (x::tail)
                                                |  NONE => NONE
                                                            

fun get_substitutions1(xss: string list list, s: string) =
    case xss of
        [] => []
    |   x::tail => case all_except_option(s, x) of
                        NONE => get_substitutions1(tail, s)
                    |   SOME sub => sub @ get_substitutions1(tail, s)
                    

fun get_substitutions2(xss: string list list, s: string) =
    let fun l(hss: string list list, acc: string list) =
            case hss of
                [] => acc
            |   x::tail => case all_except_option(s, x) of
                                NONE => l(tail, acc)
                            |   SOME meow => l(tail, (meow @ acc))
    in
      l(xss, [])
    end 


fun similar_names(xss: string list list, n: {first:string,middle:string,last:string}) =
    case n of
        {first=first,middle=middle,last=last} => let
          fun assembler(ss: string list) =
            case ss of
                [] => []
            |   x::tail => {first=x,middle=middle,last=last} :: assembler(tail)
        in
          assembler(first::get_substitutions1(xss, first))
        end

        
fun card_color(x, n) =
    case x of
        Spades => Black
    |   Clubs => Black
    |   Diamonds => Red
    |   Hearts => Red

fun card_value(n, x) =
    case x of
        Num a => a
    |   Ace => 11
    |   _ => 10

fun remove_card(cs: card list, c: card, e) =
    let
      val l = all_except_option(c, cs)
    in
      case l of
        NONE => raise e
    |   SOME a => a
    end

fun all_same_color(cs: card list) =
    case cs of
        [] => true
    |   hd::[] => true
    |   a::b::tail => case all_same_color(b::tail) of
                            false => false
                        |   true => card_color(a) = card_color(b)

fun sum_cards(cards: card list) =
    let
        fun r(cs: card list, acc: int) =
            case cs of
                [] => acc
            |   x::tl => r(tl, acc + card_value(x))
    in
        r(cards, 0)
    end

fun score(cs: card list, goal: int) =
    let
      val c_s = sum_cards(cs)
      val pre_score = if c_s > goal then 3 * (c_s - goal) else (goal - c_s)
    in
        if all_same_color(cs)
        then pre_score div 2
        else pre_score
    end

fun officiate(start_cards: card list, start_moves: move list, goal: int) =
    let
        fun proc(hand: card list, deck: card list, moves: move list) =
            case moves of
                [] => score(hand, goal)
            |   mv::moves_rest => case mv of
                    Discard x => proc(remove_card(hand, x, IllegalMove), deck, moves_rest)
                |   Draw => case deck of
                        [] => score(hand, goal)
                    |   d::deck_rest => if sum_cards(d::hand) > goal then score(d::hand, goal) else proc(d::hand, deck_rest, moves_rest)
    in
      proc([], start_cards, start_moves)
    end

    