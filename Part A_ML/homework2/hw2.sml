(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (str, strlist) =
    case strlist of
	[] => NONE
      | head::tail => if same_string(str, head)
		      then SOME tail
		      else case all_except_option(str, tail) of
			       NONE => NONE
			     | SOME strlist' => SOME(head::strlist')

fun get_substitutions1 (sub, s) =
    case sub of
	[] => []
      | head::tail => case all_except_option(s, head) of
			  NONE => get_substitutions1(tail, s)
			| SOME strlist => strlist @ get_substitutions1(tail, s)
    
fun get_substitutions2 (sub, s) =
    let fun f(sub, acc) =
	    case sub of
		[] => acc
	      | head::tail => case all_except_option(s, head) of
				  NONE => f(tail, acc)
				| SOME strlist => f(tail, acc @ strlist)
    in f(sub, []) end

fun similar_names (sub, {first=a, middle=b, last=c}) =
    let fun f(aux) =
	    case aux of
		[] => []
	      | head::tail => {first=head, middle=b, last=c}::f(tail)
    in f(a::get_substitutions2(sub, a)) end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color (suit, rank) =
    case suit of
	Clubs => Black
      | Spades => Black
      | Diamonds => Red
      | Heart => Red

fun card_value (suit, rank) =
    case rank of
	Num i => i
      | Ace => 11
      | _ => 10

fun remove_card (cs, c, e) =
    case cs of
	[] => []
      | head::tail => if head = c
		      then tail
		      else case remove_card(tail, c, e) of
			       [] => raise e
			     | cs'  => head::cs'

fun all_same_color (cs) =
    case cs of
	[] => true
      | head::[] => true
      | head::(second::rest) => if (card_color(head) = card_color(second))
				then true andalso all_same_color(second::rest)
				else false

fun sum_cards (cs) =
    let fun helper(cs, acc) =
	    case cs of
		[] => acc
	      | head::tail => helper(tail,acc + card_value(head))
    in helper(cs, 0) end

fun score (hcs, goal) =
    let val sum = sum_cards(hcs)
	fun pscore (sum) = if sum > goal
			   then 3 * (sum - goal)
			   else (goal - sum)
    in
	case (all_same_color hcs) of
	    true => pscore(sum) div 2
	  | false => pscore(sum)
    end

fun officiate (card_list, move_list, goal) =
    let fun helper (card_list, held_cards, move_list) =
	    case move_list of
		[] => score(held_cards, goal) (*Game Ends with move_list empty*)

		(*Player discards c and play continues*)
	      | (Discard c)::tail_move => (case held_cards of
					       [] => raise IllegalMove (*If c is not in the held_cards*)
					     | _ => helper(card_list, remove_card(held_cards, c, IllegalMove), tail_move))
		
		(*Player draws*)
	      | Draw::tail_move => case card_list of
				       [] => score(held_cards, goal) (*Game Ends with player draws while card_list is empty*)
				     | head::_ => if sum_cards(head::held_cards) > goal
						  then score(head::held_cards, goal) (*Game Ends when held_card > goal after drawing*)
						  else helper(remove_card(card_list, head, IllegalMove), head::held_cards, tail_move) (*Continues when card_list is not empty*)
    in helper(card_list, [], move_list) end
	

