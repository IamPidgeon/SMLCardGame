fun same_string (s1 : string, s2 : string) =
    s1 = s2

fun all_except_option (str, lst) =
    let 
	fun match (lst, acc) =
	case lst of
	    x::lst' => if same_string(str,x) then match(lst',"matched"::acc) else match(lst',acc@[x])
	  | [] => ( case acc of 
			      "matched"::acc' => SOME acc'
			  | _ => NONE )
    in match(lst, [])
    end

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun get_substitutions1 (subs, s) =
    case subs of
	subL::subs' => ( case all_except_option(s, subL) of
			     NONE => get_substitutions1(subs', s)
			   | SOME x => x @ get_substitutions1(subs', s) ) 
							  
      | []  => []

fun get_substitutions2 (subs, s) =
    let 
	fun append_list (subs, acc)  =
	    case subs of
		subL::subs' => ( case all_except_option(s, subL) of
				     NONE => append_list(subs', acc)
				   | SOME x => append_list(subs', acc @ x) ) 				  
	      | []  => acc
    in append_list(subs, [])
end

fun similar_names (subs, {first=f,middle=m,last=l}) =
(* I chose to write a new function instead of reusing get_substitutions2 since this has better efficiency due to not having to run through the list twice in order to convert from an option to a list and again to a record *) 
    let 
	fun match_names (subs, acc, inner_acc, match_sub) =
		case subs of
		    (s::ss)::subs' => if same_string(f,s) 
				      then match_names(ss::subs', acc, inner_acc, true) 
				      else match_names(ss::subs', acc, inner_acc @ [{first=s, middle=m, last=l}], match_sub)
		  | []::subs' => if match_sub = true 
				 then match_names(subs', acc @ inner_acc, [], false)
				 else match_names(subs', acc, [], false)
	          | [] => acc
    in match_names(subs, [{first=f,middle=m,last=l}], [], false)
    end

fun card_color (c) =
    case c of
	(Clubs, _) => Black
      | (Spades,_) => Black
      | (Diamonds, _) => Red
      | (Hearts, _)  => Red

fun card_value (c) =
     case c of
	 (_,Num i) => i
       | (_, Jack) => 10
       | (_, Queen) => 10
       | (_, King) => 10
       | (_, Ace) => 11

fun remove_card (cs, c, e) =
    let fun run_cards (cs, acc, match) =
	case cs of
	    x::cs' => if x = c andalso match = false then run_cards(cs', acc, true) else run_cards(cs', acc @ [x], match)
	  | [] => if match = false then raise e else acc
    in run_cards(cs, [], false)
    end

fun all_same_color (cs) =
(* It wasn't clear what to do with an empty card list so I decided to evaluate that path as false here *)
    let 
	fun color (cs, current) =
	    case cs of
		c::cs' => ( case current of
			(* I think using an if statement below instead of andalso makes it tail recursive *)
			      SOME x => if card_color(c) = x then color(cs', current) else false
			    | NONE => color(cs', SOME (card_color(c))) )
	      | [] => true 
    in color(cs, NONE)
    end

fun sum_cards (cs) =
    let 
	fun tail_sum(cs,sum) =
	case cs of
	    c::cs' => tail_sum(cs', card_value(c) + sum)
	  | [] => sum
    in tail_sum(cs,0)
    end

(* this is a helper for score and score_challenge that takes the sum of the cards, a boolean same_color for if they are all the same color, and the goal *)
fun score_helper (sum, same_color, goal) =
	if sum > goal andalso same_color then (3 * (sum - goal)) div 2
	else if sum > goal then 3 * (sum - goal)
	else if sum <= goal andalso same_color then (goal - sum) div 2
	else goal - sum

fun score (cs, goal) =
    score_helper(sum_cards(cs), all_same_color(cs), goal)

fun officiate (cs, ms, goal) =
    let 
	fun move (cs, hcs, ms) =
	    case ms of
		Draw::ms' => ( case cs of
				   c::cs' => if sum_cards(c::hcs) >= goal 
					     then score(c::hcs, goal) 
					     else move(cs', c::hcs, ms')
				 | [] => score(hcs, goal) )
	      | (Discard card)::ms' => move(cs,remove_card(hcs, card, IllegalMove), ms')
	      | [] => score(hcs, goal)
    in move(cs, [], ms)
    end

(* creates a list of all possible sums using 1/11 values of Aces for min_sum and score_challenge functions *)
fun sums_list (cs) =
    let 
	fun num_aces (cs, aces) =
	    case cs of
		(_,Ace)::cs' => num_aces(cs', 1 + aces)
	      | c::cs' => num_aces(cs', aces)
	      | [] => aces

	fun make_list (sum, aces, sums) =
	    case aces of
		0 => sums
	      | _ => make_list(sum - 10, aces - 1, [sum-10] @ sums)
    in
	make_list(sum_cards(cs), num_aces(cs,0), [sum_cards(cs)])
    end

(* Returns the min sum of a list of sums. Used in officiate_challenge. *)
fun min_sum (sums, cs) =
    let 
	fun min_acc (sums, temp_min) =
	    case sums of
		sum::sums' => min_acc(sums', Int.min(temp_min, sum))
	     | [] => temp_min
    in min_acc(sums, sum_cards(cs))
    end

fun score_challenge (cs, goal) =
   let 
       val same_color = all_same_color(cs)
       fun min_score (sums, temp_min) =
	   case sums of
	       sum::sums' => min_score(sums', Int.min(temp_min, score_helper(sum, same_color, goal)))
	     | [] => temp_min
   in min_score(sums_list(cs), score_helper(sum_cards(cs), same_color, goal))
   end

fun officiate_challenge (cs, ms, goal) =
    let 
	fun move (cs, hcs, ms) =
	    case ms of
		Draw::ms' => ( case cs of
				   c::cs' => if min_sum(sums_list(c::hcs), cs) >= goal
					     then score_challenge(c::hcs, goal) 
					     else move(cs', c::hcs, ms')
				 | [] => score_challenge(hcs, goal) )
	      | (Discard card)::ms' => move(cs,remove_card(hcs, card, IllegalMove), ms')
	      | [] => score_challenge(hcs, goal)
    in move(cs, [], ms)
    end

