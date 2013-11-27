use "HW2PeerReview-3.sml";

(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1a_1 = all_except_option("string", ["string"]) = SOME []
val test1a_2 = all_except_option("string", []) = NONE
val test1a_3 = all_except_option("string", ["string", "test1"]) = SOME ["test1"]
val test1a_4 = all_except_option("string", ["test1", "string"]) = SOME ["test1"]
val test1a_5 = all_except_option("string", ["test1", "test2", "string"]) = SOME ["test1", "test2"]
val test1a_6 = all_except_option("string", ["test1", "test2", "string", "test3"]) = SOME ["test1", "test2", "test3"]
val test1a_7 = all_except_option("string", ["string2", "test1", "test2", "string", "test3"]) = SOME ["string2", "test1", "test2", "test3"]
val test1a_8 = all_except_option("newstring", ["test1", "test2", "test3"]) = NONE
val test1a_9 = all_except_option("", ["test1", "test2", "test3"]) = NONE

val test1b_1 = get_substitutions1([["foo"],["there"]], "foo") = [] 
val test1b_2 = get_substitutions1([["foo"],["there"]], "there") = [] 
val test1b_3 = get_substitutions1([["foo", "chow"],["there"]], "foo") = ["chow"]
val test1b_4 = get_substitutions1([["chow","foo", "face"],["there"]], "foo") = ["chow", "face"]
val test1b_5 = get_substitutions1([["chow","foo", "face"],["there", "foo"]], "foo") = ["chow", "face", "there"]
val test1b_6 = get_substitutions1([], "foo") = []
val test1b_7 = get_substitutions1([["chow","foo", "face"],["there"]], "") = []
val test1b_8 = get_substitutions1([], "") = []
val test1b_9 = get_substitutions1([["chow", "face", "foo"],["there", "foo"]], "foo") = ["chow", "face", "there"]
val test1b_10 = get_substitutions1([["therefore"],["there"]], "there") = [] 

val test1c_1 = get_substitutions2([["foo"],["there"]], "foo") = [] 
val test1c_2 = get_substitutions2([["foo"],["there"]], "there") = [] 
val test1c_3 = get_substitutions2([["foo", "chow"],["there"]], "foo") = ["chow"]
val test1c_4 = get_substitutions2([["chow","foo", "face"],["there"]], "foo") = ["chow", "face"]
val test1c_5 = get_substitutions2([["chow","foo", "face"],["there", "foo"]], "foo") = ["chow", "face", "there"]
val test1c_6 = get_substitutions2([], "foo") = []
val test1c_7 = get_substitutions2([["chow","foo", "face"],["there"]], "") = []
val test1c_8 = get_substitutions2([], "") = []
val test1c_9 = get_substitutions2([["chow", "face", "foo"],["there", "foo"]], "foo") = ["chow", "face", "there"]
val test1c_10 = get_substitutions2([["therefore"],["there"]], "there") = [] 

val test1d_1 = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]
val test1d_2 = similar_names([], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}]
val test1d_3 = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="", middle="W", last="Smith"}) =
	    [{first="", last="Smith", middle="W"}]
val test1d_4 = similar_names([["Fred"],["Elizabeth","Betty"],["Fred"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}]
val test1d_5 = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="John", middle="", last="Smith"}) =
	    [{first="John", last="Smith", middle=""}]
val test1d_6 = similar_names([["Freddie","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Freddie", last="Smith", middle="W"},
	     {first="F", last="Smith", middle="W"}]
val test1d_7 = similar_names([["Fred","Fredrick"], ["Fred","Fredrick"], ["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"}, 
	     {first="Fredrick", last="Smith", middle="W"}, {first="Freddie", last="Smith", middle="W"}, 
	     {first="F", last="Smith", middle="W"}]

val test2a_1 = card_color((Clubs, Num 2)) = Black
val test2a_2 = card_color((Spades, Num 9)) = Black
val test2a_3 = card_color((Diamonds, King)) = Red
val test2a_4 = card_color((Hearts, Ace)) = Red

val test2b_1 = card_value((Clubs, Num 2)) = 2
val test2b_2 = card_value((Spades, Num 9)) = 9
val test2b_3 = card_value((Clubs, Queen)) = 10
val test2b_4 = card_value((Spades, Ace)) = 11

val test2c_1 = remove_card([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = [] 
val test2c_2 = remove_card([(Hearts, Ace), (Spades, Ace), (Hearts, Ace)], (Hearts, Ace), IllegalMove) = [(Spades, Ace), (Hearts, Ace)]
val test2c_3 = remove_card([(Hearts, Ace), (Hearts, Ace), (Spades, Ace)], (Hearts, Ace), IllegalMove) = [(Hearts, Ace), (Spades, Ace)]
val test2c_4 = remove_card([(Spades, Ace), (Hearts, Ace), (Hearts, Ace), (Spades, Ace)], (Spades, Ace), IllegalMove) = [(Hearts, Ace), (Hearts, Ace), (Spades, Ace)]
val test2c_5 = remove_card([(Hearts, Ace), (Hearts, Ace), (Spades, Ace)], (Hearts, Ace), IllegalMove) = [(Hearts, Ace), (Spades, Ace)]
val test2c_6 = remove_card([(Hearts, Ace), (Hearts, King), (Clubs, King)], (Clubs, King), IllegalMove) = [(Hearts, Ace), (Hearts, King)]
val test2c_7 = (( remove_card([(Clubs,Jack),(Spades,Num(8))], (Hearts,Jack), IllegalMove); false) handle IllegalMove => true)
val test2c_8 = (( remove_card([], (Hearts,Jack), IllegalMove); false) handle IllegalMove => true)



val test2d_1 = all_same_color([(Hearts, Ace), (Hearts, Ace)]) = true
val test2d_2 = all_same_color([(Hearts, Ace), (Hearts, King)]) = true
val test2d_3 = all_same_color([(Hearts, Ace), (Diamonds, Ace)]) = true
val test2d_4 = all_same_color([(Spades, Jack), (Spades, Queen), (Clubs, King)]) = true
val test2d_5 = all_same_color([(Hearts, Jack), (Spades, Queen), (Clubs, King)]) = false
val test2d_6 = all_same_color([(Spades, Jack), (Diamonds, Queen), (Clubs, King)]) = false
val test2d_7 = all_same_color([(Spades, Jack), (Spades, Queen), (Hearts, King)]) = false
val test2d_8 = all_same_color([(Spades, Ace)]) = true
val test2d_9 = all_same_color([]) = true

val test2e_1 = sum_cards([(Clubs, Num 2),(Clubs, Num 2)]) = 4
val test2e_2 = sum_cards([]) = 0
val test2e_3 = sum_cards([(Clubs, Ace)]) = 11
val test2e_4 = sum_cards([(Clubs, King),(Hearts, Queen)]) = 20
val test2e_5 = sum_cards([(Hearts, King),(Hearts, King)]) = 20
val test2e_6 = sum_cards([(Clubs, Ace),(Hearts, Num 10), (Hearts, Queen)]) = 31

val test2f_1 = score([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test2f_2 = score([(Hearts, Num 2),(Clubs, Ace)],10) = 9
val test2f_3 = score([(Hearts, Num 2),(Clubs, Num 10), (Clubs, King)],10) = 36
val test2f_4 = score([(Hearts, Num 2),(Clubs, Num 10), (Clubs, King)],30) = 8
val test2f_5 = score([(Hearts, Num 2),(Diamonds, Num 4)],10) = 2
val test2f_6 = score([(Hearts, Num 2),(Diamonds, Ace)],10) = 4
val test2f_7 = score([(Hearts, Num 2),(Hearts, Num 10), (Diamonds, King)],10) = 18
val test2f_8 = score([(Hearts, Num 3),(Hearts, Num 10), (Diamonds, King)],23) = 0
val test2f_9 = score([(Spades, Num 3),(Hearts, Num 10), (Diamonds, King)],23) = 0





val test2g_1 = officiate([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6
val test2g_2 = officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                       [Draw,Draw,Draw,Draw,Draw], 42) = 3


val test2g_3 = officiate([(Hearts, Num 2),(Clubs, Num 4)],[], 15) = 7


val test2g_4 = officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Clubs,Jack),Draw], 42) = 17

val test2g_5 = officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                       [Draw,Draw,Draw,Discard(Clubs,Ace),Draw], 42) = 4



val test2g_6 = officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                       [Draw,Discard(Clubs,Ace),Draw,Draw,Discard(Spades,Ace),Draw], 42) = 10

val test2g_7 = officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                       [Draw,Draw,Draw,Draw,Draw,Discard(Spades,Ace)], 52) = 4
val test2g_8 = officiate([],[Draw], 15) = 7
val test2g_9 = (( officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)], 42); false) handle IllegalMove => true)
val test2g_10 = (( officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Clubs,Jack),Discard(Spades,Jack)], 42); false) handle IllegalMove => true)
val test2g_11 = officiate([],[], 0) = 0


(*
val test3a_1 = score_challenge([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test3a_2 = score_challenge([(Hearts, Num 2),(Clubs, Ace)],10) = 7
val test3a_3 = score_challenge([(Hearts, Num 2),(Clubs, Num 10), (Clubs, King)],10) = 36
val test3a_4 = score_challenge([(Hearts, Num 2),(Clubs, Num 10), (Clubs, King)],30) = 8
val test3a_5 = score_challenge([(Hearts, Num 2),(Diamonds, Num 4)],10) = 2
val test3a_6 = score_challenge([(Hearts, Num 2),(Diamonds, Ace), (Hearts, Num 3)],10) = 2
val test3a_7 = score_challenge([(Hearts, Num 2),(Hearts, Num 10), (Diamonds, King)],10) = 18
val test3a_8 = score_challenge([(Hearts, Num 2),(Diamonds, Ace), (Hearts, Num 3)],30) = 7
val test3a_9 = score_challenge([(Spades, Num 2),(Diamonds, Ace), (Diamonds, Ace), (Hearts, Num 3)],18) = 1
val test3a_10 = score_challenge([(Hearts, Num 2),(Diamonds, Ace), (Diamonds, Ace), (Diamonds, Ace), (Hearts, Num 3)],18) = 0

val test3b_1 = officiate_challenge([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6
val test3b_2 = officiate_challenge([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                       [Draw,Draw,Draw,Draw,Draw], 42) = 3
val test3b_3 = officiate_challenge([(Hearts, Num 2),(Clubs, Num 4)],[], 15) = 7
val test3b_4 = officiate_challenge([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Clubs,Jack),Draw], 42) = 17
val test3b_5 = officiate_challenge([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                       [Draw,Draw,Draw,Discard(Clubs,Ace),Draw], 42) = 4
val test3b_6 = officiate_challenge([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                       [Draw,Discard(Clubs,Ace),Draw,Draw,Discard(Spades,Ace),Draw], 42) = 10
val test3b_7 = officiate_challenge([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                       [Draw,Draw,Draw,Draw,Draw,Discard(Spades,Ace)], 52) = 4
val test3b_8 = officiate_challenge([],[Draw], 15) = 7
val test3b_9 = (( officiate_challenge([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)], 42); false) handle IllegalMove => true)
val test3b_10 = (( officiate_challenge([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Clubs,Jack),Discard(Spades,Jack)], 42); false) handle IllegalMove => true)
val test3b_11 = officiate_challenge([],[], 0) = 0
val test3b_12 = officiate_challenge([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                       [Draw,Draw,Draw,Draw,Draw], 20) = 3
val test3b_13 = officiate_challenge([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                       [Draw,Draw,Draw,Draw,Draw], 40) = 3
val test3b_14 = officiate_challenge([],[], 5) = 2

*)
