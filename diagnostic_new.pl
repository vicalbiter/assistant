:- op(800,xfx,'=>').

member_of(X, [X|_]).
member_of(X, [_|T]) :-
	member_of(X, T).

delete_element(_, [], []).
delete_element(X, [X|T], Y) :-
  delete_element(X, T, Y).
delete_element(X, [H|T], [H|Y]) :-
  delete_element(X, T, Y).

%------------------------------------
% Auxiliary functions for working with lists
%------------------------------------

% suffix(X, Y)
% Tells if X is a suffix of Y
suffix(Xs, Xs).
suffix(Xs, [_|Ys]) :-
	suffix(Xs, Ys).

% sublist(X, Y)
% Tells if X is a sublist of Y
sublist(Xs, Ys) :-
	prefix(Ps, Ys), suffix(Xs, Ps).
sublist(Xs, Ys) :-
	prefix(Xs, Ss), suffix(Ss, Ys).
sublist(Xs, Ys) :-
	prefix(Xs, Ys).
sublist(Xs, [_|Ys]) :-
	sublist(Xs, Ys).
sublist(Xs, AsXsBs) :-
	append(_, XsBs, AsXsBs),
	append(Xs, _, XsBs).
sublist(Xs, AsXsBs) :-
	append(AsXs, _, AsXsBs),
	append(_, Xs, AsXs).

% are_sublists(X, Y)
% X is a list of lists
% Tells if all lists inside X are sublists of Y
are_sublists([], _).
are_sublists([X|T], List) :-
	sublist(X, List),
	are_sublists(T, List),
	!.

%flatten_list
flatten_list([], []).
flatten_list([X|T], NL) :-
	flatten_list(T, L1),
	append(X, L1, NL).

%-----------------------------------
% Utilities for choosing a diagnostic
%-----------------------------------

% Build a list of tuples of the form <Diagnostic, #misplaced_items>
build_list_of_tuples([], []).
build_list_of_tuples([L|T1], [[L,Count]|T2]) :-
	count_placed_items(L, Count),
	build_list_of_tuples(T1, T2).

% Find the tuple <L, Count> with the minimum count inside a list of tuples
min_count([Min], Min).
min_count([[L1,Count1],[_,Count2]|T], Min) :-
	Count1 =< Count2,
	min_count([[L1, Count1]|T], Min).
min_count([[_,Count1],[L2,Count2]|T], Min) :-
	Count1 > Count2,
	min_count([[L2, Count2]|T], Min).

% Find the tuple <L, Count> with the maximum count inside a list of tuples
max_count([Max], Max).
max_count([[L1,Count1],[_,Count2]|T], Max) :-
	Count1 > Count2,
	max_count([[L1, Count1]|T], Max).
max_count([[_,Count1],[L2,Count2]|T], Max) :-
	Count1 =< Count2,
	max_count([[L2, Count2]|T], Max).

% Dilligence Heuristic - Count the number of items that the assistant put into the shelves
count_placed_items([], 0).
count_placed_items([misplace(_)|Diagnostic], X) :-
	count_placed_items(Diagnostic, Y),
	X is Y + 9,
	!.
count_placed_items([place(_)|Diagnostic], X) :-
	count_placed_items(Diagnostic, Y),
	X is Y + 10,
	!.
count_placed_items([_|T], X) :-
	count_placed_items(T, X).

% Laziness Heuristic

%-----------------------------------
% Diagnostic form to general form transformation
%-----------------------------------

% Transform all the actions inside a diagnostic

transformation_df_to_gf(D, DGF) :-
	transformation_df_to_nf(D, DNF),
	transformation_nf_to_gf(DNF, DGF).

% Transform a diagnostic from the form [shelfID1=>[Items], shelfID2=>[Items]...] into the general form [[Objects believed to be in shelf1], [Objects believed to be in shelf2],...]
transformation_nf_to_gf(D, DGF):-
	get_shelf1(D, S1),
	get_shelf2(D, S2),
	get_shelf3(D, S3),
	append([S1], [S2], Aux1),
	append(Aux1, [S3], DGF),
	!.

get_shelf1([], []).
get_shelf1([shelf1=>X|_], X).
get_shelf1([_|T], X) :-
	get_shelf1(T, X).
get_shelf2([], []).
get_shelf2([shelf2=>X|_], X).
get_shelf2([_|T], X) :-
	get_shelf2(T, X).
get_shelf3([], []).
get_shelf3([shelf3=>X|_], X).
get_shelf3([_|T], X) :-
	get_shelf3(T, X).

% Transform a diagnostic of the form [place(X), misplace(Y), move(shelfID)...] into the form [shelfID1=>[Items], shelfID2=>[Items]...]  
transformation_df_to_nf(L, D):-
	find_all_moves(L, Moves),
	flatten_action(L, FL),
	split_all(FL, Moves, D),
	!.

% Eliminates all prefixes "place" and "misplace" from a list of actions
flatten_action([], []).
flatten_action([place(X)|L], [X|A]) :-
	flatten_action(L, A).
flatten_action([misplace(X)|L], [X|A]) :-
	flatten_action(L, A).
flatten_action([move(X)|L], [move(X)|A]) :-
	flatten_action(L, A).

% Splits a list in DF into multiple lists in NF
split_all(_, [], []).
split_all(L, [move(X)|Moves], D) :-
	split(move(X), L, LM, RM),
	split_all(RM, Moves, D1),
	append([X=>LM], D1, D).

find_all_moves([], []).
find_all_moves([move(X)|L], [move(X)|Moves]) :-
	find_all_moves(L, Moves).
find_all_moves([_|L], Moves) :-
	find_all_moves(L, Moves).

split(X, L, LM, RM) :-
	leftmost(X, L, LM),
	rightmost(X, L, RM).

leftmost(X, [X|_], []).
leftmost(X, [H|T], [H|L]) :-
	leftmost(X, T, L).

rightmost(X, L, RM) :-
	reverse(L, L1),
	leftmost(X, L1, RM1),
	reverse(RM1, RM).


%-----------------------------------
% General form to diagnostic form transformation
%-----------------------------------
transformation_gf_to_df(ListGF, Ideal, ListDF) :-
	transformation_gf_to_df_all(ListGF, Ideal, 0, Aux1),
	clean_lists(Aux1, Aux2),
	flatten_list(Aux2, ListDF).

transformation_gf_to_df_all([], _, _, []).
transformation_gf_to_df_all([GF|ListGF], Ideal, X, [DF|ListDF]) :-
	Y is X + 1,
	transformation_gf_to_df_single(GF, Ideal, Y, DF),
	transformation_gf_to_df_all(ListGF, Ideal, Y, ListDF).

transformation_gf_to_df_single([], _, X, [move(Shelf)]) :-
	atom_concat(shelf, X, Shelf).
transformation_gf_to_df_single([Item|GF], Ideal, X, [place(Item)|DF]) :-
	get_shelf_id(Item, Ideal, 1, X),
	transformation_gf_to_df_single(GF, Ideal, X, DF).
transformation_gf_to_df_single([Item|GF], Ideal, X, [misplace(Item)|DF]) :-
	get_shelf_id(Item, Ideal, 1, IdealID),
	different_ids(IdealID, X),
	transformation_gf_to_df_single(GF, Ideal, X, DF).
	
%-------------------------------------
% Functions for list transformation
%-------------------------------------
different_ids(ID1, ID2) :-
	ID1 > ID2.
different_ids(ID1, ID2) :-
	ID1 < ID2.

% get_shelf_id(Item, Shelves, InitialCounter, ID).
% Gets the shelf ID in which the item is placed, given a list of Shelves (InitialCounter must be always set to 1 when calling this function.
get_shelf_id(_, [], _, 0).
get_shelf_id(X, [IShelf|_], ID, ID) :-
	member_of(X, IShelf).
get_shelf_id(X, [IShelf|Shelves], CurrentShelf, ID) :-
	not(member_of(X, IShelf)),
	NextShelf is CurrentShelf + 1,
	get_shelf_id(X, Shelves, NextShelf, ID).

% Clean lists (intermediate step between GF to DF transformation)
clean_lists([], []).
clean_lists([A|L1], [B|L2]) :-
	clean_obs(A, B),
	clean_lists(L1, L2),
	!.

clean_obs([move(_)], []).
clean_obs(L, L).


%-------------------------------------
% Diagnostic generation
%-------------------------------------

% Diagnostic Form: [action1(item1), action2(item2)...actionN(itemN)]
% General Form: [[Items in shelf1], [Items in shelf2], [Items in shelf3]]

% Build a complete diagnostic
diagnostic(Items, NShelves, Ideal, ObsGF, ObsShelves, Diagnostic) :-
	findall(D, suggest_diagnostic_df_with_obs(Items, NShelves, Ideal, ObsGF, ObsShelves, D), LD),
	build_list_of_tuples(LD, LDH),
	max_count(LDH, [Diagnostic,_]).

% Given a list of items, a list of reported places for those items in GF, a list of observed places for those items in GF, and a list of the shelves that have already been observed, suggest a diagnostic in DF
suggest_diagnostic_df_with_obs(Items, NShelves, Ideal, ObsGF, ObsShelves, Diagnostic) :-
	suggest_diagnostic_gf_with_obs(Items, NShelves, ObsGF, ObsShelves, DGF),
	transformation_gf_to_df(DGF, Ideal, Diagnostic).

% Given a list of items, and a list of reported places for those items in GF, suggest a diagnostic in DF.
suggest_diagnostic_df(Items, NShelves, Ideal, Diagnostic) :-
	suggest_diagnostic_gf(Items, NShelves, DGF),
	transformation_gf_to_df(DGF, Ideal, Diagnostic).

% Given a list of items, a list (GF) of observed items, and a list of the shelves that have already been visited, suggest a diagnostic in general form
suggest_diagnostic_gf_with_obs(Items, NShelves, ObsGF, ObsShelves, DiagnosticGF) :-
	suggest_diagnostic_gf(Items, NShelves, DiagnosticGF),
	matches_observations(ObsGF, ObsShelves, DiagnosticGF).

% Make sure that, if a shelf has been observed (the observations match the diagnostic)
matches_observations([], [], []).
matches_observations([_|ObsGF], [0|ObsShelves], [_|DGF]) :-
	matches_observations(ObsGF, ObsShelves, DGF).
matches_observations([OShelf|ObsGF], [1|ObsShelves], [DShelf|DGF]) :-
	lists_are_equal(OShelf, DShelf),
	matches_observations(ObsGF, ObsShelves, DGF).

lists_are_equal(L1, L2) :-
	all_elements_in(L1, L2),
	all_elements_in(L2, L1).

all_elements_in([], _).
all_elements_in([X|T], L) :-
	member_of(X, L),
	all_elements_in(T, L).
	
% suggest_diagnostic_gf(Items, N, DiagnosticGF)
% Given a list of Items and a number N of shelves, suggest a diagnostic in general form
suggest_diagnostic_gf(Items, NShelves, DiagnosticGF) :-
	generate_list_of_items_placement(Items, NShelves, ItemsPlacement),
	place_items_in_shelves(Items, 1, NShelves, ItemsPlacement, DiagnosticGF).

% Distribute all Items across the N shelves, according to a placement list.
place_items_in_shelves(Items, NShelves, NShelves, IPlacement, [Shelf]) :-
	place_items_in_shelf(NShelves, Items, IPlacement, Shelf).
place_items_in_shelves(Items, CurrentShelfID, NShelves, IPlacement, [Shelf|Shelves]) :-
	place_items_in_shelf(CurrentShelfID, Items, IPlacement, Shelf),
	NextShelfID is CurrentShelfID + 1,
	NextShelfID =< NShelves,
	place_items_in_shelves(Items, NextShelfID, NShelves, IPlacement, Shelves).

place_items_in_shelf(_, [], [], []).
place_items_in_shelf(ID, [Item|Items], [ID|Shelves], [Item|Shelf]) :-
	place_items_in_shelf(ID, Items, Shelves, Shelf).
place_items_in_shelf(ID, [_|Items], [OtherID|Shelves], Shelf) :-
	different_ids(ID, OtherID),
	place_items_in_shelf(ID, Items, Shelves, Shelf). 

%generate_list(ListOfItems, N, ListOfShelves)
% Generation of all list of #Items elements containing numbers between 0 and N. Each element in this list corresponds to the shelf in which the element in the list of items was placed.
generate_list_of_items_placement([], _, []).
generate_list_of_items_placement([_|T], NShelves, [X|L]) :-
	pick_number_less_than(X, NShelves),
	generate_list_of_items_placement(T, NShelves, L).

pick_number_less_than(X, N) :-
	pick_number(X),
	X =< N.

pick_number(0).	%Item wasn't placed in any shelf
pick_number(1). %Item was placed in shelf 1
pick_number(2). %Item was placed in shelf 2
pick_number(3). %...
pick_number(4). %...
pick_number(5). %...









