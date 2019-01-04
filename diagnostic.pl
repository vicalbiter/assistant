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


%------------------------------------
% Auxiliary Functions for diagnostic construction
%------------------------------------

% pick_action_or_move
pick_action_or_move([place(X)|_], place(X)).
%pick_action_or_move([misplace(X)|_], misplace(X)).
pick_action_or_move([move(X)|_], move(X)).
pick_action_or_move([_|T], Action) :-
	pick_action_or_move(T, Action).

% pick_action
pick_action([place(X)|_], place(X)).
%pick_action([misplace(X)|_], misplace(X)).
pick_action([_|T], Action) :-
	pick_action(T, Action).

% Eliminate an action from the list of posibilities
eliminate_action_from_posibilities(_, [], []).
eliminate_action_from_posibilities(place(X), S, NewS) :-
	delete_element(place(X), S, NewS).
%	delete_element(misplace(X), S1, NewS).
%eliminate_action_from_posibilities(misplace(X), S, NewS) :-
%	delete_element(place(X), S, S1),
%	delete_element(misplace(X), S1, NewS).
eliminate_action_from_posibilities(move(X), S, NewS) :-
	delete_element(move(X), S, NewS).


% Suggest a legal action, given a list of posible actions and a list of actions that have already been done until now
legal_action(Action, Posibilities, []) :-
	pick_action(Posibilities, Action).
legal_action(Action, Posibilities, [LA|PD]) :-
	not(recentlymoved(LA)),
	pick_action_or_move(Posibilities, Action),
	validate(Action, [LA|PD]).
legal_action(Action, Posibilities, [LA|PD]) :-
	recentlymoved(LA),
	pick_action(Posibilities, Action),
	validate(Action, [LA|PD]).

% Validate if an action is valid, given a list of actions that have already been done until now
validate(place(X), PrevDiagnostic) :-
	not(member_of(place(X), PrevDiagnostic)).
%	not(member_of(misplace(X), PrevDiagnostic)).
%validate(misplace(X), PrevDiagnostic) :-
%	not(member_of(place(X), PrevDiagnostic)),
%	not(member_of(misplace(X), PrevDiagnostic)).
validate(move(X), PrevDiagnostic) :-
	not(member_of(move(X), PrevDiagnostic)).

recentlymoved(LA):-
	member_of(LA, [move(shelf1), move(shelf2), move(shelf3)]).

% Check if the last action performed by the robot was a move
last_action_was_move([move(_)|[]]).
last_action_was_move([_|Diagnostic]) :-
	last_action_was_move(Diagnostic).

% Build a list of tuples of the form <Diagnostic, #misplaced_items>
build_list_of_tuples([], []).
build_list_of_tuples([L|T1], [[L,Count]|T2]) :-
	count_misplaced_items(L, Count),
	build_list_of_tuples(T1, T2).

% Find the tuple <L, Count> with the minimum count inside a list of tuples
min_count([Min], Min).
min_count([[L1,Count1],[_,Count2]|T], Min) :-
	Count1 =< Count2,
	min_count([[L1, Count1]|T], Min).
min_count([[_,Count1],[L2,Count2]|T], Min) :-
	Count1 > Count2,
	min_count([[L2, Count2]|T], Min).


%-----------------------------------
% Heuristics for choosing a diagnostic
%-----------------------------------

% Count the number of diagnosed misplaced items in a diagnosis
count_misplaced_items([], 0).
count_misplaced_items([misplace(_)|Diagnostic], X) :-
	count_misplaced_items(Diagnostic, Y),
	X is Y + 1,
	!.
count_misplaced_items([_|T], X) :-
	count_misplaced_items(T, X).

% Only moves left
only_moves_left([], yes).
only_moves_left([move(_)|Posibilities], R) :-
	only_moves_left(Posibilities, R).
only_moves_left([place(_)|_], no).
only_moves_left([misplace(_)|_], no).

%suggest_diagnostics
suggest_diagnostics(Actions, Diagnostic) :-
	append([move(shelf1), move(shelf2), move(shelf3)], Actions, Pos),
	suggest_diagnostic(Pos, [], Diagnostic).
suggest_diagnostics(Actions, Diagnostic) :-
	append([move(shelf1), move(shelf2)], Actions, Pos),
	suggest_diagnostic(Pos, [], Diagnostic).
suggest_diagnostics(Actions, Diagnostic) :-
	append([move(shelf2), move(shelf3)], Actions, Pos),
	suggest_diagnostic(Pos, [], Diagnostic).
suggest_diagnostics(Actions, Diagnostic) :-
	append([move(shelf1), move(shelf3)], Actions, Pos),
	suggest_diagnostic(Pos, [], Diagnostic).
suggest_diagnostics(Actions, Diagnostic) :-
	append([move(shelf1)], Actions, Pos),
	suggest_diagnostic(Pos, [], Diagnostic).
suggest_diagnostics(Actions, Diagnostic) :-
	append([move(shelf2)], Actions, Pos),
	suggest_diagnostic(Pos, [], Diagnostic).
suggest_diagnostics(Actions, Diagnostic) :-
	append([move(shelf3)], Actions, Pos),
	suggest_diagnostic(Pos, [], Diagnostic).

%------------------------------------
% Diagnostic construction
%------------------------------------

% Build a list of diagnostics
complete_diagnostic(Posibilities, Observations, Ideal, Diagnostic) :-
	findall(D, clean_diagnostic(Posibilities, Observations, Ideal, D), LD),
	build_list_of_tuples(LD, LDH),
	min_count(LDH, [Diagnostic,_]).

% Suggest a diagnostic, given a list of posible actions and a previous diagnostic (this function generates a feasible path of the diagnostic tree), and it is meant to be called in conjunction with findall/3 in order to get all posible diagnostics.
suggest_diagnostic([], _, []).	% Use this if you want to empty all posibilities before suggesting a diagnostic
%suggest_diagnostic(Posibilities, _, []) :-
%	only_moves_left(Posibilities, yes).
suggest_diagnostic(Posibilities, PrevDiagnostic, [Action|Diagnostic]) :-
	legal_action(Action, Posibilities, PrevDiagnostic),
	eliminate_action_from_posibilities(Action, Posibilities, NewPos),
	suggest_diagnostic(NewPos, [Action|PrevDiagnostic], Diagnostic).

% Build a feasible diagnostic, given the robot's knowledge of the world
clean_diagnostic(Posibilities, Observations, Ideal, TDiagnostic) :-
	suggest_diagnostic(Posibilities, [], Diagnostic),
%	suggest_diagnostics(Posibilities, Diagnostic),
	last_action_was_move(Diagnostic),
%	no_placement_inconsistencies(Diagnostic, Ideal),
	transform_placement_inconsistencies(Diagnostic, Ideal, TDiagnostic),
	build_and_clean_all_observations_lists(Observations, Ideal, 0, OList),
	are_sublists(OList, TDiagnostic).



%------------------------------------
% Diagnostic validation
%------------------------------------

no_placement_inconsistencies(Diagnostic, Ideal) :-
	transform_diagnostic(Diagnostic, DGF),
	validate_all_diagnosed_actions(Diagnostic, Ideal, DGF).
	
% Validate diagnosed actions (place vs misplace) by checking if the action is consistent with the ideal and the diagnosed location of the item being placed/misplaced
validate_all_diagnosed_actions([], _, _).
validate_all_diagnosed_actions([A|T], Ideal, DGF) :-
	validate_diagnosed_action(A, Ideal, DGF),
	validate_all_diagnosed_actions(T, Ideal, DGF).

validate_diagnosed_action(place(X), Ideal, DGF) :-
	get_shelf_id(X, Ideal, IdealID),
	get_shelf_id(X, DGF, IdealID).
validate_diagnosed_action(misplace(X), Ideal, DGF) :-
	get_shelf_id(X, Ideal, IdealID),
	get_shelf_id(X, DGF, DiagnosedID),
	different_ids(IdealID, DiagnosedID).
validate_diagnosed_action(move(_), _, _).

different_ids(ID1, ID2) :-
	ID1 > ID2.
different_ids(ID1, ID2) :-
	ID1 < ID2.

get_shelf_id(X, [Shelf1, _, _], 1) :-
	member_of(X, Shelf1).
get_shelf_id(X, [_, Shelf2, _], 2) :-
	member_of(X, Shelf2).
get_shelf_id(X, [_, _, Shelf3], 3) :-
	member_of(X, Shelf3).

%------------------------------------
% Diagnostic transformation
%------------------------------------

% Transform all placement inconsistencies from a Diagnostic (i.e. if the diagnostic contains an action place(X) but the shelf of X is incorrect, transform that action into misplace(X)
transform_placement_inconsistencies(Diagnostic, Ideal, TD) :-
	transform_diagnostic(Diagnostic, DGF),
	transform_diagnosed_actions(Diagnostic, Ideal, DGF, TD).

transform_diagnosed_actions([], _, _, []).
transform_diagnosed_actions([A|T], Ideal, DGF, [TA|TD]) :-
	transform_diagnosed_action(A, Ideal, DGF, TA),
	transform_diagnosed_actions(T, Ideal, DGF, TD).

transform_diagnosed_action(place(X), Ideal, DGF, place(X)) :-
	get_shelf_id(X, Ideal, IdealID),
	get_shelf_id(X, DGF, IdealID).
transform_diagnosed_action(place(X), Ideal, DGF, misplace(X)) :-
	get_shelf_id(X, Ideal, IdealID),
	get_shelf_id(X, DGF, DiagnosedID),
	different_ids(IdealID, DiagnosedID).
transform_diagnosed_action(move(X), _, _, move(X)).


% Transform all the actions inside a diagnostic

transform_diagnostic(D, DGF) :-
	transform_diagnostic_normal_form(D, DNF),
	transform_diagnostic_general_form(DNF, DGF).

% Transform a diagnostic from the form [shelfID1=>[Items], shelfID2=>[Items]...] into the general form [[Objects believed to be in shelf1], [Objects believed to be in shelf2],...]
transform_diagnostic_general_form(D, DGF):-
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
transform_diagnostic_normal_form(L, D):-
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
% Observations Transformation
%-----------------------------------

% Build and clean observations lists
build_and_clean_all_observations_lists(Obs, Ideal, X, NL) :-
	build_all_observations_lists(Obs, Ideal, X, L1),
	clean_observations_lists(L1, NL).

% Build a list of lists from the observations that the robot has made in all shelves
build_all_observations_lists([], _, _, []).
build_all_observations_lists([L|Obs], [I|Ideal], X, [NL|List]) :-
	Y is X + 1,	
	transform_observation_list(L, I, L1),
	atom_concat(shelf, Y, Shelf),
	append(L1, [move(Shelf)], NL),
	build_all_observations_lists(Obs, Ideal, Y, List).

% Build a list from the observations that the robot has made in one shelf, that can be compared against a diagnostic
% Transforms a list of the form [Objects believed to be in shelfN] into a list of actions
transform_observation_list([], _, []).
transform_observation_list([X|Obs], Ideal, [place(X)|List]) :-
	member_of(X, Ideal),
	transform_observation_list(Obs, Ideal, List).
transform_observation_list([X|Obs], Ideal, [misplace(X)|List]) :-
	not(member_of(X, Ideal)),
	transform_observation_list(Obs, Ideal, List).

% Clean observations lists
clean_observations_lists([], []).
clean_observations_lists([A|L1], [B|L2]) :-
	clean_obs(A, B),
	clean_observations_lists(L1, L2),
	!.

clean_obs([move(_)], []).
clean_obs(L, L).











