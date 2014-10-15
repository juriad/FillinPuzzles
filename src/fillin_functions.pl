:- module(fillin_functions, [puzzle_map_fold/5, puzzle_map/3, set_tile/3, puzzle_fold/4, get_tile/3, puzzle_transpose/2]).
:- meta_predicate puzzle_map(?, 2, ?).
:- meta_predicate puzzle_fold(?, 3, ?, ?).
:- meta_predicate puzzle_map_fold(?, 4, ?, ?, ?).

% MAP_FOLD

puzzle_map_fold([], _, Result, [], Result).
puzzle_map_fold([PRow | PRows], Func, Start, [PRow2 | PRows2], Result) :-
	puzzle_map_fold_tiles(PRow, Func, Start, PRow2, Start2),
	puzzle_map_fold(PRows, Func, Start2, PRows2, Result).

puzzle_map_fold_tiles([], _, Result, [], Result).
puzzle_map_fold_tiles([PTile | PTiles], Func, Start, [PTile2 | PTiles2], Result) :-
	call(Func, PTile, Start, PTile2, Start2),
	puzzle_map_fold_tiles(PTiles, Func, Start2, PTiles2, Result).

% MAP

puzzle_map([], _, []).
puzzle_map([PRow | PRows], Func, [PRow2 | PRows2]) :-
	puzzle_map_tiles(PRow, Func, PRow2),
	puzzle_map(PRows, Func, PRows2).

puzzle_map_tiles([], _, []).
puzzle_map_tiles([PTile | PTiles], Func, [PTile2 | PTiles2]) :-
	call(Func, PTile, PTile2),
	puzzle_map_tiles(PTiles, Func, PTiles2).

% SET TILE

set_tile(Puzzle, PTile, NewPuzzle) :-
	puzzle_map(Puzzle, set_tile_func(PTile), NewPuzzle).

set_tile_func(New, PTile, Result) :-
	New = tile(Id, _, _, _, _),
	(
		PTile = tile(Id, _, _, _, _)
	->
		Result = New
	;
		Result = PTile
	).

% FOLD

puzzle_fold([], _, Result, Result).
puzzle_fold([PRow | PRows], Func, Start, Result) :-
	puzzle_fold_tiles(PRow, Func, Start, Start2),
	puzzle_fold(PRows, Func, Start2, Result).

puzzle_fold_tiles([], _, Result, Result).
puzzle_fold_tiles([PTile | PTiles], Func, Start, Result) :-
	call(Func, PTile, Start, Start2),
	puzzle_fold_tiles(PTiles, Func, Start2, Result).

% GET TILE

get_tile(Puzzle, Id, PTile) :-
	puzzle_fold(Puzzle, get_tile_func(Id), nil, PTile).

get_tile_func(Id, Start, PTile, Result) :-
	(
		PTile = tile(Id, _, _, _, _)
	->
		Result = PTile
	;
		Result = Start
	).

% TRANSPOSE

puzzle_transpose([], []).
puzzle_transpose([Row | Rows], Result) :-
	(
		Rows = []
	->
		list_to_singletons(Row, Result)
	;
		puzzle_transpose(Rows, Rest),
		prepend_to_lists(Row, Rest, Result)
	).
	
prepend_to_lists([], [], []).
prepend_to_lists([X | Xs], [List | Lists], [[X | List] | Rest]) :-
	prepend_to_lists(Xs, Lists, Rest).

list_to_singletons([], []).
list_to_singletons([X | Xs], [[X] | Rest]) :-
	list_to_singletons(Xs, Rest).