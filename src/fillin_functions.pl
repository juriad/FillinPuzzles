%!	This contains a few higher-order functions and a custom transposition.
:- module(fillin_functions, [
		puzzle_map_fold/5, 
		puzzle_map/3, 
		puzzle_fold/4, 
		puzzle_transpose/2]).

%	These higher-order predicates are exported.
%	They need to be marked as meta_predicates
%	to accept predicates from other modules. 
:- meta_predicate puzzle_map(?, 2, ?).
:- meta_predicate puzzle_fold(?, 3, ?, ?).
:- meta_predicate puzzle_map_fold(?, 4, ?, ?, ?).

%!	puzzle_map(+Puzzle, +Func, -MappedPuzzle)
%
%	@arg Puzzle Puzzle whose PTiles will be mapped
%	@arg Func predicate to be applied to all PTiles
%	@arg MappedPuzzle Puzzle with PTiles which were processed by Func
%
%	Standard higher-order map for whole Puzzle.
%	Func has arguments: (+PTile, -MappedPTile).
%
%	This predicate iterates over rows of the Puzzle
%	and passes them to puzzle_map_tiles which does the actual mapping.
puzzle_map([], _, []).
puzzle_map([PRow | PRows], Func, [PRow2 | PRows2]) :-
	puzzle_map_tiles(PRow, Func, PRow2),
	puzzle_map(PRows, Func, PRows2).

%!	puzzle_map_tiles(+PRow, +Func, -MappedPRow)
%
%	@arg PRow PRow whose PTiles will be mapped
%	@arg Func predicate to be applied to all PTiles
%	@arg MappedPRow PRow with PTiles which were processed by Func
%
%	Standard higher-order map for a single PRow.
%	Func has arguments: (+PTile, -MappedPTile).
%
%	This predicate iterates over PTiles of the PRow
%	and calls puzzle_map_tiles which maps them.
puzzle_map_tiles([], _, []).
puzzle_map_tiles([PTile | PTiles], Func, [PTile2 | PTiles2]) :-
	call(Func, PTile, PTile2),
	puzzle_map_tiles(PTiles, Func, PTiles2).

%!	puzzle_fold(+Puzzle, +Func, +Start, -Result)
%
%	@arg Puzzle Puzzle whose PTiles will be folded
%	@arg Func predicate to be applied to all PTiles and intermediate result
%	@arg Start starting value which is used for the first Func application
%	@arg Result Result of the last application of Func
%
%	Standard higher-order fold for whole Puzzle.
%	Func has arguments: (+PTile, +Intermediate, -Combined).
%
%	This predicate iterates over rows of the Puzzle
%	and passes them to puzzle_fold_tiles which does the actual folding.
%	The function puzzle_fold_tiles returns intermediate result
%	which is used as Start for the next row.
puzzle_fold([], _, Result, Result).
puzzle_fold([PRow | PRows], Func, Start, Result) :-
	puzzle_fold_tiles(PRow, Func, Start, Start2),
	puzzle_fold(PRows, Func, Start2, Result).

%!	puzzle_fold_tiles(+PRow, +Func, +Start, -Result)
%
%	@arg PRow PRow whose PTiles will be mapped
%	@arg Func predicate to be applied to all PTiles
%	@arg Start starting value which is used for the first Func application
%	@arg Result Result of the last application of Func
%
%	Standard higher-order map for a single PRow.
%	Func has arguments: (+PTile, +Intermediate, -Combined).
%
%	This predicate iterates over PTiles of the PRow
%	and calls puzzle_fold_tiles which combines them
%	with the intermediate result.
puzzle_fold_tiles([], _, Result, Result).
puzzle_fold_tiles([PTile | PTiles], Func, Start, Result) :-
	call(Func, PTile, Start, Start2),
	puzzle_fold_tiles(PTiles, Func, Start2, Result).
	
%!	puzzle_map_fold(+Puzzle, +Func, +Start, -MappedPuzzle, -Result)
%
%	@arg Puzzle Puzzle whose PTiles will be mapped and folded
%	@arg Func predicate to be applied to all PTiles
%	@arg Start starting value which is used for the first Func application
%	@arg MappedPuzzle Puzzle with PTiles which were processed by Func
%	@arg Result Result of the last application of Func
%
%	Combination of the standard map and fold.
%	Func has arguments: (+PTile, +Intermediate, -MappedPTile, -Combined).
%
%	This predicate iterates over PTiles of the PRow
%	and calls puzzle_fold_tiles which folds them with intermediate results.
puzzle_map_fold([], _, Result, [], Result).
puzzle_map_fold([PRow | PRows], Func, Start, [PRow2 | PRows2], Result) :-
	puzzle_map_fold_tiles(PRow, Func, Start, PRow2, Start2),
	puzzle_map_fold(PRows, Func, Start2, PRows2, Result).

%!	puzzle_map_fold_tiles(+PRow, +Func, +Start, -MappedPRow, -Result)
%
%	@arg PRow PRow whose PTiles will be mapped
%	@arg Func predicate to be applied to all PTiles
%	@arg Start starting value which is used for the first Func application
%	@arg Result Result of the last application of Func
%
%	Combination of the standard map and fold.
%	Func has arguments: (+PTile, +Intermediate, -MappedPTile, -Combined).
%
%	This predicate iterates over PTiles of the PRow
%	and calls puzzle_map_fold_tiles
%	which maps them, and folds them with intermediate results.
puzzle_map_fold_tiles([], _, Result, [], Result).
puzzle_map_fold_tiles([PTile | PTiles], Func, Start, [PTile2 | PTiles2], Result) :-
	call(Func, PTile, Start, PTile2, Start2),
	puzzle_map_fold_tiles(PTiles, Func, Start2, PTiles2, Result).

%!	puzzle_transpose(+Puzzle, -PuzzleT)
%
%	@arg Puzzle Puzzle to transpose
%	@arg PuzzleT transposed Puzzle
%
%	Transposes the Puzzle.
%	
%	It iterates over rows and prepends each element to a list of columns.
%	The last row is converted to list of singleton lists.
puzzle_transpose([], []).
puzzle_transpose([Row | Rows], PuzzleT) :-
	(
		Rows = []
	->
		list_to_singletons(Row, PuzzleT)
	;
		puzzle_transpose(Rows, Rest),
		prepend_to_lists(Row, Rest, PuzzleT)
	).

%!	prepend_to_lists(+Elements, +Lists, -ListsWithElements)
%
%	@arg Elements list of elements to prepend by one to list of lists
%	@arg Lists list of lists of the same size as Elements
%	@arg ListsWithElements list of lists with prepended Elements
%
%	Prepends each element to one list.
prepend_to_lists([], [], []).
prepend_to_lists([X | Xs], [List | Lists], [[X | List] | Rest]) :-
	prepend_to_lists(Xs, Lists, Rest).

%!	list_to_singletons(+List, -ListsOfSingletons)
%
%	@arg List list of elements to be converted
%	@arg ListsOfSingletons list of lists of size one
%
%	Each list of ListsOfSingletons will contain exactly one
%	element from List.
list_to_singletons([], []).
list_to_singletons([X | Xs], [[X] | Rest]) :-
	list_to_singletons(Xs, Rest).