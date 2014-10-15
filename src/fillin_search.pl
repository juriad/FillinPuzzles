:- module(fillin_search, [search/3]).

:- use_module(fillin_propagation).
:- use_module(fillin_functions).

search(Puzzle, Wordlist, Solution) :-
	find_best(Puzzle, PTile),
	(
		PTile = tile(Id, _, _, _, _)
	->
		set_value(PTile),
		propagate(Puzzle, Wordlist, [Id], FilledPuzzle),
		search(FilledPuzzle, Wordlist, Solution)
	;
		Solution = Puzzle
	).

find_best(Puzzle, PTile) :-
	puzzle_fold(Puzzle, find_best_func, nil, PTile).

find_best_func(PTile1, PTile2, PTile) :-
	(
		(
			PTile1 = nil
			;
			PTile1 = tile(_, Tile1, _, _, _),
			nonvar(Tile1)
		)
	->
		PTile = PTile2
	;
		(
			(
				PTile2 = nil
				;
				PTile2 = tile(_, Tile2, _, _, _),
				nonvar(Tile2)
			)
		->
			PTile = PTile1
		;
			compare_tiles(PTile1, PTile2, PTile)
		)
	).

compare_tiles(PTile1, PTile2, PTile) :-
	rate_tile(PTile1, Rating1),
	rate_tile(PTile2, Rating2),
	(
		Rating1 < Rating2
	->
		PTile = PTile1
	;
		PTile = PTile2
	).

rate_tile(tile(_, _, _, _, Letters), Rating) :-
	length(Letters, Rating).

set_value(tile(_, Tile, _, _, Letters)) :-
	member(Letter, Letters),
	Tile = Letter.