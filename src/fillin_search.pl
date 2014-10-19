%!	Contains the search predicate which does all the hard work.
%	This is also the only place where decisions are made.
:- module(fillin_search, [search/3]).

:- use_module(fillin_propagation).
:- use_module(fillin_functions).


%!	search(+Puzzle, +Wordlist, -Solution)
%
%	@arg Puzzle a puzzle to fill
%	@arg Wodlist list of words to fill into the Puzzle
%	@arg Solution a fully filled Puzzle 
%
%	Does the search decisions in a Puzzle
%	which cannot be filled using just propagation.
%	The search is recursive and calls itself
%	until it finds a solution or proves there is none.
%	For each step the best candidate for value assignment is used.
%	After that a full propagation of the changed PTile is run.
%	The only decision points are done in value assignment.
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

%!	find_best(+Puzzle, -PTile)
%
%	@arg Puzzle a puzzle to look through
%	@arg PTile the best candidate or nil
%
%	The best candidate for search decision is such,
%	which has the lowest number of possibilites for its value.
%	It is determined by size of Letters.
%	If no PTile is empty, nil is returned. 

%	Notice that Letters has always correct because a propagation was run
%	prior to search.
%	This is true even for the first step; in that case
%	propagation of whole Puzzle set the Letters.
%
%	Notice also that this predicate will never return a PTile
%	with a nonvar Tile despite find_best_func could return it.
find_best(Puzzle, PTile) :-
	puzzle_fold(Puzzle, find_best_func, nil, PTile).

%!	find_best_func(+PTile1, +PTile2, -PTile)
%
%	@arg PTile1 first
%	@arg PTile2 second
%	@arg PTile better of those two
%
%	This is meant to be passed to puzzle_fold.
%	The better PTile is the one which is not nil and has unbound Tile.
%	If both are such, they are compared by their Letters lengths.
%	The one with less possibilities is better.
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

%!	compare_tiles(+PTile1, +PTile2, -PTile)
%
%	@arg PTile1 first
%	@arg PTile2 second
%	@arg PTile better of those two
%
%	Similar to find_best_func but this time it suppose
%	that both contain unbound Tile.
%	Both PTiles are rated by Letters lengths
%	and the one with less value is returned.
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

%!	rate_tile(+PTile, -Rating)
%
%	@arg PTile PTile to be rated
%	@arg Rating rating
%
%	Rates the PTile by length of its Letters list.
rate_tile(tile(_, _, _, _, Letters), Rating) :-
	length(Letters, Rating).

%!	set_value(+PTile)
%
%	@arg PTile PTile which value will be set
%
%	NONDETERMINISTIC
%	This should be the only place where nondeterminism is used.
set_value(tile(_, Tile, _, _, Letters)) :-
	member(Letter, Letters),
	Tile = Letter.