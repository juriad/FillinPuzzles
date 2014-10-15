:- module(fillin_cp,[solve_puzzle/3]).

:- use_module(fillin_init).
:- use_module(fillin_propagation).
:- use_module(fillin_search).
:- use_module(fillin_functions).
:- use_module(fillin_queue).

solve_puzzle(Template, UnsortedWordlist, Solution) :-
	msort(UnsortedWordlist, Wordlist),
	sort(Wordlist, UniqueWordlist),
	create_puzzle(Template, UniqueWordlist, EmptyPuzzle),
	!,
	validate(EmptyPuzzle),
	enqueue_all(EmptyPuzzle, Queue),
	propagate(EmptyPuzzle, Wordlist, Queue, FilteredPuzzle),
	!,
	search(FilteredPuzzle, Wordlist, Solution).

validate(Puzzle) :-
	puzzle_map(Puzzle, validate_func, _).

validate_func(PTile, PTile) :-
	PTile = tile(_, Tile, Horiz, Vert, _),
	(
		Horiz = nil,
		Vert = nil,
		Tile \== '#'
	->
		fail
	;
		true
	).