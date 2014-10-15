:- module(fillin_random,[random_template/6]).

:- use_module(library(random)).

:- use_module(fillin_functions).
:- use_module(fillin_init).

random_template(Width, Height, Alphabet, Average, Template, Wordlist) :-
	empty_board(Width, Height, Board),
	fill_board(Board, Alphabet, Average, Filled),
	create_puzzle(Filled, [], Puzzle),
	gather_words(Puzzle, Wordlist),
	empty_template(Filled, Template).

empty_board(Width, Height, Board) :-
	(
		Height = 0
	->
		Board = []
	;
		length(Row, Width),
		Height2 is Height - 1,
		empty_board(Width, Height2, Rest),
		Board =[Row | Rest]
	).

fill_board(Board, Alphabet, Average, Filled) :-
	puzzle_map(Board, fill_board_func(Alphabet, Average), Filled).

fill_board_func(Alphabet, Average, _, Tile) :-
	random_between(0, Average, Chance),
	(
		Chance = 0
	->
		Tile = '#'
	;
		random_member(Tile, Alphabet)
	).

gather_words(Puzzle, Wordlist) :-
	puzzle_fold(Puzzle, gather_words_func, [], Wordlist).

gather_words_func(tile(_, _, Horiz, Vert, _), List, Result) :-
	gather_word(List, Horiz, List2),
	gather_word(List2, Vert, Result).

gather_word(List, Word, List2) :-
	(
		Word = nil
	->
		List2 = List
	;
		Word = word(_, Tiles, _),
		union(List, [Tiles], List2)
	).

empty_template(Filled, Template) :-
	puzzle_map(Filled, empty_template_func, Template).

empty_template_func(Tile1, Tile2) :-
	(
		Tile1 = '#'
	->
		Tile2 = Tile1
	;
		Tile2 = '_'
	).