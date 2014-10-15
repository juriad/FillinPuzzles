% Variable names:
% T - template
% P - puzzle
% PTile = one cell in puzzle
% Tile = char variable in PTile

:- module(fillin_init, [create_puzzle/3]).

:- use_module(fillin_filter).
:- use_module(fillin_functions).

create_puzzle(Template, Wordlist, Puzzle) :-
	create_vars(Template, 0, PuzzleWithVars),
	create_horiz(PuzzleWithVars, PuzzleVarsWithHoriz),
	create_vert(PuzzleVarsWithHoriz, PuzzleVarsWithVert),
	create_words(PuzzleVarsWithVert, Wordlist, Puzzle).

% CREATE VARS

create_vars([], _, []).
create_vars([TRow | TRows], Id, [PRow | PRows]) :-
	create_vars_tiles(TRow, Id, PRow, NextId),
	create_vars(TRows, NextId, PRows).

create_vars_tiles([], Id, [], Id).
% Sets Tile to letter or '#', or leaves it unbounded 
create_vars_tiles([TTile | TTiles], Id, [tile(Id, Tile, _, _, _) | PTiles], NextId) :-
	(
		TTile \= '_'
	->
		Tile = TTile
	;
		true
	),
	Id2 is Id + 1,
	create_vars_tiles(TTiles, Id2, PTiles, NextId).

% CREATE HORIZ

create_horiz(Puzzle, PuzzleWithHoriz) :-
	create_slots(Puzzle, horiz_tile_ftor, PuzzleWithHoriz).

horiz_tile_ftor(tile(Id, Tile, _, Vert, Letters), HorizPTiles, tile(Id, Tile, Slot, Vert, Letters)) :-
	(
		length(HorizPTiles, Len),
		Len > 1
	->
		filter_ids(HorizPTiles, HorizIds),
		filter_tiles(HorizPTiles, HorizTiles),
		Slot = slot(HorizIds, HorizTiles, _)
	;
		Slot = nil
	).

% CREATE VERT

create_vert(Puzzle, PuzzleWithCols) :-
	puzzle_transpose(Puzzle, PuzzleT),
	create_slots(PuzzleT, vert_tile_ftor, PuzzleWithColsT),
	puzzle_transpose(PuzzleWithColsT, PuzzleWithCols).

vert_tile_ftor(tile(Id, Tile, Horiz, _, Letters), VertPTiles, tile(Id, Tile, Horiz, Slot, Letters)) :-
	(
		length(VertPTiles, Len),
		Len > 1
	->
		filter_ids(VertPTiles, VertIds),
		filter_tiles(VertPTiles, VertTiles),
		Slot = slot(VertIds, VertTiles, _)
	;
		Slot = nil
	).

% COMMON FOR CREATE HORIZ AND CREATE VERT

create_slots([], _, []).
create_slots([PRow | PRows], TileFtor, [PRow2 | PRows2]) :-
	create_slot(PRow, TileFtor, [], PRow2),
	create_slots(PRows, TileFtor, PRows2).

create_slot([], _, _, []).
create_slot([PTile | PTiles], TileFtor, Before, [NewTile | PTiles2]) :-
	PTile = tile(_, Tile, _, _, _),
	(
		Tile == '#'
	->
		call(TileFtor, PTile, [], NewTile),
		WithMe = []
	;
		append(Before, [PTile], WithMe),
		create_slot_find_rest(PTiles, After),
		append(WithMe, After, Word),
		call(TileFtor, PTile, Word, NewTile)
	),
	create_slot(PTiles, TileFtor, WithMe, PTiles2).

create_slot_find_rest([], []).
create_slot_find_rest([PTile | PTiles], Result) :-
	PTile = tile(_, Tile, _, _, _),
	(
		Tile == '#'
	->
		Result = []
	;
		create_slot_find_rest(PTiles, Rest),
		append([PTile], Rest, Result)
	).

% CREATE WORDS

create_words(Puzzle, Wordlist, PuzzleWithWords) :-
	 puzzle_map(Puzzle, create_words_func(Wordlist), PuzzleWithWords).

create_words_func(Wordlist, tile(Id, Tile, Horiz, Vert, Letters), tile(Id, Tile, Horiz2, Vert2, Letters)) :-
	create_words_slot(Tile, Wordlist, Horiz, Horiz2),
	create_words_slot(Tile, Wordlist, Vert, Vert2).

create_words_slot(Tile, Wordlist, Slot, Slot2) :-
	(
		Slot = nil
	->
		Slot2 = nil
	;
		Slot = slot(Ids, Tiles, _),
		filter_words(Tile, Wordlist, Tiles, RowFiltered, _),
		Slot2 = slot(Ids, Tiles, RowFiltered)
	).