%!	Creates an empty puzzle from a template.
%	This module exports only one predicate.
:- module(fillin_init, [create_puzzle/3]).

:- use_module(fillin_filter).
:- use_module(fillin_functions).

%!	create_puzzle(+Template, +Wordlist, -Puzzle)
%
%	@arg Template template fo the puzzle to be transformed into a puzzle
%	@arg Wordlist list of all possible words in the puzzle; no duplicates
%	@arg Puzzle an empty initialized puzzle
%
%	This prodicate constructs a complex representation of a puzzle.
%	The returned structure is described in fillin_cp.
%	
%	The transformation first assignes Id and Tile of each PTile.
%	Then it creates slots of Ids and Tiles with no Words.
%	In the end it assignes possible Words to the slots.
%	Letters are left uninitialized (unbound) as they are computed by
%	propagators on-the-fly.
create_puzzle(Template, Wordlist, Puzzle) :-
	create_vars(Template, 0, PuzzleWithVars),
	create_horiz(PuzzleWithVars, PuzzleVarsWithHoriz),
	create_vert(PuzzleVarsWithHoriz, PuzzleVarsWithVert),
	create_words(PuzzleVarsWithVert, Wordlist, Puzzle).

%!	create_vars(+Template, +Id, -Puzzle)
%
%	@arg Template template representation
%	@arg Id accumulator containing the first number to assign as Id
%	@arg Puzzle puzzle with initialized Id and Tile
%
%	Iterated over rows of the template and calls create_vars_tiles,
%	which does the actual job.
create_vars([], _, []).
create_vars([TRow | TRows], Id, [PRow | PRows]) :-
	create_vars_tiles(TRow, Id, PRow, NextId),
	create_vars(TRows, NextId, PRows).


%!	create_vars_tiles(+TRow, +Id, -PRow)
%
%	@arg TRow a single row of the template representation
%	@arg Id accumulator containing the first number to assign as Id
%	@arg PRow a single row of puzzle with initialized Id and Tile
%
%	Iterated over TTiles of the template; for each of them,
%	it sets the Id and binds the Tile if the TTile is not empty ('_')
%	Otherwise, it leaves the Tile unbound.
create_vars_tiles([], Id, [], Id).
create_vars_tiles(
		[TTile | TTiles],
		Id, 
		[tile(Id, Tile, _, _, _) | PTiles], 
		NextId) :-
	(
		TTile \= '_'
	->
		Tile = TTile
	;
		true
	),
	Id2 is Id + 1,
	create_vars_tiles(TTiles, Id2, PTiles, NextId).

%!	create_horiz(+Puzzle, -PuzzleWithHoriz)
%
%	@arg Puzzle a puzzle with uninitialized horizontal slots
%	@arg PuzzleWithHoriz a puzzle with initialized horizontal slots
%
%	This predicate just calls create_slots with a horizontal slot constructor.
create_horiz(Puzzle, PuzzleWithHoriz) :-
	create_slots(Puzzle, horiz_slot_ctor, PuzzleWithHoriz).

%!	horiz_slot_ctor(+PTile, +HorizPTiles, -PTileWithHorizSlot)
%
%	@arg PTile ptile which does not contain horizontal slot
%	@arg HorizPTiles list of PTiles which make up the horizontal slot
%	@arg PTileWithHorizSlot PTile with filled in horizontal slot
%
%	If the number of PTiles in the slot is greater than 1,
%	then it sets the slot to list of Ids and Tiles extracted from HorizPTiles,
%	otherwise it sets the slot to nil.
horiz_slot_ctor(
		tile(Id, Tile, _, Vert, Letters),
		HorizPTiles,
		tile(Id, Tile, Slot, Vert, Letters)) :-
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

%!	create_vert(+Puzzle, -PuzzleWithHoriz)
%
%	@arg Puzzle a puzzle with uninitialized vertical slots
%	@arg PuzzleWithVert a puzzle with initialized vertical slots
%
%	This predicates first transposes the puzzle
%	and then it calls create_slots with a vertical slot constructor.
%	After that, it transposes the puzzle back.
create_vert(Puzzle, PuzzleWithVert) :-
	puzzle_transpose(Puzzle, PuzzleT),
	create_slots(PuzzleT, vert_slot_ctor, PuzzleWithVertT),
	puzzle_transpose(PuzzleWithVertT, PuzzleWithVert).

%!	vert_slot_ctor(+PTile, +VertPTiles, -PTileWithVertSlot)
%
%	@arg PTile ptile which does not contain vertical slot
%	@arg VertPTiles list of PTiles which make up the vertical slot
%	@arg PTileWithVertSlot PTile with filled in vertical slot
%
%	If the number of PTiles in the slot is greater than 1,
%	then it sets the slot to list of Ids and Tiles extracted from VertPTiles,
%	otherwise it sets the slot to nil.
vert_slot_ctor(
		tile(Id, Tile, Horiz, _, Letters),
		VertPTiles,
		tile(Id, Tile, Horiz, Slot, Letters)) :-
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

%!	create_slots(+Puzzle, +SlotCtor, -PuzzleWithSlot)
%
%	@arg Puzzle a puzzle whose slot will be initialized
%	@arg SlotCtor a predicate which sets the slot in PTile
%	@arg PuzzleWithSlot a puzzle with initialized slot
%
%	Iterates over rows of the puzzle; for each of them, 
%	it calls create_slot with empty list of PTiles making up the current slot.
%
%	The predicate is parametrized by the slot constructor to enable its reuse
%	for both, horizontal and vertical slots.
create_slots([], _, []).
create_slots([PRow | PRows], SlotCtor, [PRow2 | PRows2]) :-
	create_slot(PRow, SlotCtor, [], PRow2),
	create_slots(PRows, SlotCtor, PRows2).

%!	create_slot(+PRow, +SlotCtor, Before, -PRowWithSlot)
%
%	@arg PRow a row of a puzzle whose slot will be initialized
%	@arg SlotCtor a predicate which sets the slot in PTile
%	@arg Before accumulator of PTiles making up the current slot
%	@arg PRowWithSlot a row of a puzzle with initialized slot
%
%	Iterates over PTiles of the puzzle.
%	If the current PTile is not a border,
%	then it finds the slot as a concatenation of
%	Before ++ [PTile] ++ RestOfSlot.
%	The rest of the slot is found by predicate create_slot_find_rest.
%	The whole slot is then created and assigned to PTile by slot constructor.
%	On the other hand, when the tile is border, it creates a nil slot
%	and it procedes with the next PTile passing an empty list as Before.
create_slot([], _, _, []).
create_slot([PTile | PTiles], SlotCtor, Before, [NewTile | PTiles2]) :-
	PTile = tile(_, Tile, _, _, _),
	(
		Tile == '#'
	->
		call(SlotCtor, PTile, [], NewTile),
		WithMe = []
	;
		append(Before, [PTile], WithMe),
		create_slot_find_rest(PTiles, After),
		append(WithMe, After, Word),
		call(SlotCtor, PTile, Word, NewTile)
	),
	create_slot(PTiles, SlotCtor, WithMe, PTiles2).

%!	create_slot_find_rest(+RestOfRow, -RestOfSlot)
%
%	@arg RestOfRow the rest of currently processed row
%	@arg RestOfSlot Identified rest of the current slot
%
%	The rest of the slot is a prefix of the rest of the row.
%	It scans until it finds a border (a Tile which is '#').
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

%!	create_words(Puzzle, Wordlist, PuzzleWithWords)
%
%	@arg Puzzle puzzle with slots but without lists of possible words
%	@arg Wordlist list of all words without duplicates
%	@arg PuzzleWithWords puzzle with lists of possible words
%
%	This predicates just calls puzzle_map and passes it function
%	create_words_func applied to list of words.
%	The function then filters and assignes the words for each PTile.
create_words(Puzzle, Wordlist, PuzzleWithWords) :-
	 puzzle_map(Puzzle, create_words_func(Wordlist), PuzzleWithWords).

%!	create_words_func(Wordlist, PTile, PTileWithWords)
%
%	@arg Wordlist list of all words without duplicates
%	@arg PTile a PTile without assigned list of possible words
%	@arg PTileWithWords a PTile with lists of possible words
%
%	It passes on the task to create_words_slot
%	for vertical and horizontal slots separately.
create_words_func(
		Wordlist,
		tile(Id, Tile, Horiz, Vert, Letters),
		tile(Id, Tile, Horiz2, Vert2, Letters)) :-
	create_words_slot(Tile, Wordlist, Horiz, Horiz2),
	create_words_slot(Tile, Wordlist, Vert, Vert2).

%!	create_words_slot(Tile, Wordlist, Slot, SlotWithWords)
%
%	@arg Tile the Tile of current PTile
%	@arg Wordlist list of all words without duplicates
%	@arg Slot a Slot without assigned list of possible words
%	@arg SlotWithWords a Slot with lists of possible words
%
%	If the slot is nil, it does nothing.
%	Otherwise it filters out all words which are not unifiable with the Slot.
%	The words which are left are assigned to the Slot.
%
%	The first parameter Tile is necessary only because the predicate
%	filter_words can is more generally in propagation.
create_words_slot(Tile, Wordlist, Slot, SlotWithWords) :-
	(
		Slot = nil
	->
		SlotWithWords = nil
	;
		Slot = slot(Ids, Tiles, _),
		filter_words(Tile, Wordlist, Tiles, RowFiltered, _),
		SlotWithWords = slot(Ids, Tiles, RowFiltered)
	).