:- module(fillin_propagation, [propagate/4, enqueue_all/2]).

:- use_module(fillin_filter).
:- use_module(fillin_functions).
:- use_module(fillin_queue).

% PROPAGATION

propagate(Puzzle, Wordlist, [], NewPuzzle) :-
	propagate_global_words(Puzzle, Wordlist, NewPuzzle).
	
propagate(Puzzle, Wordlist, [Id | Queue], NewPuzzle) :-
	puzzle_map_fold(Puzzle, propagate_func(Id), Queue, Puzzle1, Queue2),
	propagate(Puzzle1, Wordlist, Queue2, NewPuzzle).
	
propagate_func(Id, PTile, Queue, PTile2, Queue2) :-
	(
		PTile = tile(Id, _, _, _, _)
	->
		propagate_tile(PTile, Queue, PTile2, Queue2)
	;
		PTile2 = PTile,
		Queue2 = Queue
	).
	
propagate_tile(PTile, Queue, tile(Id, Tile, Horiz3, Vert3, Letters2), Queue5) :-
	PTile = tile(Id, Tile, Horiz, Vert, _),
	propagate_words(PTile, Horiz, Queue, Horiz2, Queue2, HorizLetters),
	propagate_words(PTile, Vert, Queue2, Vert2, Queue3, VertLetters),
	merge_letters(HorizLetters, VertLetters, Letters2),
	propagate_letters(PTile, Horiz2, Letters2, Queue3, Horiz3, Queue4),
	propagate_letters(PTile, Vert2, Letters2, Queue4, Vert3, Queue5).

% PROPAGATORS

propagate_global_words(Puzzle, Wordlist, NewPuzzle) :-
	gather_words(Puzzle, AssignedWords),
	msort(AssignedWords, OrderedAssignedWords),
	subtract_list(Wordlist, OrderedAssignedWords, RestMultiWords),
	sort(RestMultiWords, RestWords),
	propagate_listed_words(Puzzle, RestWords, Queue, FilteredPuzzle),
	(
		Queue = []
	->
		NewPuzzle = Puzzle
	;
		propagate(FilteredPuzzle, Wordlist, Queue, NewPuzzle)
	).

gather_words(Puzzle, AssignedWords) :-
	puzzle_fold(Puzzle, gather_words_func, [], AssignedWords).
	
gather_words_func(tile(Id, _, Horiz, Vert, _), Start, Result2) :-
	add_word(Id, Horiz, Start, Result1),
	add_word(Id, Vert, Result1, Result2).

add_word(_, nil, Start, Start).
add_word(Id, slot(Ids, _, Words), Start, Result) :-
	(
		Words = [Word],
		Ids = [Id | _]
	->
		append(Start, [Word], Result)
	;
		Result = Start
	).

subtract_list(All, [], All).
subtract_list([A | As], Some, Rest) :-
	Some = [S | Ss],
	compare(Cmp, A, S),
	(
		Cmp = =
	->
		subtract_list(As, Ss, Rest)
	;
		(
			Cmp = <
		->
			subtract_list(As, Some, Rest2),
			Rest = [A | Rest2]
		;
			fail
		)
	).
	
propagate_listed_words(Puzzle, AllowedWords, Queue, NewPuzzle) :-
	puzzle_map_fold(Puzzle, propagate_listed_words_func(AllowedWords), [], NewPuzzle, Queue).

propagate_listed_words_func(AllowedWords, PTile, Queue,
		tile(Id, Tile, Horiz2, Vert2, Letters), Queue3) :-
	PTile = tile(Id, Tile, Horiz, Vert, Letters),
	propagate_listed_words_tile(PTile, Horiz, AllowedWords, Queue, Horiz2, Queue2),
	propagate_listed_words_tile(PTile, Vert, AllowedWords, Queue2, Vert2, Queue3).

propagate_listed_words_tile(_, nil, _, Queue, nil, Queue).
propagate_listed_words_tile(PTile, Slot, ListedWords, Queue, slot(Ids, Tiles, Words2), Queue2) :-
	PTile = tile(_, Tile, _, _, _),
	Slot = slot(Ids, Tiles, Words),
	(
		var(Tile)
	-> 
		filter_listed_words(Words, ListedWords, Words2),
		propagate_handle_queue(PTile, Slot, Words2, Queue, Queue2)
	;
		Words2 = Words,
		Queue2 = Queue
	).

propagate_words(_, nil, Queue, nil, Queue, nil).
propagate_words(PTile, Slot, Queue, slot(Ids, Tiles, Words2), Queue2, Letters) :-
	PTile = tile(_, Tile, _, _, _),
	Slot = slot(Ids, Tiles, Words),
	filter_words(Tiles, Tile, Words, Words2, Letters),
	propagate_handle_queue(PTile, Slot, Words2, Queue, Queue2).

merge_letters(L1, L2, L) :-
	(
		L1 = nil
	->
		(
			L2 = nil
		->
			L = nil
		;
			L = L2
		)
	;
		(
			L2 = nil
		->
			L = L1
		;
			intersection(L1, L2, L)
		)
	).

propagate_letters(_, nil, _, Queue, nil, Queue).
propagate_letters(PTile, Slot, Letters, Queue, slot(Ids, Tiles, Words2), Queue2) :-
	PTile = tile(_, Tile, _, _, _),
	Slot = slot(Ids, Tiles, Words),
	filter_words_by_letters(Tiles, Tile, Words, Letters, Words2),
	propagate_handle_queue(PTile, Slot, Words2, Queue, Queue2).

propagate_handle_queue(tile(Id, Tile, _, _, _), slot(Ids, Tiles, Words), Words2, Queue, Queue2) :-
	(
		Words2 = []
	->
		fail
	;
		subtract(Ids, [Id], RestIds),
		(
			var(Tile),
			Words2 = [Word]
		->
			Tiles = Word,
			enqueue(Queue, RestIds, Queue2)
		;
			(
				Words == Words2
			->
				Queue2 = Queue
			;
				enqueue(Queue, RestIds, Queue2)
			)
		)
	).