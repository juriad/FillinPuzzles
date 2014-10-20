%!	Propagation stuff which basically filters out all options
%	which are not possible from various reasons.
:- module(fillin_propagation, [propagate/4]).

:- use_module(fillin_filter).
:- use_module(fillin_functions).
:- use_module(fillin_queue).

%!	propagate(+Puzzle, +Wordlist, Queue, FilteredPuzzle)
%
%	@arg Puzzle Puzzle to propagate
%	@arg Wordlist list of all possible words including possible duplicities
%	@arg Queue a list of Ids of PTiles to process
%	@arg PropagatedPuzzle a new puzzle after all propagations performed
%
%	The Queue holds list of Ids of PTiles which should be processed
%	as there is a possibility that their options may have changed since
%	last time.
%	During the propagation new Ids may be queued (appended if not present).
%	The propagation is run until the queue is empty.
%
%	A propagator should be a predicate which takes a PTile and a Queue,
%	and returns possibly changed PTile and either a larger or the same Queue
%	depending on if any changed was performed.
%	In general, propagators should also be order independent.
%
%	To make the implementation easier, both preconditions are violated.
%	Propagators are order dependent
%	and they work only with a part of PTile,
%	they also have other arguments.
%
%	The first propagator is word propagator which is called twice,
%	each time for slot in a different direction.
%	This propagator filters out all words which simply cannot unify
%	with the slot.
%	It also returns set of letters which could be assigned to the Tile.
%
%	Before the second propagation is run a set of letters
%	which are possible in both direction is made.
%	This set is an intersection of particular letter sets from the first step.
%	If either one of the slots does not exist (nil),
%	the intersection is identity of the set of leters of the existing slot.
%
%	The second propagator is letter propagator, it is also called twice.
%	During this propagation the list of words is filtered once more
%	and only such words, which would assign one of the allowed letters
%	from the intersection, are left.
%
%	The set of all possible letters is not thrown away,
%	it is assigned to the updated PTile together with both filtered slots.
%	If during the propagation only one word is possible to assign,
%	it will be assigned (unified to the slot Tiles).
%
%	During all propagations new ids may be enqued.
%	The enqueued ids are Ids of other PTiles of slot which has been changed.
%	The processed PTile is never put to the Queue again.
%
%	The last propagator is responsible for word usage.
%	As it operates globaly, it is quite costly and therefore run
%	when the queue is empty and the propagation would have ended.
%
%	The global propagator gathers all assigned words from all slots
%	and filters out from all PTiles all words which have already been
%	assigned the number of times they appear in the initial Wordlist.
%
%	Any of the propagators may fail, the reasons are:
%	* No word can be assigned to a slot.
%	* A word has been used too many times.
%
%	This implementation just puzzle_map_folds the whole Puzzle
%	and does the propagation only when
%	the head of the Queue matches the PTile's Id
%	The map part is necessary for generation of a new Puzzle.
%	The fold part is necessary for the Queue maintanance. 
propagate(Puzzle, Wordlist, [], PropagatedPuzzle) :-
	propagate_global_words(Puzzle, Wordlist, PropagatedPuzzle).
	
propagate(Puzzle, Wordlist, [Id | Queue], PropagatedPuzzle) :-
	puzzle_map_fold(Puzzle, propagate_func(Id), Queue, Puzzle1, Queue2),
	propagate(Puzzle1, Wordlist, Queue2, PropagatedPuzzle).
	
%	propagate_global_words(+Puzzle, +Wordlist, -PropagatedPuzzle)
%
%	@arg Puzzle Puzzle to be propagaed using global words propagator
%	@arg Wordlist list of all possible Words including duplicities
%	@arg PropagaedPuzzle Puzzle after full propagation
%
%	This propagator is tricky, it is run when the Queue is empty
%	but during its invocation the queue may change,
%	therefore if the Queue is non-empty, a full propagation is run again.
%	This means there is a hidden recursion.
%
%	The propagator first gathers list of all assigned words from the Puzzle,
%	then it compares it with initial Wordlist.
%	If a word is used less than it occurs in Wordlist, no change is made.
%	If a word is used more than it occurs in Wordlist, the propagator fails.
%	If a word is used exactly as it occurs in Wordlist,
%	the word is removed from all PTiles' lists of possibilities
%	where the PTile's Slot was not assigned to that word.
%	The list of cahnged PTiles in then considered to be a new queue.
propagate_global_words(Puzzle, Wordlist, PropagatedPuzzle) :-
	gather_words(Puzzle, AssignedWords),
	msort(AssignedWords, OrderedAssignedWords),
	subtract_list(Wordlist, OrderedAssignedWords, RestMultiWords),
	sort(RestMultiWords, RestWords),
	propagate_allowed_words(Puzzle, RestWords, FilteredPuzzle, Queue),
	(
		Queue = []
	->
		PropagatedPuzzle = Puzzle
	;
		propagate(FilteredPuzzle, Wordlist, Queue, PropagatedPuzzle)
	).

%!	gather_words(+Puzzle, -AssignedWords)
%
%	@arg Puzzle Puzzle to be searched for assigned words
%	@arg AssignedWords list of words which are assigned to a Slot in Puzzle
%
%	This just calls puzzle_fold with gather_words_func.
%	The result list caontains each words exactly once per Slot
%	and multiple times if a word is assigned to several Slots.
%	The list is in no paricular order.
gather_words(Puzzle, AssignedWords) :-
	puzzle_fold(Puzzle, gather_words_func, [], AssignedWords).

%!	gather_words_func(+PTile, -AssignedWords)
%
%	@arg PTile PTile to be searched for assigned words
%	@arg Accumulator a list of words found so far
%	@arg AssignedWords a list of words found so far including PTile's words
%
%	This predicate tests if either horizontal or vertical slot has been
%	assigned a word.
%	If so, the word is added to the Accumulator and returned as AssignedWords.
gather_words_func(tile(Id, _, Horiz, Vert, _), Accumulator, AssignedWords) :-
	add_word(Id, Horiz, Accumulator, Result1),
	add_word(Id, Vert, Result1, AssignedWords).

%!	add_word(+Id, +Slot, +Accumulator, -Result)
%
%	@arg Id Id of the PTile which is currently being procesed
%	@arg Slot Slot of the PTile which is currently being procesed
%	@arg Accumulator list of words found so far
%	@arg Result list of words found so far including Slot's assigned word
%
%	If the slot is nil, the Accumulator is the Result.
%	Otherwise the slot is tested if the currently processed PTile
%	is its first PTile; if not, it is skipped.
%	If it is, the Slot is tested if it contains exactly one word,
%	if so, the word is considered assigned and is appened to the Accumulator.
add_word(_, nil, Accumulator, Accumulator).
add_word(Id, slot(Ids, _, Words), Accumulator, Result) :-
	(
		Words = [Word],
		Ids = [Id | _]
	->
		append(Accumulator, [Word], Result)
	;
		Result = Accumulator
	).

%!	subtract_list(+All, +Some, -Rest)
%
%	@arg All list of all elements
%	@arg Some list of some of the elements of All
%	@arg Rest elements of All without Some
%	TODO unify only one clause
%
%	All the lists must be sorted.
%	All the lists may contain duplicities.
%	Some must contain at most that number of copies of an element
%	which is equal to number of occurences in All.
%	Otherwise the predicated is forced to fail.
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

%!	propagate_allowed_words(+Puzzle, +AllowedWords, -PropagatedPuzzle, -Queue)
%
%	@arg Puzzle the Puzzle to apply the propagation to
%	@arg AllowedWords list of all allowed words to assign
%	@arg PropagatedPuzzle the Puzzle after propagation
%	@arg Queue list of Ids of changed PTiles
%
%	Calls puzzle_map_fold to the the propagation on all PTiles
%	and also to gather list of Ids of changed PTiles. 
propagate_allowed_words(Puzzle, AllowedWords, PropagatedPuzzle, Queue) :-
	puzzle_map_fold(
			Puzzle,
			propagate_allowed_words_func(AllowedWords),
			[], 
			PropagatedPuzzle,
			Queue).

%!	propagate_allowed_words_func(
%!			+AllowedWords,
%!			+PTile,
%!			+Queue,
%!			-PropagatedPTile,
%!			-ModifiedQueue)
%
%	@arg AllowedWords list of all allowed words to assign
%	@arg PTile the PTile to apply the propagation to
%	@arg Queue current queue
%	@arg PropagatedPTile the PTile after propagation
%	@arg ModifiedQueue the Queue with possibly enqued new Ids
%
%	Both slots' list of possible words are filtered and if changed,
%	the PTile is enqueued for further propagation. 
propagate_allowed_words_func(
		AllowedWords,
		PTile, 
		Queue,
		tile(Id, Tile, PropagatedHoriz, PropagatedVert, Letters),
		ModifiedQueue) :-
	PTile = tile(Id, Tile, Horiz, Vert, Letters),
	propagate_allowed_words_slot(
			PTile,
			Horiz,
			AllowedWords,
			Queue,
			PropagatedHoriz,
			Queue2),
	propagate_allowed_words_slot(
			PTile,
			Vert, 
			AllowedWords, 
			Queue2, 
			PropagatedVert, 
			ModifiedQueue).

%!	propagate_allowed_words_func(
%!			+AllowedWords,
%!			+PTile,
%!			+Queue,
%!			-PropagatedPTile,
%!			-ModifiedQueue)
%	TODO unify only one clause
%
%	@arg PTile the PTile to apply the propagation to
%	@arg Slot the PTile's Slot ot apply the propagation to
%	@arg AllowedWords list of all allowed words to assign
%	@arg Queue current queue
%	@arg PropagatedSlot the Slot after propagation
%	@arg ModifiedQueue the Queue with possibly enqued new Ids
%
%	If the PTile's Tile is already bound, the propagation is skipped.
%	Otherwise, the list of possible words is filtered
%	and only those which are in both lists are kept.
%	If the Slot changed, Ids of its other PTiles are enqueued. 
propagate_allowed_words_slot(_, nil, _, Queue, nil, Queue).
propagate_allowed_words_slot(
		PTile, 
		Slot, 
		AllowedWords, 
		Queue, 
		slot(Ids, Tiles, PropagatedWords), 
		ModifiedQueue) :-
	PTile = tile(_, Tile, _, _, _),
	Slot = slot(Ids, Tiles, Words),
	(
		var(Tile)
	-> 
		filter_listed_words(Words, AllowedWords, PropagatedWords),
		propagate_handle_queue(
				PTile, 
				Slot, 
				PropagatedWords, 
				Queue, 
				ModifiedQueue)
	;
		PropagatedWords = Words,
		ModifiedQueue = Queue
	).

%!	propagate_func(+Id, +PTile, +Queue, -PropagatedPTile, -ModifiedQueue)
%
%	@arg Id id of the PTile which should be propagated
%	@arg PTile current PTile which may or may not be propagated
%	@arg Queue old queue of Ids to be processed
%	@arg PropagatedPTile PTile after propagation or the PTile itself
%	@arg ModifiedQueue a new Queue with potentially enqueued ids
%
%	Tests if the PTile's Id matches Id and if it does
%	it runs propagation on the PTile returning modified PTile and Queue
%	otherwise it returns PTile and Queue unchanged. 
propagate_func(Id, PTile, Queue, PropagatedPTile, ModifiedQueue) :-
	(
		PTile = tile(Id, _, _, _, _)
	->
		propagate_tile(PTile, Queue, PropagatedPTile, ModifiedQueue)
	;
		PropagatedPTile = PTile,
		ModifiedQueue = Queue
	).

%!	propagate_tile(+PTile, +Queue, -PropagatedPTile, -ModifiedQueue)
%
%	@arg PTile PTile which will be propagated
%	@arg Queue old queue of Ids to be processed
%	@arg PropagatedPTile PTile after propagation
%	@arg ModifiedQueue a new Queue with potentially enqueued ids
%
%	First it runs word propagators for both horizontal and vertical slots,
%	gathering possible letters.
%	Then both sets of letters are merged.
%	After that letter propagators are run using letters from previous step.
%	The PTile which is returned is recreated from parts.
propagate_tile(
		PTile,
		Queue,
		tile(Id, Tile, PropagatedHoriz, PropagatedVert, PropagatedLetters),
		ModifiedQueue) :-
	PTile = tile(Id, Tile, Horiz, Vert, _),
	propagate_words(PTile, Horiz, Queue, Horiz2, Queue2, HorizLetters),
	propagate_words(PTile, Vert, Queue2, Vert2, Queue3, VertLetters),
	merge_letters(HorizLetters, VertLetters, PropagatedLetters),
	propagate_letters(
			PTile,
			Horiz2,
			PropagatedLetters,
			Queue3,
			PropagatedHoriz,
			Queue4),
	propagate_letters(
			PTile,
			Vert2,
			PropagatedLetters,
			Queue4,
			PropagatedVert,
			ModifiedQueue).

%!	propagate_words(
%!			+PTile,
%!			+Slot,
%!			+Queue,
%!			-PropagatedSlot,
%!			-ModifiedQueue,
%!			-PropagatedLetters)
%	TODO only one clause should unify
%
%	@arg PTile PTile which will be propagated
%	@arg Slot Slot of the PTile to be propagated
%	@arg Queue old queue of Ids to be processed
%	@arg PropagatedSlot Slot after propagation
%	@arg ModifiedQueue a new Queue with potentially enqueued ids
%	@arg PropagatedLetters a set of letters which could be assigned to Tile
%
%	The propagator filters out all words which don't unify with the Slot.
%	It also gathers set of all letters which could be assigned to Tile.
%	After the filtering, new Ids may have been enqueued.
propagate_words(_, nil, Queue, nil, Queue, nil).
propagate_words(
		PTile,
		Slot, 
		Queue, 
		slot(Ids, Tiles, PropagatedWords), 
		ModifiedQueue, 
		PropagatedLetters) :-
	PTile = tile(_, Tile, _, _, _),
	Slot = slot(Ids, Tiles, Words),
	filter_words(Tiles, Tile, Words, PropagatedWords, PropagatedLetters),
	propagate_handle_queue(PTile, Slot, PropagatedWords, Queue, ModifiedQueue).

%!	mergeLetters(+L1, +L2, -L)
%
%	@arg L1 first list or nil
%	@arg L2 second list of nil
%	@arg L merged list or nil
%
%	If both lists are nil, nil is returned.
%	If one list is not nil, it is returned.
%	If both lists are not nil, their intersection is returned.
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

%!	propagate_letters(
%!			+PTile,
%!			+Slot,
%!			+AllowedLetters,
%!			+Queue,
%!			-PropagatedSlot,
%!			-ModifiedQueue)
%	TODO only one clause should unify
%
%	@arg PTile PTile which will be propagated
%	@arg Slot Slot of the PTile to be propagated
%	@arg AllowedLetters set of letters which are alloed to be assigned to Tile
%	@arg Queue old queue of Ids to be processed
%	@arg PropagatedSlot Slot after propagation
%	@arg ModifiedQueue a new Queue with potentially enqueued ids
%
%	The propagator filters out all words which unifies with the Slot
%	but doing so assignes a different letter to Tile than is allowed.
%	After the filtering, new Ids may have been enqueued.
propagate_letters(_, nil, _, Queue, nil, Queue).
propagate_letters(
		PTile,
		Slot, 
		AllowedLetters, 
		Queue, 
		slot(Ids, Tiles, PropagatedWords),
		ModifiedQueue) :-
	PTile = tile(_, Tile, _, _, _),
	Slot = slot(Ids, Tiles, Words),
	filter_words_by_letters(
			Tiles,
			Tile,
			Words, 
			AllowedLetters, 
			PropagatedWords),
	propagate_handle_queue(PTile, Slot, PropagatedWords, Queue, ModifiedQueue).

%!	propagate_handle_queue(
%!			+PTile, 
%!			+Slot, 
%!			+PropagatedWords, 
%!			+Queue, 
%!			-ModifiedQueue)
%
%	@arg PTile PTile being propagated
%	@arg Slot Slot being propagated
%	@arg PropagatedWords updated list of possible words for Slot
%	@arg Queue old queue
%	@arg ModifiedQueue a new Queue with potentially enqueued ids
%
%	If there no words to assign, this predicate fails.
%	If there is only one and the Slot has not been assigned a word yet,
%	the word is unified with the Slot's Tiles and queue is updated.
%	Otherwise, if the list of words is the same, nothing is done.
%	And finally if there are more words and the list changed,
%	the queue is updated.
propagate_handle_queue(
		tile(Id, Tile, _, _, _), 
		slot(Ids, Tiles, Words), 
		PropagatedWords, 
		Queue, 
		ModifiedQueue) :-
	(
		PropagatedWords = []
	->
		fail
	;
		subtract(Ids, [Id], RestIds),
		(
			var(Tile),
			PropagatedWords = [Word]
		->
			Tiles = Word,
			enqueue(Queue, RestIds, ModifiedQueue)
		;
			(
				Words == PropagatedWords
			->
				ModifiedQueue = Queue
			;
				enqueue(Queue, RestIds, ModifiedQueue)
			)
		)
	).