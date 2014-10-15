:- module(fillin_filter, [filter_tiles/2, filter_ids/2, filter_words/5, filter_words_by_letters/5, filter_listed_words/3]).

filter_tiles([], []).
filter_tiles([tile(_, Tile, _, _, _) | PTiles], [Tile | Rest]) :-
	filter_tiles(PTiles, Rest).

filter_ids([], []).
filter_ids([tile(Id, _, _, _, _) | PTiles], [Id | Rest]) :-
	filter_ids(PTiles, Rest).

filter_words(_, [], _, [], []).
filter_words(Tile, [Word | Words], List, Filtered, Letters) :-
	filter_words(Tile, Words, List, Rest, RestLetters),
	(
		unifiable(Word, List, Subst)
	->
		Filtered = [Word | Rest],
		(
			var(Tile)
		->
			find_val(Tile, Subst, Letter)
		;
			Letter = Tile
		),
		union([Letter], RestLetters, Letters)
	;
		Filtered = Rest,
		Letters = RestLetters
	).

find_val(Elem, [(X=Y) | Xs], Val) :-
	(
		Elem == X
	->
		Val = Y
	;
		find_val(Elem, Xs, Val)
	).

filter_words_by_letters(_, [], _, _, []).
filter_words_by_letters(Tile, [Word |Words], List, Letters, Filtered) :-
	filter_words_by_letters(Tile, Words, List, Letters, Rest),
	(
		unifiable(Word, List, Subst),
		(
			var(Tile)
		->
			find_val(Tile, Subst, Letter),
			member(Letter, Letters)
		;
			true
		)
	->
		Filtered = [Word | Rest]
	;
		Filtered = Rest
	).

filter_listed_words(Words, ListedWords, Words2) :-
	intersection(Words, ListedWords, Words2).