/**	<module> Filtering Tools

	Various filtering functions which are used either for conversion
	or limiting number of possibilities.
	
	@author Adam Juraszek <juriad@gmail.com>
*/
:- module(fillin_filter, [
		filter_tiles/2, 
		filter_ids/2, 
		filter_words/5, 
		filter_words_by_letters/5, 
		filter_listed_words/3]).

%!	filter_tiles(+PTiles, -Tiles).
%
%	@arg PTiles list of PTiles
%	@arg Tiles list of Tiles
%
%	Transforms list of PTiles to list of Tiles.
%	The order of Tiles is the same as before transmation.
filter_tiles([], []).
filter_tiles([tile(_, Tile, _, _, _) | PTiles], [Tile | Rest]) :-
	filter_tiles(PTiles, Rest).

%!	filter_tiles(+PTiles, -Ids).
%
%	@arg PTiles list of PTiles
%	@arg Ids list of Ids
%
%	Transforms list of PTiles to list of Ids.
%	The order of Ids is the same as before transmation.
filter_ids([], []).
filter_ids([tile(Id, _, _, _, _) | PTiles], [Id | Rest]) :-
	filter_ids(PTiles, Rest).

%!	filter_words(+Tiles, +Tile, +Words, -FilteredWords, -Letters).
%
%	@arg Tiles list of Tiles is slot which is being processed
%	@arg Tile the current tile in the list of Tiles
%	@arg Words old list of all possible words
%	@arg FilteredWords new list of all possible words
%	@arg Letters set of letters which could be assigned to Tile
%
%	Filters out all words which cannot be assigned to Tiles
%	by trying to unify them.
%	Failure may happen when the sizes are different
%	or when some Tiles are already filled in.
%
%	It also gathers set of all letters at the Tile position
%	which could be assigned by filling Tiles with words from FilteredWords.
%
%	This predicate just calls its tail-recursive version.
filter_words(_, _, [], [], []).
filter_words(Tiles, Tile, Words, FilteredWords, Letters) :-
	filter_words(Tiles, Tile, Words, [], FilteredWordsR, [], Letters),
	reverse(FilteredWordsR, FilteredWords).

%!	filter_words(
%!			+Tiles, 
%!			+Tile, 
%!			+Words, 
%!			+WordsAccumulator, 
%!			-FilteredWords, 
%!			+LettersAccumulator, 
%!			-Letters).
%
%	@arg Tiles list of Tiles is slot which is being processed
%	@arg Tile the current tile in the list of Tiles
%	@arg Words old list of all possible words
%	@arg WordsAccumulator filtered words so far
%	@arg FilteredWords new list of all possible words
%	@arg LettersAccumulator gathered letters so far
%	@arg Letters set of letters which could be assigned to Tile
%
%	Filters out all words which cannot be assigned to Tiles
%	by trying to unify them.
%	Failure may happen when the sizes are different
%	or when some Tiles are already filled in.
%
%	It also gathers set of all letters at the Tile position
%	which could be assigned by filling Tiles with words from FilteredWords.
filter_words(_, _, [], FilteredWords, FilteredWords, Letters, Letters).
filter_words(
		Tiles, 
		Tile, 
		[Word | Words], 
		WordsAccumulator, 
		FilteredWords,
		LettersAccumulator, 
		Letters) :-
	(
		unifiable(Word, Tiles, Subst)
	->
		WordsAccumulator2 = [Word | WordsAccumulator],
		(
			var(Tile)
		->
			find_val(Subst, Tile, Letter)
		;
			Letter = Tile
		),
		union([Letter], LettersAccumulator, LettersAccumulator2)
	;
		WordsAccumulator2 = WordsAccumulator,
		LettersAccumulator2 = LettersAccumulator
	),
	filter_words(
			Tiles, 
			Tile, 
			Words, 
			WordsAccumulator2, 
			FilteredWords, 
			LettersAccumulator2, 
			Letters).

%!	filter_words_by_letters(+Tiles, +Tile, +Words, +Letters, -FilteredWords).
%
%	@arg Tiles list of Tiles is slot which is being processed
%	@arg Tile the current tile in the list of Tiles
%	@arg Words old list of all possible words
%	@arg Letters set of letters which could be used in Tile
%	@arg FilteredWords new list of all possible words
%
%	Filters out all words which cannot be assigned to Tiles
%	by trying to unify them and testing
%	if their letter at Tile position is one of those in set of Letters.
%
%	This predicate just calls its tail-recursive version.
filter_words_by_letters(_, _, [], _, []).
filter_words_by_letters(Tiles, Tile, Words, Letters, FilteredWords) :-
	filter_words_by_letters(Tiles, Tile, Words, Letters, [], FilteredWordsR),
	reverse(FilteredWordsR, FilteredWords).

%!	filter_words_by_letters(
%!			+Tiles, 
%!			+Tile, 
%!			+Words, 
%!			+Letters, 
%!			+WordsAccumulator, 
%!			-FilteredWords).
%
%	@arg Tiles list of Tiles is slot which is being processed
%	@arg Tile the current tile in the list of Tiles
%	@arg Words old list of all possible words
%	@arg Letters set of letters which could be used in Tile
%	@arg WordsAccumulator filtered words so far
%	@arg FilteredWords new list of all possible words
%
%	Filters out all words which cannot be assigned to Tiles
%	by trying to unify them and testing
%	if their letter at Tile position is one of those in set of Letters.
filter_words_by_letters(_, _, [], _, FilteredWords, FilteredWords).
filter_words_by_letters(
		Tiles, 
		Tile, 
		[Word | Words], 
		Letters, 
		WordsAccumulator, 
		FilteredWords) :-
	(
		unifiable(Word, Tiles, Subst),
		(
			var(Tile)
		->
			find_val(Subst, Tile, Letter),
			member(Letter, Letters)
		;
			true
		)
	->
		WordsAccumulator2 = [Word | WordsAccumulator]
	;
		WordsAccumulator2 = WordsAccumulator
	),
	filter_words_by_letters(
			Tiles, 
			Tile, 
			Words, 
			Letters, 
			WordsAccumulator2, 
			FilteredWords).


%!	find_val(+Var, +Substitutions, -Val).
%!	find_val(+Var, +Substitutions, +Val).
%
%	@arg Substitutions list of Var=Val pairs
%	@arg Var left side of pair to search for
%	@arg Val right side of pair which has Var on the left side
%
%	Searches for a value to which Var would be substituted.
%	Or it tests if there is a pair of Var=Val.
find_val([(X=Y) | Xs], Var, Val) :-
	(
		Var == X
	->
		Val = Y
	;
		find_val(Xs, Var, Val)
	).

%!	filter_listed_words(+Words, +ListedWords, -FilteredWords).
%
%	@arg Words old list of words
%	@arg ListedWords list of allowed words
%	@arg FilteredWords list of words which are in both lists
%
%	In fact this is just an intersection pof two lists.
filter_listed_words(Words, ListedWords, FilteredWords) :-
	intersection(Words, ListedWords, FilteredWords).