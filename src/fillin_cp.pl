%!	Hides the complexity of constarint solver.
%	Exports only one predicate which solves the puzzle.
:- module(fillin_cp,[solve_puzzle/3]).

:- use_module(fillin_init).
:- use_module(fillin_propagation).
:- use_module(fillin_search).
:- use_module(fillin_queue).

%!	solve_puzzle(+Template, +Wordlist, -Solution)
%
%	arg Template template of a puzzle to be solved
%	arg Wordlist list of words to be filled in in the puzzle
%	arg Solution filled puzzle
%
%	The process of solving contains three stages:
%	1) creation of puzzle from a template
%	2) initial propagation
%	3) recursive search
%
%	A template, which is just a list of lists of chars, is transformed
%	to a list of lists of PTiles.
%	Each PTile is: tile(Id, Tile, Horiz, Vert, Letters)
%	Id is a numerical identifier of the current PTile.
%	Tile is a scalar variable which is eventually bounded to a char.
%	Horiz and Vert are slots: slot(Ids, Tiles, Words)
%	Ids is a list of Ids of PTiles which are part of the slot
%	either in horizontal or vertical direction.
%	Tiles is list of Tiles of PTiles corresponding to PTiles
%	referenced by Ids.
%	Both Ids and Tiles preserve the order of PTiles in the slot.
%	Words is a list of all possible words which could be filled in;
%	it doesn't contain duplicit values;
%	the order of words is not important.
%	Letters is list of chars which can be assigned to that Tile.
%	If the tile is not part of slot (in a direction),
%	then Horiz and/or Vert may be set to nil,
%	as may be Letters if the PTile as a border.
%	The transformation into a puzzle is performed in a special module.
%
%	After an empty puzzle is created (possibly with some prefilled tiles),
%	all PTiles are enqueued to be processed by set of propagators.
%	The initial propagation differs from propagations performed later
%	in the processing by enqueuing all PTiles instead of
%	only the changed ones.
%	This is intensional as we want to limit the possibilites to try
%	as early as possible, in this case before searching,
%	which means that there are no decisions to undo.
%
%	The search takes an empty PTile, assigns its value.
%	Then it enqueues the filled PTile and runs the propagation.
%	In the end it calls search again with the filtered puzzle.
%	If there are no empty PTiles, the puzzle is returned.

solve_puzzle(Template, UnsortedWordlist, Solution) :-
	msort(UnsortedWordlist, Wordlist),
	sort(Wordlist, UniqueWordlist),
	create_puzzle(Template, UniqueWordlist, EmptyPuzzle),
	!,
	enqueue_all(EmptyPuzzle, Queue),
	propagate(EmptyPuzzle, Wordlist, Queue, FilteredPuzzle),
	!,
	search(FilteredPuzzle, Wordlist, Solution).