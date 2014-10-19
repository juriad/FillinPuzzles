%!	Provides a queue which respects element order and uniqueness.
%	The elements of the queue are numeral Ids of PTiles.
:- module(fillin_queue, [
		enqueue_all/2, 
		enqueue/3]).

:- use_module(fillin_filter).

%!	enqueue_all(+Puzzle, -Queue)
%
%	@arg Puzzle a puzzle
%	@arg Queue a queue contining all Ids of the Puzzle
%
%	This is a proxy hiding the tail-recursive version.
enqueue_all(Puzzle, Queue) :-
	enqueue_all(Puzzle, [], Queue).

%!	enqueue_all(+Puzzle, -Before, -Queue)
%
%	@arg Puzzle a puzzle
%	@arg Before an accumulator of the Queue
%	@arg Queue a queue contining all Ids of the Puzzle
%
%	Appends Ids of all PTiles in Puzzle to the end of Before.
enqueue_all([], Queue, Queue).
enqueue_all([PRow | PRows], Before, Queue) :-
	filter_ids(PRow, Ids),
	append(Before, Ids, Queue2),
	enqueue_all(PRows, Queue2, Queue).

%!	enqueue(+Queue, +Ids, -QueueWithIds)
%
%	@arg Queue a queue of numbers
%	@arg Ids a list of numbers
%	@arg QueueWithIds a queue with all non-repeating Ids appended
%
%	Only those Ids which are not yet in the queue are appended to its end.
enqueue(Queue, Ids, QueueWithIds) :-
	subtract(Ids, Queue, RestIds),
	append(Queue, RestIds, QueueWithIds).