:- module(fillin_queue,[enqueue_all/2, enqueue/3]).

:- use_module(fillin_filter).

% QUEUING

enqueue_all([], []).
enqueue_all([PRow | PRows], Queue2) :-
	enqueue_all(PRows, Queue),
	filter_ids(PRow, Ids),
	append(Ids, Queue, Queue2).

enqueue(Queue, [], Queue). 
enqueue(Queue, Elems, NewQueue) :-
	subtract(Elems, Queue, RestElems),
	append(Queue, RestElems, NewQueue).