:- module(fillin_io, [read_template/2, read_wordlist/2, puzzle_to_template/2, write_puzzle/2, write_template/2, write_wordlist/2]).

:- use_module(fillin_functions).

% READING STUFF

read_template(Filename, Puzzle) :-
	read_file(Filename, Puzzle).

read_wordlist(Filename, Wordlist) :-
	read_file(Filename, Wordlist).

% ACTUAL READING

read_file(Filename, Content) :-
	open(Filename, read, Stream),
	read_lines(Stream, Content),
	close(Stream).

read_lines(Stream, Content) :-
	read_line(Stream, Line, Last),
	(
		Last = true
	->
		(
			Line = []
		->
			Content = []
		;
			Content = [Line]
		)
	;
		Content = [Line | Content1],
		read_lines(Stream, Content1)
	).

read_line(Stream, Line, Last) :-
	get_char(Stream, Char),
	(
		Char = end_of_file
	->
		Line = [],
		Last = true
	;
		(
			Char = '\n'
		->
			Line = [],
			Last = false
		;
			Line = [Char | Line1],
			read_line(Stream, Line1, Last)
		)
	).

% WRITING PUZZLE

write_puzzle(Filename, Puzzle) :-
	puzzle_to_template(Puzzle, Template),
	write_file(Filename, Template).

puzzle_to_template(Puzzle, Template) :-
	puzzle_map(Puzzle, puzzle_to_template_func, Template).

puzzle_to_template_func(tile(_, Tile, _, _, _), TTile) :-
	(
		var(Tile)
	->
		TTile = '_'
	;
		TTile = Tile
	).

write_template(Filename, Template) :-
	write_file(Filename, Template).

write_wordlist(Filename, Wordlist) :-
	write_file(Filename, Wordlist).

% ACTUAL WRITING

write_file(Filename, Content) :-
	open(Filename, write, Stream),
	write_rows(Stream, Content),
	close(Stream).

write_rows(_, []).
write_rows(Stream, [Row | Rows]) :-
	write_row(Stream, Row),
	nl(Stream),
	write_rows(Stream, Rows).

write_row(_, []).
write_row(Stream, [Char | Chars]) :-
	put_char(Stream, Char),
	write_row(Stream, Chars).