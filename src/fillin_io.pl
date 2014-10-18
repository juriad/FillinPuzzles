%!	Reading and writing of puzzles and related things.
%	This module exports fucntions which are meant to be used
%	by the main function of the program.
%	It provides reading and writing of templates and wordlists,
%	and writing and converting puzzles.
:- module(fillin_io, [
		read_template/2,
		read_wordlist/2,
		puzzle_to_template/2,
		write_puzzle/2,
		write_template/2,
		write_wordlist/2]).

:- use_module(fillin_functions).

%!	read_template(+Filename, -Template)
%
%	@arg Filename file containing the template of the puzzle
%	@arg Template the template read from the file
%
%	Reads the template of the puzzle from the file and returns it
%	as a list of lists of chars.
%	Each char is either:
%	* '#' -- border,
%	* '_' -- empty space,
%	* something else -- prefilled letter or digit
read_template(Filename, Template) :-
	read_file(Filename, Template).

%!	read_wordlist(+Filename, -Wordlist)
%
%	@arg Filename file containing the list of words used in the puzzle
%	@arg Wordlist the list of words read from file
%
%	Reads the list of words from the file and returns it
%	as a list of lists of chars.
%	Each inner list is one word.
%	Each word must be used exactly once.
%	Words in Wordlist may repeat. 
read_wordlist(Filename, Wordlist) :-
	read_file(Filename, Wordlist).

%!	read_file(+Filename, -Content)
%
%	@arg Filename file containing list of lines
%	@arg Content the file as list of lines
%
%	Reads whole content of the file.
%	It is returned as list of lines; each line as list of chars.
%	The only character which is handled special is a new-line.
read_file(Filename, Content) :-
	open(Filename, read, Stream),
	read_lines(Stream, Content),
	close(Stream).

%!	read_lines(+Stream, -Content)
%
%	@arg Stream file handle opened for reading
%	@arg Content content of the file until the end
%
%	Reads the content of the stream to the end.
%	It is returned as list of lines; each line as list of chars.
%	The only character which is handled special is a new-line.
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

%!	read_lines(+Stream, -Line, -Last)
%
%	@arg Stream file handle opened for reading
%	@arg Line a single line read from the file
%	@arg Last true iff the line is the last line of the file
%
%	Reads a single line from the stream.
%	The reading stops either with new-line or end_of_file.
%	In the latter case, the output argument Last is set to true.
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

%!	write_puzzle(+Filename, +Puzzle)
%
%	@arg Filename file where the puzzle will be written
%	@arg Puzzle puzzle to write to the file
%
%	Puzzle is first converted to template
%	and the template is then written to the file.
write_puzzle(Filename, Puzzle) :-
	puzzle_to_template(Puzzle, Template),
	write_file(Filename, Template).

%!	puzzle_to_template(+Puzzle, -Template)
%
%	@arg Puzzle puzzle to convert
%	@arg Template converted puzzle in forma of a template
%
%	Each tile of the puzzle is converted to a single char.
%	Border to '#', unfilled tile to '_', otherwise to its char. 
puzzle_to_template(Puzzle, Template) :-
	puzzle_map(Puzzle, puzzle_to_template_func, Template).

%!	puzzle_to_template_func(+PTile, -TTile)
%
%	@arg PTile tile of a puzzle
%	@arg TTile tile of a template
%
%	Converts a puzzle tile to a template tile.
%	This function is meant to be passed to puzzle_map function.
puzzle_to_template_func(tile(_, Tile, _, _, _), TTile) :-
	(
		var(Tile)
	->
		TTile = '_'
	;
		TTile = Tile
	).

%!	write_template(+Filename, +Template)
%
%	@arg Filename file where to write the template
%	@arg Template the template to be written to the file
%
%	Template is list of lists of chars, where outer list is list of lines.
write_template(Filename, Template) :-
	write_file(Filename, Template).

%!	write_wordlist(+Filename, +Wordlist)
%
%	@arg Filename file where to write the wordlist
%	@arg Wordlist the wordlist to be written to the file
%
%	Wordlist is list of lists of chars, where outer list is list of lines.
write_wordlist(Filename, Wordlist) :-
	write_file(Filename, Wordlist).

%!	write_file(+Filename, +Content)
%
%	@arg Filename file where to write the content
%	@arg Content list of lists of chars
%
%	Opens the file, writes all lines and closes the file.
%	If such file already exists, it will be overwritten. 
write_file(Filename, Content) :-
	open(Filename, write, Stream),
	write_line(Stream, Content),
	close(Stream).

%!	write_lines(+Stream, +Lines)
%
%	@arg Stream handle of a file opened for writing
%	@arg Content list of lists of chars
%
%	Writes all the lines into the stream.
%	Each line will be ended with a new line character.
write_lines(_, []).
write_lines(Stream, [Line | Lines]) :-
	write_line(Stream, Line),
	nl(Stream),
	write_lines(Stream, Lines).

%!	write_line(+Stream, +Line)
%
%	@arg Stream handle of a file opened for writing
%	@arg Line list of chars
%
%	Writes all the charcter into the stream.
write_line(_, []).
write_line(Stream, [Char | Chars]) :-
	put_char(Stream, Char),
	write_line(Stream, Chars).