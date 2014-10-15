% You can use this code to get your started with your fillin puzzle solver.

% main("UNIMELB/prolog/FillinPuzzles/samples/puzzle1", "UNIMELB/prolog/FillinPuzzles/samples/words1", "out").

:- use_module(fillin_io).
:- use_module(fillin_cp).

main(TemplateFile, WordlistFile, SolutionFile) :-
	read_template(TemplateFile, Template),
	read_wordlist(WordlistFile, Wordlist),
	solve_puzzle(Template, Wordlist, Solution),
	write_puzzle(SolutionFile, Solution).

:- use_module(fillin_random).

tests(Size, Average) :-
	Alphabet = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o'],
	random_template(Size, Size, Alphabet, Average, Template, Wordlist),
	solve_puzzle(Template, Wordlist, Solution),
	write_puzzle("out", Solution).