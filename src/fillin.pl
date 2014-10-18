:- use_module(fillin_io).
:- use_module(fillin_cp).

%!	main(+TemplateFile, +WordlistFile, +SolutionFile)
%
%	@arg TemplateFile file containing the template of the puzzle
%	@arg WordlistFile file containing list of words to fill in
%	@arg SolutionFile output file of the solved puzzle
%
%	This predicate is the entry to the puzzle solving program.
%	It reads both input files, then it calls the Constraint Solver,
%	which solves the puzzle and returns the solution.
%	In the end, the solution is written into the output file.
main(TemplateFile, WordlistFile, SolutionFile) :-
	read_template(TemplateFile, Template),
	read_wordlist(WordlistFile, Wordlist),
	solve_puzzle(Template, Wordlist, Solution),
	write_puzzle(SolutionFile, Solution).