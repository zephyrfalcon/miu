% tests.pl

:- set_prolog_flag(back_quotes, chars).

% :- consult(miu).
:- use_module(tools).
:- use_module(format).
:- use_module(assemble).
:- use_module(opcodes).
:- use_module(parsing).
:- use_module(library(clpfd)).

% these files contain the actual tests:
:- consult(test_opcodes).
:- consult(test_assemble).
:- consult(test_tools).
:- consult(test_format).
:- consult(test_parsing).

go :-
    run_tests.

