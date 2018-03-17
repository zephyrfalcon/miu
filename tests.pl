% tests.pl

% :- consult(miu).
:- use_module(tools).
:- use_module(format).
:- use_module(assemble).
:- use_module(opcodes).
:- use_module(library(clpfd)).

% these files contain the actual tests:
:- consult(test_opcodes).
:- consult(test_assemble).
:- consult(test_tools).
:- consult(test_format).

go :-
    run_tests.

