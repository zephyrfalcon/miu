% tests.pl

:- consult(miu).

:- begin_tests(assemble_line).

% dis/assemble rts

test(assemble_line, []) :-
    assemble_line(rts/implied, Bytes, 0xC000, []),
    assertion(Bytes = [0x60]).

test(assemble_line, []) :-
    assemble_line(Instruction/Mode, [0x60], 0xC000, []),
    assertion(Instruction/Mode = rts/implied).

:- end_tests(assemble_line).

go :-
    run_tests.
