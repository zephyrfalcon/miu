% tests.pl

:- consult(miu).

:- begin_tests(assemble_line).

% dis/assemble rts

test(assemble_line_rts, []) :-
    assemble_line(rts/implied, Bytes, 0xC000, []),
    assertion(Bytes = [0x60]).

test(assemble_line_rts, []) :-
    assemble_line(Instruction/Mode, [0x60], 0xC000, []),
    assertion(Instruction/Mode = rts/implied).

% dis/assemble lda/immediate

test(assemble_line_lda, []) :-
    assemble_line(lda(66)/immediate, Bytes, 0xC000, []),
    assertion(Bytes = [0xA9, 66]).

test(assemble_line_lda, []) :-
    assemble_line(Instruction/Mode, [0xA9, 66], 0xC000, []),
    assertion(Instruction/Mode = lda(66)/immediate).

/*
try_lda_immediate_with_value :-
    % assemble
    assemble_line(lda(letter_c)/immediate, Bytes, 0xC000, [letter_c=67]),
    write_hex_bytes(Bytes),
    % disassemble
    assemble_line(Instruction/Mode, [0xA9, 67], 0xC000, []),
    format("~w ~w~n", [Instruction, Mode]).
*/

% assemble lda/immediate with value

test(assemble_line_lda) :-
    assemble_line(lda(letter_c)/immediate, Bytes, 0xC000, [letter_c=67]),
    assertion(Bytes = [0xA9, 67]).

% When disassembling, the list of values will/should be empty, but even if it
% is not, the result should not be affected:
test(assemble_line_lda) :-
    assemble_line(Instruction/Mode, [0xA9, 67], 0xC000, [letter_c=67]),
    assertion(Instruction/Mode = lda(67)/immediate).

:- end_tests(assemble_line).

go :-
    run_tests.
