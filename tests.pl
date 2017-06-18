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

% assemble lda/immediate with value

test(assemble_line_lda) :-
    assemble_line(lda(letter_c)/immediate, Bytes, 0xC000, [letter_c=67]),
    assertion(Bytes = [0xA9, 67]).

% When disassembling, the list of values will/should be empty, but even if it
% is not, the result should not be affected:
test(assemble_line_lda) :-
    assemble_line(Instruction/Mode, [0xA9, 67], 0xC000, [letter_c=67]),
    assertion(Instruction/Mode = lda(67)/immediate).

% dis/assemble jsr

test(assemble_line_jsr) :-
    assemble_line(jsr(0xC000)/absolute, Bytes, 0xC000, []),
    assertion(Bytes = [0x20, 0x00, 0xC0]).

test(assemble_line_jsr) :-
    assemble_line(Instruction/Mode, [0x20, 0x00, 0xC0], 0xC000, []),
    assertion(Instruction/Mode = jsr(0xC000)/absolute).

% assemble jsr with value

test(assemble_line_jsr) :-
    assemble_line(jsr(chrout)/absolute, Bytes, 0xC000, [chrout=0xFFD2]),
    assertion(Bytes = [0x20, 0xD2, 0xFF]).

:- end_tests(assemble_line).

go :-
    run_tests.
