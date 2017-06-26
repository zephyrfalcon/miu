% tests.pl

:- consult(miu).

:- begin_tests(opcodes).

% XXX maybe move helper functions to separate module?
%find_duplicates(List, Duplicates)
find_duplicates([], []).
find_duplicates([H|T], [H|Dups]) :-
    member(H, T), !,
    find_duplicates(T, Dups).
find_duplicates([H|T], Dups) :-
    find_duplicates(T, Dups).

% check if all opcodes match certain requirements:
% - their values must be bytes, i.e. 0 >= x >= 255
% - their modes must be known (as an opcode_size/2 fact)
% - there should be no duplicate byte values.
% (this is to catch obvious typos and omissions and such.)

check_opcode(op(Name, Mode, HexValue)) :-
    assertion(opcode_size(Mode, _)),
    assertion(HexValue #>= 0),
    assertion(HexValue #=< 255).

test(opcodes_valid) :-
    findall(op(Name, Mode, HexValue), opcode(Name, Mode, HexValue), Opcodes),
    maplist(check_opcode, Opcodes).

test(opcode_duplicates) :-
    findall(Byte, opcode(_, _, Byte), Bytes),
    %assertion(find_duplicates(Bytes, [])). % why does this not complain?
    find_duplicates(Bytes, Dups),
    assertion(Dups = []).

:- end_tests(opcodes).

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

:- begin_tests(assemble0).

program1([0xA9, 0x43, 0x20, 0x00, 0xC0, 0x60]).

test(assemble0) :-
    program1(Code),
    assemble0([
        letter_c = 67,
        lda(67)/immediate,
        jsr(0xC000)/absolute,
        label(the_end),
        rts/implied
    ], Bytes, 0xC000),
    assertion(Bytes = Code).

test(assemble0) :-
    program1(Code),
    assemble0(Lines, Code, 0xC000), 
    assertion(Lines = [lda(67)/immediate, jsr(0xC000)/absolute, rts/implied]).

test(labels_scan_forward) :-
    assemble0([
        jsr(the_end)/absolute,
        label(the_end),
        rts/implied
    ], Bytes, 0xC000),
    assertion(Bytes = [0x20, 0x03, 0xC0, 0x60]).

test(labels_scan_forward) :-
    assemble0(Lines, [0x20, 0x03, 0xC0, 0x60], 0xC000),
    assertion(Lines = [jsr(0xC003)/absolute, rts/implied]).

test(branching) :-
    assemble0([
        label(here),                % here = C000
        nop/implied,                % C000
        nop/implied,                % C001
        bne(here)/relative,         % C002; jump to C000-(C002+2) = -4 = 0xFC
        rts/implied,                % C004
        bne(there)/relative,        % C005; jump to C008-(C005+2) = 1 = 0x01
        nop/implied,                % C007
        label(there)                % there = C008
    ], Bytes, 0xC000),
    assertion(Bytes = [0xEA, 0xEA, 0xD0, 0xFC, 0x60, 0xD0, 0x01, 0xEA]).

test(branching) :-
    assemble0(Lines, [0xEA, 0xEA, 0xD0, 0xFC, 0x60, 0xD0, 0x01, 0xEA], 0xC000),
    assertion(Lines = [nop/implied, nop/implied, bne(0xC000)/relative,
                       rts/implied, bne(0xC008)/relative, nop/implied]).

:- end_tests(assemble0).

go :-
    run_tests.

