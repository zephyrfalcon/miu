% miu.pl

% TODO:
% - data
% - handle unrecognized opcodes 
% - if there's only one version of an opcode, allow omitting the /mode
% - parse strings like "LDA #$01"
% - conversely, translate terms like lda(0x01)/immediate to "LDA #$01"
% - add proper tests
% - refactor
% - command line interface
% - fill out all opcodes

:- use_module(library(clpfd)).

%
% opcode(Opcode, Mode, HexCode).

opcode(bne, relative, 0xD0).

opcode(lda, immediate, 0xA9).
opcode(lda, zeropage, 0xA5).
opcode(lda, zeropage_x, 0xB5).
opcode(lda, absolute, 0xAD).
opcode(lda, absolute_x, 0xBD).
opcode(lda, indirect_x, 0xA1).
opcode(lda, indirect_y, 0xB1).

opcode(ldx, immediate, 0xA2).
opcode(ldx, zeropage, 0xA6).
opcode(ldx, zeropage_y, 0xB6).
opcode(ldx, absolute, 0xAE).
opcode(ldx, absolute_y, 0xBE).

opcode(jsr, absolute, 0x20).

opcode(nop, implied, 0xEA).

opcode(rts, implied, 0x60).

%
% opcode_size(Type, Size)
% The size of each instruction type, in bytes. Includes the operand.

opcode_size(absolute, 3).
opcode_size(absolute_x, 3).
opcode_size(absolute_y, 3).
opcode_size(implied, 1).
opcode_size(immediate, 2).
opcode_size(indirect_x, 2).
opcode_size(indirect_y, 2).
opcode_size(relative, 2).
opcode_size(zeropage, 2).
opcode_size(zeropage_y, 2).

% 16-bit number to/from high and low bytes.
hilo(Word, High, Low) :-
    var(Word),
    Word is High * 256 + Low, !.
hilo(Word, High, Low) :-
    var(High), 
    var(Low),
    High is (Word >> 8) /\ 0xFF,        % (word >> 8) & 0xFF
    Low is Word /\ 0xFF, !.      % word & 0xFF

%
% assemble_stmt(Opcode, Type, Bytes)

% size 1 means, just a one-element list with the hex code of the opcode
assemble_line(Instruction/Mode, [HexCode], _PC, _Values) :-
    opcode(Instruction, Mode, HexCode), % assumes statement *is* an opcode, no frills
    opcode_size(Mode, 1), !.

% size 2 opcodes, branching
assemble_line(Instruction/relative, [HexCode, Byte], PC, Values) :-
    opcode(Opcode, relative, HexCode),
    Instruction =.. [Opcode, Value],
    translate_value(Value, Values, Address),
    Address - (PC + 2) #= Diff,
    % format("#DEBUG: PC=~w, Address=~w, Diff=~w~n", [PC, Address, Diff]),
    signed_unsigned(Diff, Byte),
    !.

% regular size 2 opcodes
assemble_line(Instruction/Mode, [HexCode, Byte], _PC, Values) :-
    opcode(Opcode, Mode, HexCode), 
    % Mode \= relative,
    opcode_size(Mode, 2), 
    Instruction =.. [Opcode, Value], % must not be first
    translate_value(Value, Values, Byte), !.

% size 3 opcodes
assemble_line(Instruction/Mode, [HexCode, Low, High], _PC, Values) :-
    opcode(Opcode, Mode, HexCode),
    opcode_size(Mode, 3),
    Instruction =.. [Opcode, Value],
    translate_value(Value, Values, Address),
    hilo(Address, High, Low), !.

%
% display_assemble_status(Line, Bytes, PC)

display_assemble_status(Line, Bytes, PC) :-
    format("~|~`0t~16R~4+: ", PC),
    format("~w => ", [Line]),
    write_hex_bytes(Bytes).

%
% scan_labels(Lines, StartAddress, Labels)

scan_labels(Lines, _PC, []) :-
    var(Lines), !.  % just succeed if we're disassembling
scan_labels([], _StartAddress, []).
scan_labels([Line|Lines], PC, [Name=PC|Labels]) :-
    Line = label(Name), !,
    format("label ~w set to ~16R~n", [Name, PC]),
    scan_labels(Lines, PC, Labels).
scan_labels([Line|Lines], PC, Labels) :-
    Line = _Instruction/Mode, !,
    % opcode(_Opcode, Mode, _),  % seems redundant
    opcode_size(Mode, Size),
    NewPC #= PC + Size, !,
    scan_labels(Lines, NewPC, Labels).
scan_labels([_Other|Lines], PC, Labels) :-
    scan_labels(Lines, PC, Labels).

%
% assemble(Statements, Bytes)

assemble0(Lines, Bytes, PC) :-
    scan_labels(Lines, PC, Values), !,  
    assemble(Lines, Bytes, PC, Values).

assemble([], [], _PC, _Values).
assemble([Name=Value|Lines], Bytes, PC, Values) :-
    nonvar(Name), !, % don't try to "make up" X=Y terms in the disassemble step
    assemble(Lines, Bytes, PC, [Name=Value|Values]).
assemble([label(Name)|Lines], Bytes, PC, Values) :-
    % label has already been set by pre-scan, so this is a no-op
    nonvar(Name), % only matches when assembling, not disassembling
    assemble(Lines, Bytes, PC, Values).
assemble([Line|Lines], [B|Bytes], PC, Values) :-
    assemble_line(Line, [B], PC, Values),
    display_assemble_status(Line, [B], PC),
    NewPC #= PC + 1,
    assemble(Lines, Bytes, NewPC, Values).
assemble([Line|Lines], [B1,B2|Bytes], PC, Values) :-
    assemble_line(Line, [B1,B2], PC, Values),
    display_assemble_status(Line, [B1,B2], PC),
    NewPC #= PC + 2,
    assemble(Lines, Bytes, NewPC, Values).
assemble([Line|Lines], [B1,B2,B3|Bytes], PC, Values) :-
    assemble_line(Line, [B1,B2,B3], PC, Values),
    display_assemble_status(Line, [B1,B2,B3], PC),
    NewPC #= PC + 3,
    assemble(Lines, Bytes, NewPC, Values).

%
% lookup_name(Name, Values, Result)
% One-way lookup of Name in Values. Stops at the first result found.
% Produces an error if Name is not found.

lookup_name(Name, [], _Value) :-
    existence_error(name, Name), !.
lookup_name(Name, [Name=Value|_Values], Value) :- !.
lookup_name(Name, [_|Values], Value) :-
    lookup_name(Name, Values, Value).

%
% translate_value(Value, Values, Byte)
% If Value is a name (atom), then look it up and produce its value.
% Otherwise, it evaluates to itself.

translate_value(Value, Values, Byte) :-
    atom(Value), !,
    lookup_name(Value, Values, Byte), !.
translate_value(Value, _, Value).

%
% tools...

signed_unsigned(X, X) :-
    X #>= 0, X #=< 127, !.
signed_unsigned(Signed, Unsigned) :-
    Signed #< 0, Signed #>= -128,
    Signed #= Unsigned - 256, !.
signed_unsigned(Signed, _) :-
    domain_error(signed_byte, Signed).

write_hex_bytes([]) :- nl.
write_hex_bytes([B|Bs]) :-
    format("~|~`0t~16R~2+ ", [B]),  % don't ask...
    write_hex_bytes(Bs).

%
% test test...

try_rts :-
    % assemble
    assemble_line(rts/implied, Bytes, 0xC000, []),
    write_hex_bytes(Bytes),
    % disassemble!
    assemble_line(Instruction/Mode, [0x60], 0xC000, []),
    format("~w ~w~n", [Instruction, Mode]).

try_lda_immediate :-
    % assemble
    assemble_line(lda(66)/immediate, Bytes, 0xC000, []),
    write_hex_bytes(Bytes),
    % disassemble
    assemble_line(Instruction/Mode, [0xA9, 66], 0xC000, []),
    format("~w ~w~n", [Instruction, Mode]).

try_lda_immediate_with_value :-
    % assemble
    assemble_line(lda(letter_c)/immediate, Bytes, 0xC000, [letter_c=67]),
    write_hex_bytes(Bytes),
    % disassemble
    assemble_line(Instruction/Mode, [0xA9, 67], 0xC000, []),
    format("~w ~w~n", [Instruction, Mode]).

try_jsr :-
    % assemble
    assemble_line(jsr(0xC000)/absolute, Bytes, 0xC000, []),
    write_hex_bytes(Bytes),
    % disassemble
    assemble_line(Instruction/Mode, [0x20, 0x00, 0xC0], 0xC000, []),
    format("~w ~w~n", [Instruction, Mode]).

try_jsr_with_value :-
    % assemble
    assemble_line(jsr(chrout)/absolute, Bytes, 0xC000, [chrout=0xFFD2]),
    write_hex_bytes(Bytes),
    % disassemble
    assemble_line(Instruction/Mode, [0x20, 0xD2, 0xFF], 0xC000, []),
    format("~w ~w~n", [Instruction, Mode]).

try_all :-
    writeln("-- try_all"),
    writeln("assembling:"),
    assemble0([
        letter_c = 67,
        lda(67)/immediate,
        jsr(0xC000)/absolute,
        label(the_end),
        rts/implied
    ], Bytes, 0xC000),
    write_hex_bytes(Bytes),
    writeln("disassembling:"),
    assemble0(Lines, [0xA9, 0x43, 0x20, 0x0, 0xC0, 0x60], 0xC000),
    write_term(Lines, [nl(true), spacing(next_argument)]).

try_labels_scan_forward :-
    writeln("-- try_labels_scan_forward"),
    writeln("assembling:"),
    assemble0([
        jsr(the_end)/absolute,
        label(the_end),
        rts/implied
    ], Bytes, 0xC000),
    write_hex_bytes(Bytes),
    writeln("disassembling:"),
    assemble0(Lines, Bytes, 0xC000),
    write_term(Lines, [nl(true), spacing(next_argument)]).

try_branching :-
    writeln("-- try_branching"),
    writeln("assembling:"),
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
    write_hex_bytes(Bytes),
    writeln("disassembling:"),
    assemble0(Lines, Bytes, 0xC000),
    write_term(Lines, [nl(true), spacing(next_argument)]).

go :-
    try_rts,
    try_lda_immediate,
    try_lda_immediate_with_value,
    try_jsr,
    try_jsr_with_value,
    try_all,
    try_labels_scan_forward,
    try_branching.

