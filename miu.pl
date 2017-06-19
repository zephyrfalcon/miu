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
:- use_module(opcodes).

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
    % format("label ~w set to ~16R~n", [Name, PC]),
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
    nonvar(Name),  % don't try to "make up" X=Y terms in the disassemble step
    assemble(Lines, Bytes, PC, [Name=Value|Values]), !.
assemble([label(Name)|Lines], Bytes, PC, Values) :-
    % label has already been set by pre-scan, so this is a no-op
    nonvar(Name), % only matches when assembling, not disassembling
    assemble(Lines, Bytes, PC, Values), !.
assemble([Line|Lines], [B|Bytes], PC, Values) :-
    assemble_line(Line, [B], PC, Values),
    %display_assemble_status(Line, [B], PC),
    NewPC #= PC + 1,
    assemble(Lines, Bytes, NewPC, Values), !.
assemble([Line|Lines], [B1,B2|Bytes], PC, Values) :-
    assemble_line(Line, [B1,B2], PC, Values),
    %display_assemble_status(Line, [B1,B2], PC),
    NewPC #= PC + 2,
    assemble(Lines, Bytes, NewPC, Values), !.
assemble([Line|Lines], [B1,B2,B3|Bytes], PC, Values) :-
    assemble_line(Line, [B1,B2,B3], PC, Values),
    %display_assemble_status(Line, [B1,B2,B3], PC),
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

go :-
    true.
    % a "real" main program will be added shortly... >.>

