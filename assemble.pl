% assemble.pl

:- module(assemble, [assemble0/3, assemble/4, assemble_line/4, op(150, fx, ':')]).

:- use_module(library(clpfd)).
:- use_module(opcodes).
:- use_module(tools).

% define prefix ':' so we can use it for label syntax
:- op(150, fx, ':').

%
% assemble_stmt(Opcode, Type, Bytes)

% size 1 means, just a one-element list with the hex code of the opcode
assemble_line(Instruction/Mode, [HexCode], _PC, _Values) :-
    opcode(Instruction, Mode, HexCode), !, % assumes statement *is* an opcode
    opcode_size(Mode, 1).

% size 2 opcodes, branching
assemble_line(Instruction/relative, [HexCode, Byte], PC, Values) :-
    opcode(Opcode, relative, HexCode),
    Instruction =.. [Opcode, Value], !,
    translate_value(Value, Values, Address),
    Address - (PC + 2) #= Diff,
    % format("#DEBUG: PC=~w, Address=~w, Diff=~w~n", [PC, Address, Diff]),
    signed_unsigned(Diff, Byte).

% regular size 2 opcodes
assemble_line(Instruction/Mode, [HexCode, Byte], _PC, Values) :-
    opcode(Opcode, Mode, HexCode), 
    % Mode \= relative,
    opcode_size(Mode, 2), 
    Instruction =.. [Opcode, Value], !,  % must not be first
    translate_value(Value, Values, Byte).

% size 3 opcodes
assemble_line(Instruction/Mode, [HexCode, Low, High], _PC, Values) :-
    opcode(Opcode, Mode, HexCode),
    opcode_size(Mode, 3),
    Instruction =.. [Opcode, Value], !,
    translate_value(Value, Values, Address),
    hilo(Address, High, Low).

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
scan_labels([label(Name)|Lines], PC, [Name=PC|Labels]) :-
    scan_labels(Lines, PC, Labels).
scan_labels([:Name|Lines], PC, [Name=PC|Labels]) :-
    scan_labels(Lines, PC, Labels).
scan_labels([Line|Lines], PC, Labels) :-
    Line = _Instruction/Mode, !,
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

assemble([], [], _PC, _Values) :- !.
assemble([Name=Value|Lines], Bytes, PC, Values) :-
    nonvar(Name), !,  % don't try to "make up" X=Y terms in the disassemble step
    assemble(Lines, Bytes, PC, [Name=Value|Values]).
assemble([label(Name)|Lines], Bytes, PC, Values) :-
    % label has already been set by pre-scan, so this is a no-op
    nonvar(Name), !, % only matches when assembling, not disassembling
    assemble(Lines, Bytes, PC, Values).
assemble([:Name|Lines], Bytes, PC, Values) :-
    % same as label(Name)
    nonvar(Name), !,
    assemble([label(Name)|Lines], Bytes, PC, Values).
assemble([Line|Lines], [B|Bytes], PC, Values) :-
    assemble_line(Line, [B], PC, Values),
    %display_assemble_status(Line, [B], PC),
    NewPC #= PC + 1, !,
    assemble(Lines, Bytes, NewPC, Values).
assemble([Line|Lines], [B1,B2|Bytes], PC, Values) :-
    assemble_line(Line, [B1,B2], PC, Values),
    %display_assemble_status(Line, [B1,B2], PC),
    NewPC #= PC + 2, !,
    assemble(Lines, Bytes, NewPC, Values).
assemble([Line|Lines], [B1,B2,B3|Bytes], PC, Values) :-
    assemble_line(Line, [B1,B2,B3], PC, Values),
    %display_assemble_status(Line, [B1,B2,B3], PC),
    NewPC #= PC + 3, !,
    assemble(Lines, Bytes, NewPC, Values).

