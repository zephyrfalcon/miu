% format.pl
% Format assembler instructions (in Prolog format) to strings, and vice versa.

:- module(format, []).

format_as_hex(Number, Size, HexString) :-
    format(string(FormatString), "~~|~~`0t~~16R~~~w+", [Size]),
    format(string(HexString), FormatString, [Number]).

% 
% format_instruction(+Instruction/+Mode, -S)
% Format the given instruction (in Prolog format) as a string.

% XXX might need an addition argument just for the branching opcodes??

format_instruction(Instruction/implied, S) :-
    string_upper(Instruction, StrUpper),
    format(string(S), "~w", [StrUpper]), !.

format_instruction(Instruction/absolute, S) :-
    Instruction =.. [Opcode, Address],
    string_upper(Opcode, StrOpcode),
    format_as_hex(Address, 4, StrAddress),
    format(string(S), "~w $~w", [StrOpcode, StrAddress]), !.

