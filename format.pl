% format.pl

:- module(format, []).

format_as_hex(Number, Size, HexString) :-
    format(string(FormatString), "~~|~~`0t~~16R~~~w+", [Size]),
    format(string(HexString), FormatString, [Number]).

% 
% format_instruction(+Instruction/+Mode, -S)
% Format the given instruction (in Prolog format) as a string.

format_instruction(Instruction/implied, S) :-
    string_upper(Instruction, StrUpper),
    format(string(S), "~w", [StrUpper]), !.

