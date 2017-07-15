% format.pl
% Format assembler instructions (in Prolog format) to strings, and vice versa.

% (It would be cool if this also was bidirectional... maybe using a DCG or
% something... not sure if that's possible. :-)

:- module(format, []).

:- use_module(opcodes).

format_as_hex(Number, Size, HexString) :-
    format(string(FormatString), "~~|~~`0t~~16R~~~w+", [Size]),  % tja
    format(string(HexString), FormatString, [Number]).

format_pattern(absolute, "~w $~w").
format_pattern(absolute_x, "~w $~w,X").
format_pattern(absolute_y, "~w $~w,Y").
format_pattern(accumulator, "~w A").
format_pattern(immediate, "~w #$~w").
format_pattern(implied, "~w").
format_pattern(indirect, "~w ($~w)").
format_pattern(indirect_x, "~w ($~w,X)").
format_pattern(indirect_y, "~w ($~w),Y").
format_pattern(relative, "~w $~w").
format_pattern(zeropage, "~w $~w").
format_pattern(zeropage_x, "~w $~w,X").
format_pattern(zeropage_y, "~w $~w,Y").

% hex_value_size(Mode, PrintSize)
% Determine the print size of an instruction in the given address mode.
% Relative addressing is the odd one out here (value takes one byte in memory,
% but we display a full 16-bit address, so 4 hex numbers).
hex_value_size(relative, 4) :- !.
hex_value_size(Mode, 4) :-
    opcode_size(Mode, 3), !.
hex_value_size(_Mode, 2).

% 
% format_instruction(+Instruction/+Mode, -S)
% Format the given instruction (in Prolog format) as a string.

format_instruction(Opcode/Mode, S) :-
    opcode(Opcode, _, _),  % make sure opcode exists
    string_upper(Opcode, StrOpcode),
    format_pattern(Mode, FormatString), !,
    format(string(S), FormatString, [StrOpcode]).

format_instruction(Instruction/Mode, S) :-
    Instruction =.. [Opcode, Value], 
    opcode(Opcode, Mode, _),  % make sure opcode/mode combination is valid
    string_upper(Opcode, StrOpcode),
    format_pattern(Mode, FormatString),
    hex_value_size(Mode, HexSize),
    format_as_hex(Value, HexSize, HexValue), !,  
    format(string(S), FormatString, [StrOpcode, HexValue]).

% NOTE: re: the branching opcodes; when we write out the assembler code,
% a full address (2 bytes) is usually used, but in memory only one byte is used
% for the destination address. the Prolog formats use a full address (e.g.
% bne(0xC000) so that is what we're using here.

% invalid instruction and/or mode
format_instruction(Instruction/Mode, _) :-
    existence_error(instruction, Instruction/Mode).

% TODO: labels etc

