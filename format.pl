% format.pl
% Format assembler instructions (in Prolog format) to strings, and vice versa.

% (It would be cool if this also was bidirectional... maybe using a DCG or
% something... not sure if that's possible. :-)

:- module(format, []).

format_as_hex(Number, Size, HexString) :-
    format(string(FormatString), "~~|~~`0t~~16R~~~w+", [Size]),
    format(string(HexString), FormatString, [Number]).

% 
% format_instruction(+Instruction/+Mode, -S)
% Format the given instruction (in Prolog format) as a string.

% XXX might need an addition argument just for the branching opcodes??

format_instruction(Instruction/absolute, S) :-
    Instruction =.. [Opcode, Address],
    string_upper(Opcode, StrOpcode),
    format_as_hex(Address, 4, StrAddress),
    format(string(S), "~w $~w", [StrOpcode, StrAddress]), !.

format_instruction(Instruction/absolute_x, S) :-
    Instruction =.. [Opcode, Address],
    string_upper(Opcode, StrOpcode),
    format_as_hex(Address, 4, StrAddress),
    format(string(S), "~w $~w,X", [StrOpcode, StrAddress]), !.

format_instruction(Instruction/absolute_y, S) :-
    Instruction =.. [Opcode, Address],
    string_upper(Opcode, StrOpcode),
    format_as_hex(Address, 4, StrAddress),
    format(string(S), "~w $~w,Y", [StrOpcode, StrAddress]), !.

format_instruction(Instruction/accumulator, S) :-
    string_upper(Instruction, StrUpper),
    format(string(S), "~w A", [StrUpper]), !.

format_instruction(Instruction/immediate, S) :-
    Instruction =.. [Opcode, Byte],
    string_upper(Opcode, StrOpcode),
    format_as_hex(Byte, 2, StrByte),
    format(string(S), "~w #$~w", [StrOpcode, StrByte]), !.

format_instruction(Instruction/implied, S) :-
    string_upper(Instruction, StrUpper),
    format(string(S), "~w", [StrUpper]), !.

format_instruction(Instruction/indirect, S) :-
    Instruction =.. [Opcode, Address],
    string_upper(Opcode, StrOpcode),
    format_as_hex(Address, 4, StrAddress),
    format(string(S), "~w ($~w)", [StrOpcode, StrAddress]), !.

format_instruction(Instruction/indirect_x, S) :-
    Instruction =.. [Opcode, Byte],
    string_upper(Opcode, StrOpcode),
    format_as_hex(Byte, 2, StrByte),
    format(string(S), "~w ($~w,X)", [StrOpcode, StrByte]), !.

format_instruction(Instruction/indirect_y, S) :-
    Instruction =.. [Opcode, Byte],
    string_upper(Opcode, StrOpcode),
    format_as_hex(Byte, 2, StrByte),
    format(string(S), "~w ($~w),Y", [StrOpcode, StrByte]), !.

% NOTE: these are the branching opcodes; when we write out the assembler code,
% a full address (2 bytes) is usually used, but in memory only one byte is used
% for the destination address. the Prolog formats use a full address (e.g.
% bne(0xC000) so that is what we're using here.
format_instruction(Instruction/relative, S) :-
    Instruction =.. [Opcode, Address],
    string_upper(Opcode, StrOpcode),
    format_as_hex(Address, 4, StrAddress),
    format(string(S), "~w $~w", [StrOpcode, StrAddress]), !.

format_instruction(Instruction/zeropage, S) :-
    Instruction =.. [Opcode, Byte],
    string_upper(Opcode, StrOpcode),
    format_as_hex(Byte, 2, StrByte),
    format(string(S), "~w $~w", [StrOpcode, StrByte]), !.

format_instruction(Instruction/zeropage_x, S) :-
    Instruction =.. [Opcode, Byte],
    string_upper(Opcode, StrOpcode),
    format_as_hex(Byte, 2, StrByte),
    format(string(S), "~w $~w,X", [StrOpcode, StrByte]), !.

format_instruction(Instruction/zeropage_y, S) :-
    Instruction =.. [Opcode, Byte],
    string_upper(Opcode, StrOpcode),
    format_as_hex(Byte, 2, StrByte),
    format(string(S), "~w $~w,Y", [StrOpcode, StrByte]), !.

% invalid instruction and/or mode
format_instruction(Instruction/Mode, _) :-
    existence_error(instruction, Instruction/Mode).
