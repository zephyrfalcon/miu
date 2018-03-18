% parsing.pl

:- module(parsing, [optional_whitespace//0, 
                    required_whitespace//0,
                    label//1,
                    instruction//1]).

:- use_module(library(dcg/basics)).
:- use_module(opcodes).

/* 

Current idea:

- Read code in actual assembler syntax (as a list of strings)
- For each line:
  - remove comments
  - convert to lowercase
  - strip leading/trailing whitespace
  - single-space the resulting string
  - if empty, remove from list
  - parse the resulting string, i.e. convert it to the appropriate value in 
    "Prolog assembler", like rts/implied, or label(here), etc.
  - these values can then be passed to the assembler, etc

The idea is that the DCGs should be bidirectional, so they can also be used to
*generate* assembler-syntax strings from "Prolog assembler". (Whether this will
work is yet to be seen. ;-)

At least part of the process will not be bidirectional; e.g. the input strings
can be in any case, but should be converted to lowercase in order to match the
opcode names which are stored as atoms like 'rts'; on the other hand, the
output format will (probably) convert them to uppercase, which is more common.

But, the pipeline source <--> Prolog assembler <--> bytes *should* be
bidirectional, if at all possible. That was the whole point of this project
anyway...

*/

% Source: https://www.metalevel.at/prolog/dcg
% SWIPL doesn't seem to have this built in. No matter:
list([])     --> [].
list([L|Ls]) --> [L], list(Ls).

% Match 0 or 1 whitespace characters.
optional_whitespace --> [].
optional_whitespace -->
    [W], { code_type(W, space) }.

% Match exactly one whitespace character.
required_whitespace -->
    [W], { code_type(W, space) }.

% Parse a number, either in decimal or in hexadecimal (in which case it needs
% to be preceded by a "$").
asm_number(N) -->
    `$`,
    xinteger(N), !.
asm_number(N) -->
    integer(N).

% BUG: this totally does not find the right opcode, and I don't know why.
asm_opcode(Opcode/Mode) -->
    nonblanks(Chars),
    { opcode(Opcode, Mode, _),
      atom_codes(Opcode, Chars), ! }.

label(Name) -->
    list(NameChars), 
    `:`, !,
    { atom_codes(Name, NameChars) }.

instruction(label(Name)) -->
    label(Name).

instruction(Opcode/implied) -->
    list(OpcodeChars), 
    { atom_codes(Opcode, OpcodeChars),
      opcode(Opcode, implied, _), ! }.

instruction(Head/absolute) -->
    asm_opcode(Opcode/absolute),
    %{ atom_codes(Opcode, OpcodeChars),
    %  opcode(Opcode, absolute, _), ! },
    required_whitespace,
    asm_number(Address),
    % TODO: check if address in correct range
    { Head =.. [Opcode, Address] }.

% TODO:
% parse_line(Line, Instruction)           (bidirectional)
% parse_lines(Lines, Instructions)   (bidirectional)

