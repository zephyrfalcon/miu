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

While the DCGs can in theory be made bidirectional, generating output with them
seems to be exceedingly slow, so I am using them for parsing only.

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

asm_opcode(Opcode/Mode) -->
    { opcode(Opcode, Mode, _),
      atom_codes(Opcode, Chars) }, 
    nonblanks(Chars), !.

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
    required_whitespace,
    { Head =.. [Opcode, Address] },
    asm_number(Address), !.
    % TODO: check if address in correct range

% TODO:
% parse_line(Line, Instruction)           (bidirectional?)
% parse_lines(Lines, Instructions)        (bidirectional?)

