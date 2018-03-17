% parsing.pl

:- module(parsing, [optional_whitespace//0, 
                    required_whitespace//0,
                    label//1,
                    instruction//1]).

/* 

Current idea:

- Read code in actual assembler syntax (as a list of strings)
- For each line:
  - remove comments
  - strip leading/trailing whitespace
  - single-space the resulting string
  - if empty, remove from list
  - parse the resulting string, i.e. convert it to the appropriate value in 
    "Prolog assembler", like rts/implied, or label(here), etc.

The idea is that the DCGs should be bidirectional, so they can also be used to
*generate* assembler-syntax strings from "Prolog assembler". (Whether this will
work is yet to be seen. ;-)

*/

% Source: https://www.metalevel.at/prolog/dcg
% SWIPL doesn't seem to have this built in. No matter:
list([])     --> [].
list([L|Ls]) --> [L], list(Ls).

% Match 0 or 1 whitespace characters.
optional_whitespace --> [].
optional_whitespace -->
    [W], { char_type(W, space) }.

% Match exactly one whitespace character.
required_whitespace -->
    [W], { char_type(W, space) }.

label(Name) -->
    list(NameChars), 
    `:`, !,
    { atom_chars(Name, NameChars) }.

instruction(label(Name)) -->
    label(Name).

