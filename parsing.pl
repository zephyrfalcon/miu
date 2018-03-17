% parsing.pl

:- module(parsing, [optional_whitespace//0, 
                    required_whitespace//0]).

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


