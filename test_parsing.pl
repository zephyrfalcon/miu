% test_parsing.pl

:- begin_tests(parsing).

:- set_prolog_flag(back_quotes, chars).

test(required_whitespace) :-
    phrase(required_whitespace, ` `).
test(required_whitespace, [fail]) :-
    phrase(required_whitespace, `abc`).

test(optional_whitespace) :-
    phrase(optional_whitespace, ` `).
test(optional_whitespace) :-
    phrase(optional_whitespace, ``).

test(label) :-
    phrase(label(Name), `foo:`),
    assertion(Name = foo).
test(label) :-
    phrase(label(Name), `FOO:`),
    assertion(Name = 'FOO').

test(asm_number) :-
    phrase(parsing:asm_number(X), `49152`),
    assertion(X = 49152).
test(asm_number) :-
    phrase(parsing:asm_number(X), `$c000`),
    assertion(X = 49152).
test(asm_number) :-
    phrase(parsing:asm_number(X), `$C000`),
    assertion(X = 49152).
% PROBLEM: we pass a number in there, it generates a list of *codes*, which
% fails the tests!
test(asm_number) :-
    phrase(parsing:asm_number(49152), `49152`).  % ?!
test(asm_number) :-
    phrase(parsing:asm_number(33), S),
    assertion(S = `33`).

% "backwards": produce `foo:` from label(foo)
test(label) :-
    phrase(label(foo), `foo:`).

test(instruction_label) :-
    phrase(instruction(label(Name)), `bar:`),
    assertion(Name = bar).

test(instruction_implied) :-
    phrase(instruction(rts/implied), `rts`).
test(instruction_implied) :-
    phrase(instruction(I), `rts`),
    assertion(I = rts/implied).
test(instruction_implied) :-
    phrase(instruction(rts/implied), Chars),
    assertion(Chars = `rts`).

%test(instruction_absolute) :-
%    phrase(instruction(jmp(0xC000)/absolute), `jmp $c000`).

:- end_tests(parsing).

