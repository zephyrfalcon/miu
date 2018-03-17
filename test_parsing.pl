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

:- end_tests(parsing).

