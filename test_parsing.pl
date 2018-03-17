% test_parsing.pl

:- begin_tests(parsing).

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

:- end_tests(parsing).

