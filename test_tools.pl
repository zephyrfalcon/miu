% test_tools.pl

:- begin_tests(tools).

test(hilo) :-
    hilo(0xC000, High, Low),
    assertion([High, Low] = [0xC0, 0x00]),
    hilo(Word, 0xB0, 0x32),
    assertion(Word = 0xB032).

test(signed_unsigned) :-
    signed_unsigned(3, 3),
    signed_unsigned(-128, 0x80),
    signed_unsigned(-127, 0x81),
    signed_unsigned(127, 0x7F).

test(strip_whitespace) :-
    strip_whitespace("  Hello   friends  ", S),
    assertion(S = "Hello   friends").

test(single_spaced) :-
    single_spaced("I  like   cookies", S),
    assertion(S = "I like cookies").

:- end_tests(tools).

