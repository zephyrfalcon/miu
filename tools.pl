% tools.pl

:- module(tools, [hilo/3, signed_unsigned/2, write_hex_bytes/1,
                  lookup_name/3, translate_value/3, strip_whitespace/2,
                  single_spaced/2]).

:- use_module(library(clpfd)).

% 16-bit number to/from high and low bytes.
hilo(Word, High, Low) :-
    var(Word),
    Word is High * 256 + Low, !.
hilo(Word, High, Low) :-
    var(High), 
    var(Low),
    High is (Word >> 8) /\ 0xFF,  % (word >> 8) & 0xFF
    Low is Word /\ 0xFF, !.       % word & 0xFF

% TODO: try this with CLP(FD). last time I tried it, it was really slow, but
% maybe I did something wrong.

signed_unsigned(X, X) :-
    X #>= 0, X #=< 127, !.
signed_unsigned(Signed, Unsigned) :-
    Signed #< 0, Signed #>= -128,
    Unsigned #>= 0, Unsigned #=< 255,
    Signed #= Unsigned - 256, !.
%signed_unsigned(Signed, _) :-
%    domain_error(signed_byte, Signed).
% XXX this gives a confusing error when I try e.g. `signed_unsigned(4,5)`,
% which should just fail.

write_hex_bytes([]) :- nl.
write_hex_bytes([B|Bs]) :-
    format("~|~`0t~16R~2+ ", [B]),  % don't ask...
    write_hex_bytes(Bs).

%
% lookup_name(Name, Values, Result)
% One-way lookup of Name in Values. Stops at the first result found.
% Produces an error if Name is not found.

lookup_name(Name, [], _Value) :-
    existence_error(name, Name).
lookup_name(Name, [Name=Value|_Values], Value) :- !.
lookup_name(Name, [_|Values], Value) :-
    lookup_name(Name, Values, Value).

%
% translate_value(Value, Values, Byte)
% If Value is a name (atom), then look it up and produce its value.
% Otherwise, it evaluates to itself.

translate_value(Value, Values, Byte) :-
    atom(Value), !,
    lookup_name(Value, Values, Byte), !.
translate_value(Value, _, Value).

%
% strip_whitespace(+SIn, -SOut)
% Strip a string SIn from leading and trailing whitespace, producing SOut.

strip_whitespace(SIn, SOut) :-
    split_string(SIn, "", " \n\t\r", [SOut]).

%
% single_spaced(+SIn, -SOut)
% Look for contiguous whitespace (i.e. 2 or more consecutive whitespace
% characters) in string SIn, and replace each occurrence with a single space,
% producing SOut.
% (SIn is silently assumed to have no leading or trailing whitespace.)

single_spaced(SIn, SOut) :-
    split_string(SIn, " \t", " \t", Parts),
    atomics_to_string(Parts, " ", SOut).

