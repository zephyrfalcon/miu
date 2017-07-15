% tools.pl

:- module(tools, [hilo/3, signed_unsigned/2, write_hex_bytes/1,
                  format_as_hex/3]).

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

% TODO: add predicate to convert number to 2- or 4-digit hex value.

format_as_hex(Number, Size, HexString) :-
    format(string(FormatString), "~~|~~`0t~~16R~~~w+", [Size]),
    format(string(HexString), FormatString, [Number]).

