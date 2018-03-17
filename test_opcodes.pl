% test_opcodes.pl

:- begin_tests(opcodes).

% XXX maybe move helper functions to separate module?
%find_duplicates(List, Duplicates)
find_duplicates([], []).
find_duplicates([H|T], [H|Dups]) :-
    member(H, T), !,
    find_duplicates(T, Dups).
find_duplicates([_|T], Dups) :-
    find_duplicates(T, Dups).

% check if all opcodes match certain requirements:
% - their values must be bytes, i.e. 0 >= x >= 255
% - their modes must be known (as an opcode_size/2 fact)
% - there should be no duplicate byte values.
% (this is to catch obvious typos and omissions and such.)

check_opcode(op(_Name, Mode, HexValue)) :-
    assertion(opcode_size(Mode, _)),
    assertion(HexValue #>= 0),
    assertion(HexValue #=< 255).

test(opcodes_valid) :-
    findall(op(Name, Mode, HexValue), opcode(Name, Mode, HexValue), Opcodes),
    maplist(check_opcode, Opcodes).

test(opcode_duplicates) :-
    findall(Byte, opcode(_, _, Byte), Bytes),
    %assertion(find_duplicates(Bytes, [])). % why does this not complain?
    find_duplicates(Bytes, Dups),
    assertion(Dups = []).

:- end_tests(opcodes).


