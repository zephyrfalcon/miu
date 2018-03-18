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

test(any_of) :-
    phrase(parsing:any_of(`xX`, X), `x`),
    assertion([X] = `x`).

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
test(asm_number) :-
    phrase(parsing:asm_number(49152), `49152`).

test(asm_opcode) :-
    phrase(parsing:asm_opcode(rts/implied), `rts`).
test(asm_opcode) :-
    phrase(parsing:asm_opcode(I), `rts`), !,
    % has choicepoints because in theory there could be more opcodes named
    % 'rts' with different modes... (in practice there aren't, hence the cut)
    assertion(I = rts/implied).

test(instruction_label) :-
    phrase(instruction(label(Name)), `bar:`),
    assertion(Name = bar).

test(instruction_implied) :-
    phrase(instruction(rts/implied), `rts`).
test(instruction_implied) :-
    phrase(instruction(I), `rts`),
    assertion(I = rts/implied).

test(instruction_absolute) :-
    phrase(instruction(jmp(0xC000)/absolute), `jmp $c000`).
test(instruction_absolute) :-
    phrase(instruction(I), `jmp $c000`),
    assertion(I = jmp(0xC000)/absolute).
test(instruction_absolute, [fail]) :-
    % partial matches should fail; e.g. adc(0xC000)/absolute would be a match
    % for the first part of the string, `adc $c000`, but should be rejected
    % because there are still characters left.
    phrase(instruction(_/absolute), `adc $c000,x`).

% TODO: test uppercase

test(instruction_absolute_x) :-
    phrase(instruction(I), `adc $D000,X`),
    assertion(I = adc(0xD000)/absolute_x).

test(instruction_absolute_y) :-
    phrase(instruction(I), `lda $C010,Y`),
    assertion(I = lda(0xC010)/absolute_y).

test(instruction_accumulator) :-
    phrase(instruction(I), `lsr a`),
    assertion(I = lsr/accumulator).


:- end_tests(parsing).

