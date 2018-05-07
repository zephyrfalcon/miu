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

test(asm_word) :-
    phrase(parsing:asm_word(X), `$c000`),
    assertion(X = 49152).
test(asm_word) :-
    phrase(parsing:asm_word(X), `$C000`),
    assertion(X = 49152).
test(asm_word, [fail]) :-
    phrase(parsing:asm_word(_), `$FF`).  % too short

test(asm_byte) :-
    phrase(parsing:asm_byte(X), `$c0`),
    assertion(X = 0xC0).
test(asm_byte) :-
    phrase(parsing:asm_byte(X), `$C0`),
    assertion(X = 0xC0).
test(asm_byte, [fail]) :-
    phrase(parsing:asm_byte(_), `$FFFF`).  % too long

test(asm_opcode) :-
    phrase(parsing:asm_opcode(rts/implied), `rts`).
test(asm_opcode) :-
    phrase(parsing:asm_opcode(I), `rts`), !,
    % has choicepoints because in theory there could be more opcodes named
    % 'rts' with different modes... (in practice there aren't, hence the cut)
    assertion(I = rts/implied).
% opcodes can appear in uppercase
test(asm_opcode) :-
    phrase(parsing:asm_opcode(rts/implied), `RTS`).

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
test(instruction_absolute, [fail]) :-
    % this should not succeed; $02 should not match an absolute opcode.
    phrase(instruction(_/absolute), `adc $02`).

test(instruction_absolute_x) :-
    phrase(instruction(I), `adc $D000,X`),
    assertion(I = adc(0xD000)/absolute_x).

test(instruction_absolute_y) :-
    phrase(instruction(I), `lda $C010,Y`),
    assertion(I = lda(0xC010)/absolute_y).

test(instruction_accumulator) :-
    phrase(instruction(I), `lsr a`),
    assertion(I = lsr/accumulator).

test(instruction_immediate) :-
    phrase(instruction(I), `ldy #$FF`),
    assertion(I = ldy(0xFF)/immediate).

test(instruction_indirect) :-
    phrase(instruction(I), `jmp ($E000)`),
    assertion(I = jmp(0xE000)/indirect).
test(instruction_indirect) :-
    phrase(instruction(I), `jmp ( $E001 )`), % allow spaces
    assertion(I = jmp(0xE001)/indirect).

test(instruction_indirect_x) :-
    phrase(instruction(I), `ORA ($E1,X)`),
    assertion(I = ora(0xE1)/indirect_x).
test(instruction_indirect_x) :-
    phrase(instruction(I), `ora ( $E0 , X )`), % allow spaces
    assertion(I = ora(0xE0)/indirect_x).

test(instruction_indirect_y) :-
    phrase(instruction(I), `SBC ($E2),Y`),
    assertion(I = sbc(0xE2)/indirect_y).
test(instruction_indirect_y) :-
    phrase(instruction(I), `sbc ( $E0 ) , Y`), % allow spaces
    assertion(I = sbc(0xE0)/indirect_y).

test(relative) :-
    phrase(instruction(bne(0x0801)/relative), `bne $0801`).
test(relative) :-
    phrase(instruction(I), `bne $0802`),
    assertion(I = bne(0x0802)/relative).

test(zeropage) :-
    phrase(instruction(adc(0x08)/zeropage), `adc $08`).
test(relative) :-
    phrase(instruction(I), `adc $10`),
    assertion(I = adc(0x10)/zeropage).

:- end_tests(parsing).

