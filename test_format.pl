% test_format.pl

:- begin_tests(format).

test(format_as_hex) :-
    format:format_as_hex(255, 2, S1),
    assertion(S1 = "FF"),
    format:format_as_hex(255, 4, S2),
    assertion(S2 = "00FF"),
    format:format_as_hex(49152, 4, S3),
    assertion(S3 = "C000").

test(format_instruction) :-
    format:format_instruction(rts/implied, S),
    assertion(S = "RTS").

test(format_instruction) :-
    format:format_instruction(jsr(49152)/absolute, S),
    assertion(S = "JSR $C000").

test(format_instruction) :-
    format:format_instruction(sta(0xBC)/zeropage, S),
    assertion(S = "STA $BC").

test(format_instruction) :-
    format:format_instruction(lda(4)/immediate, S),
    assertion(S = "LDA #$04").

test(format_instruction) :-
    format:format_instruction(lda(0xC0)/indirect_x, S),
    assertion(S = "LDA ($C0,X)").

test(format_instruction) :-
    format:format_instruction(lda(0xC1)/indirect_y, S),
    assertion(S = "LDA ($C1),Y").

test(format_instruction) :-
    format:format_instruction(ora(15)/zeropage_x, S),
    assertion(S = "ORA $0F,X").

test(format_instruction) :-
    format:format_instruction(stx(17)/zeropage_y, S),
    assertion(S = "STX $11,Y").

test(format_instruction) :-
    format:format_instruction(ora(49152)/absolute_x, S),
    assertion(S = "ORA $C000,X").

test(format_instruction) :-
    format:format_instruction(and(49153)/absolute_y, S),
    assertion(S = "AND $C001,Y").

test(format_instruction) :-
    format:format_instruction(jmp(49152)/indirect, S),
    assertion(S = "JMP ($C000)").

test(format_instruction) :-
    format:format_instruction(asl/accumulator, S),
    assertion(S = "ASL A").

test(format_instruction) :-
    format:format_instruction(bmi(0xC000)/relative, S),
    assertion(S = "BMI $C000"),
    format:format_instruction(bne(0x0801)/relative, S1),
    assertion(S1= "BNE $0801").

:- end_tests(format).

