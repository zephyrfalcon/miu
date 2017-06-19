% opcodes.pl

:- module(opcodes, [opcode/3, opcode_size/2]).

%
% opcode(Opcode, Mode, HexCode).

opcode(bne, relative, 0xD0).

opcode(lda, immediate, 0xA9).
opcode(lda, zeropage, 0xA5).
opcode(lda, zeropage_x, 0xB5).
opcode(lda, absolute, 0xAD).
opcode(lda, absolute_x, 0xBD).
opcode(lda, indirect_x, 0xA1).
opcode(lda, indirect_y, 0xB1).

opcode(ldx, immediate, 0xA2).
opcode(ldx, zeropage, 0xA6).
opcode(ldx, zeropage_y, 0xB6).
opcode(ldx, absolute, 0xAE).
opcode(ldx, absolute_y, 0xBE).

opcode(jsr, absolute, 0x20).

opcode(nop, implied, 0xEA).

opcode(rts, implied, 0x60).

%
% opcode_size(Type, Size)
% The size of each instruction type, in bytes. Includes the operand.

opcode_size(absolute, 3).
opcode_size(absolute_x, 3).
opcode_size(absolute_y, 3).
opcode_size(implied, 1).
opcode_size(immediate, 2).
opcode_size(indirect_x, 2).
opcode_size(indirect_y, 2).
opcode_size(relative, 2).
opcode_size(zeropage, 2).
opcode_size(zeropage_y, 2).


