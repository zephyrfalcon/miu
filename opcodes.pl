% opcodes.pl

:- module(opcodes, [opcode/3, opcode_size/2]).

%
% opcode(Opcode, Mode, HexCode).

opcode(adc, immediate, 0x69).
opcode(adc, zeropage, 0x65).
opcode(adc, zeropage_x, 0x75).
opcode(adc, indirect_x, 0x61).
opcode(adc, indirect_y, 0x71).
opcode(adc, absolute, 0x6D).
opcode(adc, absolute_x, 0x7D).
opcode(adc, absolute_y, 0x79).

opcode(and, immediate, 0x29).
opcode(and, zeropage, 0x25).
opcode(and, zeropage_x, 0x35).
opcode(and, indirect_x, 0x21).
opcode(and, indirect_y, 0x31).
opcode(and, absolute, 0x2D).
opcode(and, absolute_x, 0x3D).
opcode(and, absolute_y, 0x39).

opcode(asl, zeropage, 0x06).
opcode(asl, zeropage_x, 0x16).
opcode(asl, absolute, 0x0E).
opcode(asl, absolute_x, 0x1E).
opcode(asl, accumulator, 0x0A).

opcode(bit, zeropage, 0x24).
opcode(bit, absolute, 0x2C).

opcode(bpl, relative, 0x10).
opcode(bmi, relative, 0x30).
opcode(bvc, relative, 0x50).
opcode(bvs, relative, 0x70).
opcode(bcc, relative, 0x90).
opcode(bcs, relative, 0xB0).
opcode(bne, relative, 0xD0).
opcode(beq, relative, 0xF0).

opcode(brk, implied, 0x00).

opcode(clc, implied, 0x18).

opcode(cld, implied, 0xD8).

opcode(cli, implied, 0x58).

opcode(clv, implied, 0xB8).

opcode(cmp, immediate, 0xC9).
opcode(cmp, zeropage, 0xC5).
opcode(cmp, zeropage_x, 0xD5).
opcode(cmp, absolute, 0xCD).
opcode(cmp, absolute_x, 0xDD).
opcode(cmp, absolute_y, 0xD9).
opcode(cmp, indirect_x, 0xC1).
opcode(cmp, indirect_y, 0xD1).

opcode(cpx, immediate, 0xE0).
opcode(cpx, zeropage, 0xE4).
opcode(cpx, absolute, 0xEC).

opcode(cpy, immediate, 0xC0).
opcode(cpy, zeropage, 0xC4).
opcode(cpy, absolute, 0xCC).

opcode(dec, zeropage, 0xC6).
opcode(dec, zeropage_x, 0xD6).
opcode(dec, absolute, 0xCE).
opcode(dec, absolute_x, 0xDE).

opcode(dex, implied, 0xCA).

opcode(dey, implied, 0x88).

opcode(eor, immediate, 0x49).
opcode(eor, zeropage, 0x45).
opcode(eor, zeropage_x, 0x55).
opcode(eor, indirect_x, 0x41).
opcode(eor, indirect_y, 0x51).
opcode(eor, absolute, 0x4D).
opcode(eor, absolute_x, 0x5D).
opcode(eor, absolute_y, 0x59).

opcode(inc, zeropage, 0xE6).
opcode(inc, zeropage_x, 0xF6).
opcode(inc, absolute, 0xEE).
opcode(inc, absolute_x, 0xFE).

opcode(inx, implied, 0xE8).

opcode(iny, implied, 0xC8).

opcode(jmp, absolute, 0x4C).
opcode(jmp, indirect, 0x6C).

opcode(jsr, absolute, 0x20).

opcode(lda, immediate, 0xA9).
opcode(lda, zeropage, 0xA5).
opcode(lda, zeropage_x, 0xB5).
opcode(lda, absolute, 0xAD).
opcode(lda, absolute_x, 0xBD).
opcode(lda, absolute_y, 0xB9).
opcode(lda, indirect_x, 0xA1).
opcode(lda, indirect_y, 0xB1).

opcode(ldx, immediate, 0xA2).
opcode(ldx, zeropage, 0xA6).
opcode(ldx, zeropage_y, 0xB6).
opcode(ldx, absolute, 0xAE).
opcode(ldx, absolute_y, 0xBE).

opcode(ldy, immediate, 0xA0).
opcode(ldy, zeropage, 0xA4).
opcode(ldy, zeropage_x, 0xB4).
opcode(ldy, absolute, 0xAC).
opcode(ldy, absolute_x, 0xBC).

opcode(lsr, zeropage, 0x46).
opcode(lsr, zeropage_x, 0x56).
opcode(lsr, absolute, 0x4E).
opcode(lsr, absolute_x, 0x5E).
opcode(lsr, accumulator, 0x4A).

opcode(nop, implied, 0xEA).

opcode(ora, immediate, 0x09).
opcode(ora, zeropage, 0x05).
opcode(ora, zeropage_x, 0x15).
opcode(ora, indirect_x, 0x01).
opcode(ora, indirect_y, 0x11).
opcode(ora, absolute, 0x0D).
opcode(ora, absolute_x, 0x1D).
opcode(ora, absolute_y, 0x19).

opcode(pha, implied, 0x48).

opcode(php, implied, 0x08).

opcode(pla, implied, 0x68).

opcode(plp, implied, 0x28).

opcode(rol, zeropage, 0x26).
opcode(rol, zeropage_x, 0x36).
opcode(rol, absolute, 0x2E).
opcode(rol, absolute_x, 0x3E).
opcode(rol, accumulator, 0x2A).

opcode(ror, zeropage, 0x66).
opcode(ror, zeropage_x, 0x76).
opcode(ror, absolute, 0x6E).
opcode(ror, absolute_x, 0x7E).
opcode(ror, accumulator, 0x6A).

opcode(rti, implied, 0x40).

opcode(rts, implied, 0x60).

opcode(sbc, immediate, 0xE9).
opcode(sbc, zeropage, 0xE5).
opcode(sbc, zeropage_x, 0xF5).
opcode(sbc, indirect_x, 0xE1).
opcode(sbc, indirect_y, 0xF1).
opcode(sbc, absolute, 0xED).
opcode(sbc, absolute_x, 0xFD).
opcode(sbc, absolute_y, 0xF9).

opcode(sec, implied, 0x38).

opcode(sed, implied, 0xF8).

opcode(sei, implied, 0x78).

opcode(sta, zeropage, 0x85).
opcode(sta, zeropage_x, 0x95).
opcode(sta, indirect_x, 0x81).
opcode(sta, indirect_y, 0x91).
opcode(sta, absolute, 0x8D).
opcode(sta, absolute_x, 0x9D).
opcode(sta, absolute_y, 0x99).

opcode(stx, zeropage, 0x86).
opcode(stx, zeropage_y, 0x96).
opcode(stx, absolute, 0x8E).

opcode(sty, zeropage, 0x84).
opcode(sty, zeropage_x, 0x94).
opcode(sty, absolute, 0x8C).

opcode(tax, implied, 0xAA).

opcode(tay, implied, 0xA8).

opcode(tsx, implied, 0xBA).

opcode(txa, implied, 0x8A).

opcode(txs, implied, 0x9A).

opcode(tya, implied, 0x98).

%
% opcode_size(Type, Size)
% The size of each instruction type, in bytes. Includes the operand.

opcode_size(absolute, 3).
opcode_size(absolute_x, 3).
opcode_size(absolute_y, 3).
opcode_size(accumulator, 1).
opcode_size(implied, 1).
opcode_size(immediate, 2).
opcode_size(indirect, 3).
opcode_size(indirect_x, 2).
opcode_size(indirect_y, 2).
opcode_size(relative, 2).
opcode_size(zeropage, 2).
opcode_size(zeropage_x, 2).
opcode_size(zeropage_y, 2).


