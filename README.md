# miu

This is Miu, a 6502 cross-assembler written in Prolog. 

## Requirements

* SWI-Prolog 7.x (I currently use 7.5.10. It will likely require the `pcre`
  module in the future, which was introduced in 7.5.4.)

## Rationale

Since Prolog predicates are often multi-directional, I wanted to explore the
idea of using the same code as both an assembler and a disassembler. The idea
actually seems to work. :-) 

More specifically, you can feed `assemble0/3` and
`assemble/4` either lines of Prolog-formatted assembler instructions, or a list
of bytes, and it will produce the other. Consider these simple test cases:

```prolog
test(labels_scan_forward) :-
    assemble0([
        jsr(the_end)/absolute,
        label(the_end),
        rts/implied
    ], Bytes, 0xC000),
    assertion(Bytes = [0x20, 0x03, 0xC0, 0x60]).

test(labels_scan_forward) :-
    assemble0(Lines, [0x20, 0x03, 0xC0, 0x60], 0xC000),
    assertion(Lines = [jsr(0xC003)/absolute, rts/implied]).
```

## Status

**2018-03-07**: **This branch** (`dcgs-for-generating`) is here mostly to preserve my earlier attempt to use DCGs for both parsing of input and generating of output. The generating will work, *in theory*... but in reality is unusably slow. As I understand it, to generate a number, it will try all (hex) digit combinations until it finds the right ones. It will also try several opcodes of the same mode, until the right one is found, which by itself is not so costly, but the resulting recalculation of the number is.

The actual assembling and disassembling works, that is, assembling from a
Prolog semi-DSL to a list of bytes, and vice versa. What is not implemented
yet, is dis/assembling from/to a more regular text format. Nor is there
currently a workable user interface or command line options. These will be
added once the `pcre` module is a bit more stable (so code will be less likely
to break), or when I master Prolog DCGs, whichever comes first. ;-)
