# miu

This is Miu, a 6502 cross-assembler written in Prolog. 

## Requirements

* SWI-Prolog 7.x (I currently use 7.5.10. It will likely require the `pcre`
  module in the future, which was introduced in 7.5.4.)

## Status

The actual assembling and disassembling works, that is, assembling from a
Prolog semi-DSL to a list of bytes, and vice versa. What is not implemented
yet, is dis/assembling from/to a more regular text format. Nor is there
currently a workable user interface or command line options. These will be
added once the `pcre` module is a bit more stable (so code will be less likely
to break), or when I master Prolog DCGs, whichever comes first. ;-)

## Rationale

Since Prolog predicates are often multi-directional, I wanted to explore the
idea of using the same code as both an assembler and a disassembler. The idea
actually seems to work. :-)
