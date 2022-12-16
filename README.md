# emacs-binary-utils #

This repository contains a bunch of Emacs packages for working with binary
files:

- [`objdump.el`](#objdumpel---a-library-for-working-with-objdump-utility)
- [`asm-data.el`](#asm-datael---conversion-between-data-representations-in-asm-buffers)
- [`asm-jump.el`](#asm-jumpel---buttons-for-jumps-in-asm-mode)

## `objdump.el` - A library for working with `objdump` utility ##

This library provides functions for working with the objdump program.  It can
read the symbol table, disassemble code, extract contents of sections,
mangle/demangle names.

The API functions are:

- `objdump-file-dynamic-p`

  Check if file is dynamic.

- `objdump-read-symtab`

  Read the symbol table from a file.

- `objdump-mangle`, `objdump-demangle`, `objdump-symbol-mangled-p`,
`objdump-symbol-demangled-p`

  Mangle and demangle symbols; check if they are mangled.

- `objdump-disassemble`

  Disassemble a portion of a code section.

- `objdump-raw`

  Get the raw contents of a section.

## `asm-data.el` - Conversion between data representations in ASM buffers ##

This package lets you change the way data is represented in ASM buffers.  For
example, when the buffer contains:

```asm
.4byte 1819043176
.byte 111
.byte 0
```

calling `asm-data-convert` and selecting the ".asciz" directive changes that
to:

```asm
.asciz "hello"
```

This representation can again be changed, e.g. to 2byte (here called with a
prefix argument for hexadecimal number representation):

```asm
.2byte 0x6568
.2byte 0x6c6c
.2byte 0x006f
```

It can convert data to .ascii, .asciz, .byte, .2byte, .4byte, .8byte, .octa
(16-byte), .single and .zero GAS directives.

Integers can be converted to unsigned/signed decimal/hex/binary
representation.

Floating-point directive .single assumes 32-bit floats.

The variable `asm-data-endianness` controls the type of numbers<->bytes
conversion.

## `asm-jump.el` - Buttons for jumps in ASM mode ##

This package provides the function `asm-jump-process-buffer` which scans the
current buffer for ASM jump statements and makes buttons for them.

The action for buttons created by this function will search the current buffer
for the referenced label and move the point there.

Another command, `asm-jump-reverse` finds the first jump statement to the
current label.

## License ##

```
Copyright (C) 2022 Michał Krzywkowski

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
```
<!-- Local Variables: -->
<!-- coding: utf-8 -->
<!-- fill-column: 79 -->
<!-- End: -->