# emacs-binary-utils #

This repository contains a bunch of Emacs packages for working with binary
files:

- [`binfile.el`](#binfileel---disassemble-binary-files)
- [`objdump.el`](#objdumpel---a-library-for-working-with-objdump-utility)
- [`asm-data.el`](#asm-datael---conversion-between-data-representations-in-asm-buffers)
- [`asm-jump.el`](#asm-jumpel---buttons-for-jumps-in-asm-mode)
- [`asm2src.el`](#asm2srcel---asm-to-source-file-overlays)
- [`compiled-file.el`](#compiled-fileel---getset-the-compiled-file-for-current-source-file)
- [`compdb.el`](#compdbel---work-with-compilation-databases)

## `binfile.el` - Disassemble binary files ##

An extendable package for examining binary files.  It can disassemble many
types of ELF files and postprocess the results to be more easily readable
(e.g. it can parse relocations reported by objdump and output them intermixed
with code).

The main command is `binfile-disassemble`, which prompts for a function name
(by default, the function at point) and, if it can't be guessed, a binary file.
The binary file is by default provided by `compiled-file.el` library.

Using `compdb-output-filename` as `compiled-file-function` allows automatically
finding binary (.o) files for current buffer from `compile_commands.json` file.

The other commands are:

- `binfile-insert-data`

  Insert data (a symbol, a section, or an address range) from a binary file
  into the current disassembly buffer.  It can be used to examine .data,
  .rodata sections etc.

- `binfile-diff`

  Display a diff buffer for examining a difference between two disassembled
  binary files.

- `binfile-symbol-info`

  Prompt for a symbol and a binary filename, and display information about that
  symbol.

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

## `asm2src.el` - ASM to source file overlays ##

This package allows you to jump from ASM buffer to a source file and vice
versa.

`asm2src-process-buffer` is the main function which scans the current ASM
buffer for file mappings, it must be called once before further usage.

When using `binfile` and the rest of the packages here, you should add:

    (setq objdump-disassembly-extra-args '("-l"))
    (add-hook 'binfile-disassembly-hook #'asm2src-process-buffer)

To your init file.  The first line makes objdump output source file mappings
when dumping disassembly, and the second makes sure we can parse and use that
in Emacs.

`asm2src-jump` allows you to jump to the source buffer from the preprocessed
ASM buffer.  It sets a transient keymap, `asm2src-jump-keymap` for the duration
of the command: `C-c C-c` or `RET` goes to the source file, `p` goes to the
previous mapped location, `n` goes to the next.

`asm2src-jump-to-asm` is the inverse, it should be invoked in a source file and
will go to the first ASM buffer containing the source line.

`asm2src-add-mapping` can be used to add a custom ASM<->source directory mapping.

## `compiled-file.el` - Get/set the compiled file for current source file ##

This helper package defines a variable `compiled-file` which can be
set to the path to the object file for current source file.

It can also automatically find an object file if the variable
`compiled-file-directory` is set to the build directory.

Having the path to a binary file of the current source is used by some
other packages to automatically switch from source to disassembly and
vice-versa.

## `compdb.el` - Work with compilation databases ##

This package provides utilities for working with compilation
databases.

The main API functions are:

- `compdb-path`

Locate database for FILENAME by scanning directory tree upwards.

- `compdb`

Get parsed compilation database for a project. The return value is
either nil if the database does not exist, or a hash table.

- `compdb-compile`

Compile the file the current buffer is visiting.

- `compdb-switch`

Switch the current compilation database. This works by replacing the
current compilation database with a symbolic link to the new database.

## License ##

```
Copyright (C) 2022-2024 Michał Krzywkowski

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
