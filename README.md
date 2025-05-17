# emacs-binary-utils #

This repository contains a bunch of Emacs packages for working with binary
files:

- [`binfile.el`](#binfileel---prettify-disassembly)
- [`bdx.el`](#bdxel---frontend-for-bdx)
- [`asm-data.el`](#asm-datael---conversion-between-data-representations-in-asm-buffers)
- [`asm-jump.el`](#asm-jumpel---buttons-for-jumps-in-asm-mode)
- [`asm2src.el`](#asm2srcel---asm-to-source-file-overlays)
- [`untemplatize-cxx.el`](#untemplatize-cxxel---make-c-templates-readable)
- [`compdb.el`](#compdbel---work-with-compilation-databases)

## `binfile.el` - Prettify disassembly ##

This package provides the function `binfile-postprocess-buffer` which
prettifies objdump disassembly in the current buffer and makes it easier to
read and follow by stripping addresses, adding labels for jump targets,
removing useless comments, and some other things.

By default it works on objdump output (and is optimized for x86 architecture)
but you can set `binfile-region-postprocessing-functions`,
`binfile-region-postprocessing-functions-alist` and
`binfile-file-format-function` for your own needs to work with different
disassemblers/architectures.

## `bdx.el` - Frontend for bdx ##

[bdx][bdx] is an indexer for ELF files.

The `bdx.el` package provides an Emacs frontend for the command line tool, and
allows quickly finding and disassembling symbols even in huge repositories.

This package interactively displays a list of matched symbols as the user
types.

The commands defined in it are:

- `bdx-disassemble`

  Read a bdx query from the user with `ivy`, and use `binfile` (or a custom
  function) to disassemble the selected symbol.

- `bdx-show-graph-xdg-open`

  Read two queries from user, START and GOAL, and use `bdx` to generate an
  image of a graph that connects symbols matching START and GOAL, then display
  that image.

It also provides these API functions:

- `bdx-query`

  Read a bdx query from the user, interactively displaying results, and return
  symbol data.

- `bdx-generate-graph`

  Generate graph from two queries, and write it (in DOT format) to the current
  buffer.

- `bdx-generate-graph-image`

  Generate a graph image from two queries, and return it's path.

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

    (setq bdx-disassembler-options
          (concat bdx-disassembler-options " --line-numbers"))
    (add-hook 'binfile-buffer-postprocessing-functions
              #'asm2src-process-buffer)

To your init file.  The first sexp makes objdump output source file mappings
when dumping disassembly, and the second makes sure we can parse and use that
in Emacs.

`asm2src-jump` allows you to jump to the source buffer from the preprocessed
ASM buffer.  It sets a transient keymap, `asm2src-jump-keymap` for the duration
of the command: `C-c C-c` or `RET` goes to the source file, `p` goes to the
previous mapped location, `n` goes to the next.

`asm2src-jump-to-asm` is the inverse, it should be invoked in a source file and
will go to the first ASM buffer containing the source line.

`asm2src-add-mapping` can be used to add a custom ASM<->source directory mapping.

## `untemplatize-cxx.el` - Make C++ templates readable ##

This package provides a function, `untemplatize-cxx-buffer` which makes the
buffer show templated C++ symbols in a more readable way:

    std::_Hashtable<int, std::pair<int const, char const*>,
      std::pmr::polymorphic_allocator<std::pair<int const, char const*>>,
      std::__detail::_Select1st, std::equal_to<int>, std::hash<int>,
      std::__detail::_Mod_range_hashing, std::__detail::_Default_ranged_hash,
        std::__detail::_Prime_rehash_policy,
        std::__detail::_Hashtable_traits<false, false, true>
      >::_M_rehash(unsigned long, std::integral_constant<bool, true>)

Gets turned into:

    std::_Hashtable<...>::_M_rehash(unsigned long, std::integral_constant<...>)

The package uses overlays to achieve this; the buffer contents are never
modified.

After the buffer is processed, these commands can be used:

- `untemplatize-cxx-show`

  Show one level of contents.  In the above example, when point is at the last
  "...", it will show the symbol as:

      _M_rehash_aux(unsigned long, std::integral_constant<bool, true>)

  This command only works when point is at an overlay shown as "...".

- `untemplatize-cxx-hide`

  Hide the overlay at point, if there is one.  This only makes sense after
  calling `untemplatize-cxx-show` at point.

- `untemplatize-cxx-dwim`

  If the overlay at point is showing as "..." then expand it, otherwise, try to
  hide the overlay at point.

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
Copyright (C) 2022-2025 Michał Krzywkowski

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

[bdx]: https://github.com/mkcms/bdx

<!-- Local Variables: -->
<!-- coding: utf-8 -->
<!-- fill-column: 79 -->
<!-- End: -->
