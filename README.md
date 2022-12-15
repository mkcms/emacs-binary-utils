# emacs-binary-utils #

This repository contains a bunch of Emacs packages for working with binary
files:

- [`objdump.el`](#objdumpel---a-library-for-working-with-objdump-utility)

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
