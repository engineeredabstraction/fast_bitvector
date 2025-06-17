
## Development
- More efficient create
- Compatibility with multiple versions of ocaml_intrinsics_kernel
- Add QCheck tests

## 0.1.0.1
- Simplify append, add test

## 0.1.0
- New, more clear, sexp representation
- Better error messages for safe functions
- Fix big-endian architecture compatibility
- Remove returned bitvector in ops, user is now required to track it themselves
- Use `~dst` instead of `~result`
- Add `Allocate` variant of all ops in a submodule
- Allow equality check between different-length bitvectors


## 0.0.5
- 32-bit compatible
- Properly fix append
- Add more tests for logical operations

## 0.0.4.2
- Additional opam fixes

## 0.0.4.1
- Update opam files to build with earlier ocaml

## 0.0.4
- compatibility with ocaml 4.14
- use `~len` instead of `~length`
- fix append and add tests

## 0.0.3.1
- use ocaml_intrinsics_kernel

## 0.0.3
- add `set_all` and `clear_all`
- add basic testing
- add endianness in sexp conversion
- add `is_empty` and `is_full`
- add set operations
- add `create_full`
- fix `set_to`

## 0.0.2
- Initial release
