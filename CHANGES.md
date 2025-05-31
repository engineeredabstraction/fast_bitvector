## Development
- Added new API:
    - `union` to complete set operations;
    - `extend` allocates a bigger copy of a bitvector, `extend_inplace` - same, but may reuse the internal byte array;
    - `mapi` as simple `map` requires external mutable state to do something more complex;
    - `to/of_seq` to connect to other containers;
    - `iter` and `iteri`, `foldi` implementations.
- Added documentation to most of the public API.

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
