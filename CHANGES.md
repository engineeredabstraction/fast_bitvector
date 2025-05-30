## Development
- Added new API:
    - `union` to complete set operations;
    - `extend` in order to append more bit fields without intermediate allocations;
    - `mapi` as just `map` seems to be useless;
    - `to/of_iter/seq` to connect to other containers.
- Added public documentation to public API.

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
