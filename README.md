# Morsmall -- A concise AST for POSIX shell

This library is a small wrapper around [Morbig]. Morbig provides a parser and a
CST representation of POSIX shell scripts which may contain too much information
for certain uses.

[morbig]: https://github.com/colis-anr/morbig/

Morsmall provides a converter from Morbig's CST to an AST that aims to be easely
understandable and manipulable. It also provides a printer from that AST to
Shell.

This program is a free software licensed under the GNU General Public License,
version 3. Please refer to the [LICENSE](./LICENSE).

## Downloading, building and installing

### Download

#### Clone the repository

```
git clone https://github.com/colis-anr/morsmall
```

#### Download the latest release

cf [releases](https://github.com/colis-anr/morsmall/releases)

### Install dependencies

Morsmall depends on the following software:

- [OCaml](https://ocaml.org/) ≥ 4.04.0
- [Morbig](https://github.com/colis-anr/morbig/)
- [ppx_deriving](https://github.com/ocaml-ppx/ppx_deriving) ≥ 4.2.0
- [Dune](https://github.com/ocaml/dune) with ocaml-migrate-parsetree
- [OPAM](http://opam.ocaml.org/)

### Build

```
make
```

### Run tests

```
make tests
```

### Install

```
make install
```

This target recognises the environment variables `PREFIX` and `LIBDIR`.

### Uninstall

```
make uninstall
```

This target recognises the environment variables `PREFIX` and `LIBDIR`.
