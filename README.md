__Note: The contents of this repository are generated automatically from the [fortran-lang/stdlib](https://github.com/fortran-lang/stdlib) upstream project__

# stdlib-fpm

__An auto-generated mirror of the fortran-lang [standard library (stdlib)](https://github.com/fortran-lang/stdlib) structured as an [*fpm*](https://github.com/fortran-lang/fpm) package__

This repository uses Github actions to pull and pre-process source files from the fortran-lang [stdlib](https://github.com/fortran-lang/stdlib)
project in order to generate a package repository compatible with the [Fortran Package Manager (fpm)](https://github.com/fortran-lang/fpm).

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![fpm test](https://github.com/LKedward/stdlib-fpm/workflows/Generate%20package/badge.svg)](https://github.com/LKedward/stdlib-fpm/actions)

## Usage

To use *stdlib* within your [*fpm*](https://github.com/fortran-lang/fpm) project, add the following to your project manifest (`fpm.toml`):


```toml
[dependencies]
stdlib = { git = "https://github.com/LKedward/stdlib-fpm.git" }
```

## Release notes (important)

- The library __cannot be built in *release* mode__  (`fpm build --release`)

- Generic procedures have a __maximum array rank of 4__

- The following tests from the upstream repository are not included here:

  - `test_always_fail.f90`
  - `test_always_skip.f90`
  - `stats/test_mean_f03.f90`

