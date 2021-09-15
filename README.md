# THIS REPOSITORY IS ARCHIVED

### You can now use stdlib with fpm using the upstream [standard library (stdlib)](https://github.com/fortran-lang/stdlib) repository.
See [here](https://github.com/fortran-lang/stdlib#build-with-fortran-langfpm) for instructions.


---

# stdlib-fpm

__An auto-generated mirror of the fortran-lang [standard library (stdlib)](https://github.com/fortran-lang/stdlib) structured as an [*fpm*](https://github.com/fortran-lang/fpm) package (experimental)__

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

## Release notes

- Generic procedures have a __maximum array rank of 4__

- The [legacy Haskell implementation](https://github.com/fortran-lang/fpm-haskell) of `fpm` is __not supported__

- The following tests from the upstream repository are not included here:

  - `test_always_fail.f90`
  - `test_always_skip.f90`
  - `stats/test_mean_f03.f90`

## More information

- See <https://stdlib.fortran-lang.org/> for API documentation

- See <https://github.com/fortran-lang/stdlib> for reporting bugs and contributing to *stdlib*

- See <https://github.com/fortran-lang/fpm> for more information on using *fpm*

## How to update this mirror from upstream

1. Update [`stdlib-revision`](./stdlib-revision) with the updated git commit from <https://github.com/fortran-lang/stdlib>

2. (Optionally) update the [`generate-package.sh`](./generate-package.sh) script to account for any changes in the upstream library
   - This script is responsible for ensuring that the resulting package is *fpm* compatible
   - Update the *release notes* in this `README.md` to account for any changes in `generate-package.sh`
   
3. Create a PR containing __only the modified `std-revision`, `generate-package.sh` and `README.md`__ files (if changed)

4. GOTO step 2 if the CI checks fail, otherwise GOTO step 5

5. If the CI checks pass, then the PR can be reviewed and merged
