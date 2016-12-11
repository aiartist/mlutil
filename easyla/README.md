# `easyla`

Simplified monomorphic bindings to [`hmatrix`][hmatrix] and various linear algebra helpers

# Motivation

`hmatrix` is great. However, the fact that everything is polymorphic makes it more difficult to use than is necessary if all you ever use is `Double`. So, this little package wraps up `hmatrix` (and bits of [`vector`][vector]). Here are the design goals:

* Export simplified monomorphic versions of functions
* Specialize to `Double`
* Simplify some common operations by providing helper functions
* Export all the most commonly used bits via a single namespace to enable use of a single `import` for common usages

# Install prerequisites

## Windows

```cmd
stack exec -- pacman -Syu
stack exec -- pacman -Sy mingw64/mingw-w64-x86_64-lapack mingw64/mingw-w64-x86_64-openblas
```

## Ubuntu

```bash
sudo apt-get install libblas-dev liblapack-dev
```

## Centos

```bash
sudo yum install blas-devel lapack-devel
```

# Build

```bash
stack build
```

# Setting up runtime environment

On Windows, you'll need to run `script\env.cmd` (Windows command prompt) or `script\env.ps1` (PowerShell) to set up the `PATH` environment variable so that various libraries (e.g. ICU) can be located at runtime:

```cmd
script\env.cmd
```

or

```ps
script\env.ps1
```

## Licence

[Licensed under the MIT License][licence]

[hmatrix]: https://hackage.haskell.org/package/hmatrix
[licence]: LICENSE
[vector]: https://hackage.haskell.org/package/vector
