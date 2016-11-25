# Machine Learning in Action in Haskell

This is a project to translate the code examples in Peter Harrington's [Machine Learning in Action][pbharrin] into Haskell. I don't have a lot of free time so this will take some time.

## The plan

I plan to translate as many of the Python examples into Haskell as possible and to create a package of Haskell support functions (tentatively called [`mlutil`][mlutil]). I'll then contribute them to the [DataHaskell][dh] community if they might be of interest to others.

## Projects

* [`mlutil`][mlutil]
* [Programs from chapter 2 of book: k-nearest neighbours algorithm][ch02knn]
* [Programs from chapter 3 of book: decision trees][ch03decisiontrees]

## Supported platforms

The code has been tested on the following platforms:

* Linux (Centos7 and Ubuntu 14.04)
* OS X 10.10.5
* Windows 7 and Windows 10

## Building and testing the code

Individual subprojects may have their own prerequisites, so please consult their respective `README.md` files for more information.

Once up and running with prerequisites, you can build or test individual projects or all projects using the helper scripts [`build`][buildscript] and [`test`][testscript] in this repo's root directory:

To build all projects:

```bash
cd /path/to/repo/root
./build
```

To build a single project:

```bash
cd /path/to/repo/root/project0
../build
```

The `test` script works in a similar manner. I'll write similar companion scripts for Windows when I get the chance.

## Licence

[Licensed under the MIT License][licence]

[buildscript]: build
[ch02knn]: ch02-knn/README.md
[ch03decisiontrees]: ch03-decision-trees/README.md
[dh]: https://github.com/datahaskell
[licence]: LICENSE
[mlutil]: mlutil/README.md
[pbharrin]: https://github.com/pbharrin/machinelearninginaction
[testscript]: test
