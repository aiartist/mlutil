# Machine Learning in Action in Haskell

This is a project to translate the code examples in Peter Harrington's [Machine Learning in Action][pbharrin] into Haskell. I don't have a lot of free time so this will take some time.

## The plan

I plan to translate as many of the Python examples into Haskell as possible and to create a package of Haskell support functions (tentatively called [`mlutil`][mlutil]). I'll then contribute them to the [DataHaskell][dh] community if they might be of interest to others.

Other goals beyond this include:

* Use stronger typing throughout to take advantage of Haskell's strong static typing
* Refactoring the resulting programs so they are more idiomatic in their use of the Haskell programming language
* Improvements to the efficiency of the implementations to use closer-to-optimal Haskell data structures

The last point includes refactoring much of the code where I have taken lazy approach of translating code to use Haskell lists. In many cases, this has led to code that make use of the `!!` indexing operator on lists which, given its $O\(n\)$ performance characteristics is not good. Much of the code will be refactored to use [vectors][vectorpackage] and [matrices][hmatrixpackage] as appropriate.

## Projects

* [`mlutil`][mlutil]
* [Programs from chapter 2 of book: k-nearest neighbours algorithm][ch02knn]
* [Programs from chapter 3 of book: decision trees][ch03decisiontrees]
* [Programs from chapter 4 of book: na&iuml;ve Bayes][ch04naivebayes]
* [Programs from chapter 5 of book: logistic regression][ch05logisticregression]

## Supported platforms

The code has been tested on the following platforms:

* Linux (Centos7 and Ubuntu 14.04)
* OS X 10.10.5
* Windows 7 and Windows 10

## Building and testing the code

Individual subprojects may have their own prerequisites, so please consult their respective `README.md` files for more information.

Once up and running with prerequisites, you can clean, build or test individual projects or all projects using the helper scripts [`clean`][cleanscript], [`build`][buildscript] and [`test`][testscript] ([`clean.cmd`][cleancmd], [`build.cmd`][buildcmd] and [`test.cmd`][testcmd] respectively on Windows) in this repo's root directory:

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

[buildcmd]: build.cmd
[buildscript]: build
[ch02knn]: ch02-knn/README.md
[ch03decisiontrees]: ch03-decision-trees/README.md
[ch04naivebayes]: ch04-naive-bayes/README.md
[ch05logisticregression]: ch05-logistic-regression/README.md
[cleancmd]: clean.cmd
[cleanscript]: clean
[dh]: https://github.com/datahaskell
[hmatrixpackage]: https://hackage.haskell.org/package/hmatrix
[licence]: LICENSE
[mlutil]: mlutil/README.md
[pbharrin]: https://github.com/pbharrin/machinelearninginaction
[testcmd]: test.cmd
[testscript]: test
[vectorpackage]: https://hackage.haskell.org/package/vector
