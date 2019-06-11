# crochet

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/crochet)](https://CRAN.R-project.org/package=crochet)
[![Rdoc](http://www.rdocumentation.org/badges/version/crochet)](http://www.rdocumentation.org/packages/crochet)
[![Travis-CI Build Status](https://travis-ci.org/agrueneberg/crochet.svg?branch=master)](https://travis-ci.org/agrueneberg/crochet)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/5osfclaxqxhq04r2?svg=true)](https://ci.appveyor.com/project/agrueneberg/crochet)
[![Coverage status](https://codecov.io/gh/agrueneberg/crochet/branch/master/graph/badge.svg)](https://codecov.io/github/agrueneberg/crochet?branch=master)

crochet is an R package that provides functions to help implement the extraction / subsetting / indexing function `[` and replacement function `[<-` of custom matrix-like types (based on S3, S4, etc.), modeled as closely to the base `matrix` class as possible (with tests to prove it).


Example
-------

An example of a custom type that implements the `extract` and `replace` functions can be found in the `StringMatrix` vignette.


Installation
------------

Install the stable version from CRAN:

```R
install.packages("crochet")
```

Alternatively, install the development version from GitHub:

```R
# install.packages("devtools")
devtools::install_github("agrueneberg/crochet")
```


Discussion
----------

R used to export functions for index conversion such as `makeSubscript`, `vectorSubscript`, and `arraySubscript` (see [src/main/subscript.c](https://svn.r-project.org/R/trunk/src/main/subscript.c)) to package developers until R 2.3.1. These exports were removed in R 2.4.0 as part of a cleanup (https://github.com/wch/r-source/commit/7e3ce2f08807c005f930c0b36b545b10c7e9b391). `arraySubscript` was later re-added as some packages such as arules and cba still rely on it (https://github.com/wch/r-source/commit/e7f0603fe69fc972466df01d6e8d4f8c207a757b). I still need to investigate, whether `arraySubscript` would be useful for this package.


Contribute
----------

- Issue Tracker: https://github.com/agrueneberg/crochet/issues
- Source Code: https://github.com/agrueneberg/crochet


Documentation
-------------

Further documentation can be found on [RDocumentation](http://www.rdocumentation.org/packages/crochet).
