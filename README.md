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


Contribute
----------

- Issue Tracker: https://github.com/agrueneberg/crochet/issues
- Source Code: https://github.com/agrueneberg/crochet


Documentation
-------------

Further documentation can be found on [RDocumentation](http://www.rdocumentation.org/packages/crochet).
