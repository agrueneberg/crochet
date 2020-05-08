# crochet 2.2.0.9000

* Use `inherits(., *)` instead of `class(.) == *` (R4 compat).


# crochet 2.2.0

* Move test suite to `inst/test-suite` so that it can be used without
  `--install-tests`.
* Move example in README to `StringMatrix` vignette.


# crochet 2.1.0

* Export `convertIndex` function.


# crochet 2.0.1

* Fix incorrect index type determination when additional arguments were passed
  to `extract()` and `replace()`.
* Fix missing `...` pass-through for single indices in `extract()`.
* Throw an error if index type is not supported.


# crochet 2.0.0

* The `length()` method must now be implemented in addition to `dim()` and
  `dimnames()` for a matrix to be supported by crochet. One way to implement
  `length()` could be `length.TYPE = function(x) prod(dim(x))`.
* Add parameter `allowDoubles` to support huge matrices by not converting
  indices of type double to integers if the operation would overflow. Defaults
  to `FALSE` to reduce the burden of implementing backends in statically typed
  languages.
* Add `ktoij()` and `ijtok()` helper functions.


# crochet 1.0.0

Initial release.
