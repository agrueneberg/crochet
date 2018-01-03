# crochet 1.0.0.9000

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
