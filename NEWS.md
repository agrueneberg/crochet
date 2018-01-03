# crochet 1.0.0.9000

* `length()` needs to be implemented.
* Add parameter `allowDoubles` to support huge matrices by not converting
  indices of type double to integers if the operation would overflow. Defaults
  to `FALSE` to reduce the burden of implementing backends in statically typed
  languages.
* Add `ktoij()` and `ijtok()` helper functions.


# crochet 1.0.0

Initial release.
