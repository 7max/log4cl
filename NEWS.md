## 0.9.2
* (Bugfix) When logger is specified at runtime, check that it has the correct type

* Do not reset log level to info with :sane when no log level was
  specified. I had found that `(log:config :sane)` resetting log level
  back to info, when it used to be debug seems surprising. If you want
  old behavior, use `(log:config :sane :i)`

* (Feature): Added %& pattern format, which does FRESH-LINE, that is
  outputs a newline only if output position is not already at the
  beginning of a new line

* On some complicated functions inside of LOOP inside of LABELS, the
  automatic logger naming was duplicating the function name.

## 0.9.1

* (Doc): Added examples directory which shows how to customize the
  logging category per-package, and include the file name as part of
  logging category.

* (New Feature): Pattern layout %c formatter had been extended with
  %c{FROM,COUNT} format, which allows printing of Nth category name in
  the hierarchy. This is especially useful if one uses filename as
  part of of the category name, and allows user to configure pattern
  as for example `(file.lisp) <function>`

* (New Feature): the `(log:expr)` now uses pretty printing conditional
  newline to separate printed values, which results in much better
  formatted output when a large structures are printed. In addition
  `(nameing-option)` generic function protocol had been extended to
  allow specifying custom formatting for `(log:expr)` (ie different
  separator between var=value then default equal sign, or different
  separator between entire experssions).

* (Bugfix): %z timezone string had wrong sign, ie +0400 instead of -0400
  and did not take daylight savings into account

* (Bugfix): All tests now correctly pass with inverted readtable in
  effect

* (Bugfix) The console appender in the initial configuration changed
  to use `:immediate-flush t` and therefore will no longer start the
  background thread. Correspondinly an `:immediate-flush` option was
  added to `(log:config)` which will cause appenders it creates to
  have `:immediate-flush t` property. It's recommended that user
  reconfigures the logging system from initial configuration without
  the `:immediate-flush` option, as stream appenders flushed by
  background thread have much better performance

* (Bugfix): Fixed running under LispWorks.

* (Doc): Assorted docstrings were updated to reflect reality.
