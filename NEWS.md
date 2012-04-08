## 0.9.1

* (Doc): Added examples directory which shows how to customize
  the logging category per-package, and include the file name
  as part of logging category.

* (New Feature): Pattern layout %c formatter had been extended with
  %c{FROM,COUNT} format, which allows printing of Nth category name
  in the hierarchy. This is especially useful if one uses filename
  as part of of the category name, and allows user to configure
  pattern as for example `(file.lisp) <function>`

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
  to use `:immediate-flush t` and therefore will no longer start
  the background thread.

  Correspondinly an `:immediate-flush` option was added to
  `(log:config)` which will cause appenders it creates to
  have `:immediate-flush t` property.

  It is recommended that user reconfigures the logging system from
  initial configuration without the `:immediate-flush` option, as
  stream appenders flushed by background thread have much better
  performance

* (Bugfix): Fixed running under LispWorks.

* (Doc): Assorted docstrings were updated to reflect reality.
