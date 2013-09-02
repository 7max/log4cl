## 1.1.2 - Bugfixes 

* Re-initialize any appenders that remember the resolved stream in INIT-HOOK on
  SBCL; which fixes a crash if SBCL dump is ressurected without doing
  (CLEAR-LOGGING-CONFIGURATION). Patch by Jan Moringen.
  
* Add (log:config ...adding an appender... :filter LEVEL) option, which causes
  appender to drop messages less serious then LEVEL. This allows one to
  configure per-level appenders, for example error.log and debug.log. Patch by
  https://github.com/naryl

## 1.1.1 - Log4Slime 

* LOG:CONFIG :thread or :ndc argument can be followed by two numbers,
  which will be used as MIN/MAX width fields, this helps if you want
  to align output from multiple threads.

* Pattern layout can use {pretty}{100} to set **PRINT-RIGHT-MARGIN** in
  addition to forcing pretty printing.

* (LOG:CONFIG (LOG:CATEGORY) ...) from top level now configures the package logger 
  instea of package.<sourcefile> logger. 

* :console appender changed back to use **DEBUG-IO** instead of 
  **TERMINAL-IO**, because on LispWorks **TERMINAL-IO** goes to actual
  terminal.

* Removed forgotten Log4Slime dependency on org-mode in faces definition.

* Default logging configuration is now :SANE2

* Fix LOG:CONFIG :backup option not accepting NIL argument.

* Log4SLime informational message about logger level changes was giving error
  if category name contained % character.

* %t and %h pattern layout (displaying thread and host name) were crashing
  if corresponding attribute was actually NIL (apparently its possible to 
  do create SBCL thread with NIL name)

* (LOG:CONFIG :daily ...) now accepts pathnames again

## 1.1.0 - Log4Slime 

* Log4CL now has Slime/Emacs integration, which is available in a
  system :log4slime. It colorizes the log output, and provides
  ability to change log levels from Emacs.

* Default category separator changed to dot.

* The old CLOS method of setting option is depreciated in favor of new
  (LOG:PACKAGE-OPTIONS) macro

* LOG package namespace was cleaned up, and its now an actual
  package named LOG. The LOG4CL package is now nickname for
  LOG4CL-IMPL

* Loggers now retain information about source file and package they
  were instantiated from, with corresponding PATTERN-LAYOUT formats
  to print them

  As a consequence to pass a dynamically determined logger object to a
  log statement, now requires (log:debug :logger
  (form-that-returs-a-logger))

* (log:info a b c) and such now automatiaclly do (log:expr) like processing
  if they don't detect a format control string as first argument.

* Automatic category naming implemented on Clozure Common Lisp (CCL),
  working exactly like SBCL one.

* Pattern layout extended with control of pretty printing and other enchencements

* Much more robust error handling in standard appenders, better reporting
  of errors.

* It is possible to set a log level for the source file, it takes precedence
  over the package category.

* On SBCL Log4CL hierarchy watcher thread that is used for flushing, a
  is automatically terminated on exit, and before SAVE-LISP-AND-DIE.

* Added API to flush all appenders, LOG4CL:FLUSH-ALL-APPENDERS, which
  is automatically called on SBCL from exit hooks. 

* (LOG:CONFIG) now has many new pattern layout related options
  :pretty, :nopretty, :file, :file2, :nofile, :time, :notime, :package
  :nopackage, :ndc, :thread

  These options change the built-in pattern used for :sane, :console
  and other options that add new appender

* (LOG:CONFIG) with only the pattern options, will change existing console
  appender instead of adding a new one.

* (LOG:CONFIG) appender displays a number next to each appender, and allows
  removing of appenders via :REMOVE <n> option

## 1.0.0
 * (Bugfix): Fix SB-C package lock error when compiling on new SBCL.
   Created stable branch stable version

## 0.9.5

* (Bugfix) DAILY-FILE-APPENDER was only rolling over the file if
  rollover time passed in between two log mesjsages. Therefore a
  short-lived command line utility configured with `(log:config :daily
  "log.txt")` would never roll over the log.txt to
  log.txt-yyyy-mm-dd.txt, unless it happen to run exactly at
  midnight. This had now been corrected, and pre-existing log file
  will be rolled over based on its modification time.

## 0.9.3

* (Feature): Added ability to quick save/restore of named logging
  configurations, with the list of 30 most recently used
  configurations saved in a file in user home directory. See next to
  last section in README.md QuickStart quide describing new
  functionality

* (Bugfix): Change log4cl package to forward functions and macros from the
  log4cl-impl package by setf'ing fdefinition or macro-function. This allows
  Slime M-. key to correctly locate sources of "nicknamed" functions or macros.

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
