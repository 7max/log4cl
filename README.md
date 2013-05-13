### Changes since last version

#### Package changes

`LOG4CL-IMPL` package had been renamed to `LOG4CL`, retaining `LOG4CL-IMPL` as
the nickname. Old `LOG4CL` package now is now simply `LOG`

#### Logging macros changes

`(log:debug)` and friends now figure out that you want to do same
processing as `(log:expr)` was doing before, based on the absense of
format control string with a tilde as first argument.

So while previously `(log:info (foo a b) c d)` was assumed to be trying to
log a message to a logger returned by function call `(foo a b)`, now
its exactly the same as `(log:sexp-info (foo a b) c d)`

You can still use format string, and the constant logger names as first argument
like so:

 `(log:info "pi is ~d" pi)`
 `(log:info :category.name (+ 1 2))`


It is still possibel to log the message to a logger determined at runtime
by using `(log:debug :logger <form-returning-a-logger> ...`) syntax.

You can also use a dynamic format control string by using
`:format-control <form returning a string>` argument.

The `(log:sexp-trace)`, `(log:sexp-info)` and such macros are depreciated.
`(log:expr ...)` remains an alias to `(log:debug)` with a special feature
that you can customize what log level it would log into instead of debug.

#### Category naming changes

  Default category separator is now dot. If package has a nickname, then
  the package shortest nickname will still be used by default, which can
  be overwritted by new `(package-options)` mechanism

  When package name, and source block information are used to automatically
  form a logger category name, the split point between them is remembered,
  and patters can print package part, and rest of the category part seprately.

  Each logger now automatically gets source file that it was
  instantiated in remembered, and can be printed in a pattern
  layout. Source files references are special type of a logger, then
  are never logged into themselfs but if a level is set for them,
  file's level, takes precedence over the package level.

#### Pattern layout had been extended

  - Control of pretty printing
  - Ability to bind original **package**
  - Can print source file that log statement had came from

#### LOG:CONFIG function had been extended 

   In now has many pattern layout selection flags, to modify build-in layouts.

   For example:

#### Log4CL now has Slime integration!

   You need to load `log4slime` system, and type (log4slime:install)
   to setup Emacs side of things.

   See LOG4SLIME.md file for documentation

