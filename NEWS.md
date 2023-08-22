ParallelLogger 3.3.0
====================

Changes

1. Switching from `mailR` to `sendmailR` for sending e-mails, which has fewer dependencies.

Bugfixes

1. Fixing test for whether packages exist, which was causing errors on some Linux versions.



ParallelLogger 3.2.0
====================

Changes

1. When calling any log function (e.g. `logInfo()`) before any loggers are registered, `ParallelLogger` no longer creates a default console logger, but just writes the output to console (except for `logTrace()` and `logDebug()`). Global handlers will not be registered until a logger is registered explicitly (using `registerLogger()`). As a consequence, any warnings about calling global handlers with callers on the stack (when in a try...catch) will not occur until explicitly registering a logger.


ParallelLogger 3.1.0
====================

Changes

1. Truncating long argument values when a thread throws an error in `clusterApply()` to avoid clutter.

2. Showing warning about being inside a `tryCatch` or `withCallingHandlers` block only once per R session.

3. The `matchInList()` function now looks for equivalence, not exact match (e.g. a numeric and integer can still be considered the same).

Bugfixes

1. Fixed issue when loading a JSON object where the first item in a list is a data frame.


ParallelLogger 3.0.1
====================

Changes

1. Throw more informative error message if file not found in `loadSettingsFromJson()`.

Bugfixes

1. Fixing extraction of parameter documentation in `createArgFunction()` on R >= 4.2.0.

2. Fixes error when saving and loading tibbles to JSON.


ParallelLogger 3.0.0
====================

Changes

1. Using new `globalCallingHandlers` functionality introduced in R 4.0.0 to capture warnings, errors and messages. This should be more stable than the legacy code. 

2. All `message()` calls are now logged automatically (at the 'INFO' level)


ParallelLogger 2.0.2
====================

Changes

1. Log file viewer uses chunking to allow viewing of huge log files.

Bugfixes

1. Disabling capturing of errors and warnings during R Check and unit testing so R Check fails if an error occurs during testing.


ParallelLogger 2.0.1
====================

Changes

1. Changed dependency from XML to xml2 to avoid trouble installing dependencies.

2. Allow override of name of default loggers.

Bugfixes

1. Correct function attribution in log when using rlang `warn` or `abort`.


ParallelLogger 2.0.0
====================

Changes

1. Simplifying argument functions.

2. Dropping support for ff in favor of Andromeda.

Bugfixes

1. Fixing missing spaces in argument function documentation.

2. More gracefully handling unnamed lists when saving and loading from JSON.


ParallelLogger 1.2.0
====================

Changes

1. Errors in a cluster are now also logged by the remote thread. This allows for example for the stack trace to be captured. The e-mail logger will only be triggered by events in the main thread to avoid spam.

2. Added the layoutErrorReport layout.

3. Added overwrite and expirationTime arguments to createFileAppender.

4. Added the addDefaultErrorReportLogger function. 

5. Improved stack trace.

6. Changed names of default loggers to DEFAULT_FILE_LOGGER, DEFAULT_EMAIL_LOGGER, and DEFAULT_ERRORREPORT_LOGGER.

7. Added 'silent' argument to unregisterLogger function.

Bugfixes

1. Now walking up the stack to try to evaluate warning message. For example prevents 'wrn not found' errors when using tidyVerse packages.


ParallelLogger 1.1.2
====================

Bugfixes

1. Fixing error when ggplot2 (v3.3.0) throws a warning.


ParallelLogger 1.1.1
====================

Bugfixes

1. Call to .Deprecated() no longer causes silent crash.


ParallelLogger 1.1.0
====================

Changes

1. Added e-mail appender and layout.


ParallelLogger 1.0.1
====================

Changes

1. When the folder containing the log file is deleted while logging, a warning is thrown (instead of an error), and the file appender is automatically deleted.

2. The Shiny app now ignores malformed lines in the log file (instead of throwing an error).


ParallelLogger 1.0.0
====================

initial submission to CRAN
