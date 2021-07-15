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
