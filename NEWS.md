ParallelLogger 1.1.2
=====================

Bugfixes:

1. Fixing error when ggplot2 (v3.3.0) throws a warning.

ParallelLogger 1.1.1
=====================

Bugfixes:

1. Call to .Deprecated() no longer causes silent crash.

ParallelLogger 1.1.0
=====================

Changes:

1. Added e-mail appender and layout.


ParallelLogger 1.0.1
=====================

Changes:

1. When the folder containing the log file is deleted while logging, a warning is thrown (instead of an error), and the file appender is automatically deleted.

2. The Shiny app now ignores malformed lines in the log file (instead of throwing an error).


ParallelLogger 1.0.0
=====================

Changes: initial submission to CRAN
