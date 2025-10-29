# @file PackageMaintentance.R
#
# Copyright 2025 Observational Health Data Sciences and Informatics
#
# This file is part of ParallelLogger
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Format and check code -----------------------------------------------
styler::style_pkg()
OhdsiRTools::checkUsagePackage("ParallelLogger")
OhdsiRTools::updateCopyrightYearFolder()
devtools::spell_check()

# Test reverse dependencies listed in CRAN ----------------------------
# ProteoDisco:
# Not able to test. Requires installation of >2,000 packages from BioConductor, several of which tend to fail to install.

# IterativeHardThresholding:
# Clone and test in RStudio

# Create manual and vignette ------------------------------------------
unlink("extras/ParallelLogger.pdf")
system("R CMD Rd2pdf ./ --output=extras/ParallelLogger.pdf")

dir.create("inst/doc", showWarnings = FALSE)
rmarkdown::render("vignettes/Logging.Rmd",
                  output_file = "../inst/doc/Logging.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE,
                                          number_sections = TRUE))

rmarkdown::render("vignettes/Parallel.Rmd",
                  output_file = "../inst/doc/Parallel.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE,
                                          number_sections = TRUE))

pkgdown::build_site()
OhdsiRTools::fixHadesLogo()

# Release package -----------------------------------------------------
# pak::pkg_install("r-lib/revdepcheck")
revdepcheck::revdep_check(num_workers = 4) # Checks packages in CRAN only

devtools::check_win_devel()

rhub::rc_submit(platforms = "atlas")

devtools::release()
