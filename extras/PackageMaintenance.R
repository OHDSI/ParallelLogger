# @file PackageMaintentance.R
#
# Copyright 2020 Observational Health Data Sciences and Informatics
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
OhdsiRTools::formatRFolder("R")
OhdsiRTools::checkUsagePackage("ParallelLogger")
OhdsiRTools::updateCopyrightYearFolder()
devtools::spell_check()

# Create manual and vignette ------------------------------------------
unlink("extras/ParallelLogger.pdf")
shell("R CMD Rd2pdf ./ --output=extras/ParallelLogger.pdf")

rmarkdown::render("vignettes/Logging.Rmd",
                  output_file = "../inst/doc/Logging.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE,
                                          number_sections = TRUE))
unlink("inst/doc/Logging.tex")

rmarkdown::render("vignettes/Parallel.Rmd",
                  output_file = "../inst/doc/Parallel.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE,
                                          number_sections = TRUE))
unlink("inst/doc/Parallel.tex")

pkgdown::build_site()
OhdsiRTools::fixHadesLogo()

# Release package -----------------------------------------------------
devtools::check_win_devel()

devtools::check_rhub()

devtools::release()
