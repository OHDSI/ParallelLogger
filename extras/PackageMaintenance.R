# @file PackageMaintentance.R
#
# Copyright 2022 Observational Health Data Sciences and Informatics
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
install.packages("BiocManager")
BiocManager::install("ProteoDisco", update = FALSE, dependencies = c("Depends", "Imports", "LinkingTo", "Suggests"))
# BiocManager::install("BiocStyle", update = FALSE)
# BiocManager::install("TxDb.Hsapiens.UCSC.hg19.knownGene", update = FALSE)
# BiocManager::install("BSgenome.Hsapiens.UCSC.hg19", update = FALSE)
sourceFile <- tempfile(fileext = "tar.gz")
download.file(url = "http://www.bioconductor.org/packages/release/bioc/src/contrib/ProteoDisco_1.2.0.tar.gz", destfile = sourceFile)
sourceFolder <- tempfile()
untar(tarfile = sourceFile, exdir  = sourceFolder)
rcmdcheck::rcmdcheck(path = file.path(sourceFolder, "ProteoDisco"), args = c("--no-manual", "--no-multiarch"), error_on = "warning")

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
