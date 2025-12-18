## Test the MsBackendMongoDb backend
library(testthat)
library(MsBackendMongoDb)
library(mongolite)
library(Spectra)
library(msdata)

#' [TODO] we can run the unit tests only if a mongodb server is running on the
#' system. We should check how to best tackle that. Maybe looking at the unit
#' tests of the mongolite package?

test_check("MsBackendMongoDb")

#' TODO @jo
#'
#' - [ ] @dbcon: use class union instead of ANY
#'
#' - [ ] continue unit test check from [ on.
