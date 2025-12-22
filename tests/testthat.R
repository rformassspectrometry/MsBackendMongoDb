## Test the MsBackendMongoDb backend
library(testthat)
library(MsBackendMongoDb)
library(mongolite)
library(Spectra)

#' [TODO] we can run the unit tests only if a mongodb server is running on the
#' system. We should check how to best tackle that. Maybe looking at the unit
#' tests of the mongolite package?

test_check("MsBackendMongoDb")
