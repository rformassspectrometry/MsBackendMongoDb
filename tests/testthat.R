## Test the MsBackendMongoDb backend
library(testthat)
library(MsBackendMongoDb)
library(mongolite)
library(Spectra)

#' [TODO] we can run the unit tests only if a mongodb server is running on the
#' system. We should check how to best tackle that. Maybe looking at the unit
#' tests of the mongolite package?

test_dbcon <- function() {
  db_name <- "test_spectra_db"
  list(
      ms_spectrum_coll = mongo(collection = "ms_spectrum_coll",
                               db = db_name,
                               url = "mongodb://ruser:weak_pass@127.0.0.1"),

      ms_peaks_coll = mongo(collection = "ms_peaks_coll",
                            db = db_name,
                            url = "mongodb://ruser:weak_pass@127.0.0.1")
  )
}

clear_db <- function(dbcon) {
  dbcon$ms_spectrum_coll$drop()
  dbcon$ms_peaks_coll$drop()
}

test_check("MsBackendMongoDb")
