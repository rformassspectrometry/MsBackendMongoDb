## Test the MsBackendMongoDb backend
library(testthat)
library(MsBackendMongoDb)
library(mongolite)
library(Spectra)
library(MsDataHub)

dbd <- file.path(tempdir(), "test_mongo")
start_mongodb(dbpath = dbd)

test_dbcon <- function() {
  db_name <- "test_spectra_db"
  list(
      ms_spectrum_coll = mongo(collection = "ms_spectrum_coll",
                               db = db_name,
                               url = "mongodb://127.0.0.1"),
      ms_peaks_coll = mongo(collection = "ms_peaks_coll",
                            db = db_name,
                            url = "mongodb://127.0.0.1")
  )
}

clear_db <- function(dbcon) {
  dbcon$ms_spectrum_coll$drop()
  dbcon$ms_peaks_coll$drop()
}

test_check("MsBackendMongoDb")

## RUN THE TESTS FROM SPECTRA
test_suite <- system.file("test_backends", "test_MsBackend",
                          package = "Spectra")

fls <- c(X20171016_POOL_POS_1_105.134.mzML(),
         X20171016_POOL_POS_3_105.134.mzML())
sp <- Spectra(fls)
mcon <- connectMsBackendMongoDb(
    db = "test_spectra_db",
    url = "mongodb://127.0.0.1",
    clean = TRUE)

be <- setBackend(sp[1:200], backend = MsBackendMongoDb(), dbcon = mcon)@backend
test_dir(test_suite, stop_on_failure = TRUE)

stop_mongodb(dbpath = dbd)
unlink(dbd, recursive = TRUE)
