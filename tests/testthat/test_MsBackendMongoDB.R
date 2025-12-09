test_dbcon <- function() {
  db_name <- "test_spectra_db"
  list(
    ms_spectrum_coll = mongolite::mongo(collection = "ms_spectrum_coll", 
                                        db = db_name,
                                        url = "mongodb://localhost"),
    
    ms_peaks_coll = mongolite::mongo(collection = "ms_peaks_coll", 
                                     db = db_name,
                                     url = "mongodb://localhost")
  )
}

clear_db <- function(dbcon) {
  dbcon$ms_spectrum_coll$drop()
  dbcon$ms_peaks_coll$drop()
}

test_that("MsBackendMongoDb constructor works (empty)", {
  dbcon <- test_dbcon()
  clear_db(dbcon)
  
  backend <- MsBackendMongoDb(dbcon = NULL)   # empty constructor
  expect_s4_class(backend, "MsBackendMongoDb")
  expect_equal(backend@nspectra, 0L)
  expect_equal(length(backend@spectraIds), 0L)
  
  clear_db(dbcon)
})

test_that("backendInitialize() loads spectraIds and sets internal slots", {
  dbcon <- test_dbcon()
  clear_db(dbcon)
  
  # Insert minimal metadata records
  dbcon$ms_spectrum_coll$insert(
    data.frame(
      spectrum_id_ = c(1L, 2L),
      msLevel = c(1L, 2L)
    )
  )
  
  backend <- new("MsBackendMongoDb")
  
  backend <- backendInitialize(backend, dbcon = dbcon)
  
  expect_s4_class(backend, "MsBackendMongoDb")
  expect_equal(backend@nspectra, 2L)
  expect_equal(backend@spectraIds, c("1", "2"))
  expect_equal(backend@id_map, c("1", "2"))
  expect_true(!is.null(backend@peak_fun))
  expect_true(is.list(backend@.collections))
})


test_that("peaksData() returns list of matrices with correct columns", {
  dbcon <- test_dbcon()
  clear_db(dbcon)
  
  # Insert metadata as character
  dbcon$ms_spectrum_coll$insert(
    data.frame(spectrum_id_ = c("1", "2"), stringsAsFactors = FALSE)
  )
  
  # Insert peaks as character
  dbcon$ms_peaks_coll$insert(
    data.frame(
      spectrum_id_ = c("1", "2"),
      mz = I(list(c(100, 200), c(150, 250))),
      intensity = I(list(c(10, 20), c(15, 25))),
      stringsAsFactors = FALSE
    )
  )
  
  backend <- backendInitialize(new("MsBackendMongoDb"), dbcon = dbcon)
  backend@spectraIds <- c("1", "2")  # use character, not numeric
  backend@id_map     <- backend@spectraIds
  backend@nspectra   <- length(backend@spectraIds)
  backend@.collections <- list(
    ms_spectrum_coll = dbcon$ms_spectrum_coll,
    ms_peaks_coll    = dbcon$ms_peaks_coll
  )
  backend@peak_fun <- .fetch_peaks_data_long_mongo
  
  peaks <- peaksData(backend)
  
  expect_type(peaks, "list")
  expect_equal(length(peaks), 2)
  
  # First spectrum
  expect_true(is.matrix(peaks[[1]]))
  expect_equal(colnames(peaks[[1]]), c("mz", "intensity"))
  expect_equal(peaks[[1]][, "mz"], c(100, 200))
  expect_equal(peaks[[1]][, "intensity"], c(10, 20))
  
  # Second spectrum
  expect_true(is.matrix(peaks[[2]]))
  expect_equal(peaks[[2]][, "mz"], c(150, 250))
  expect_equal(peaks[[2]][, "intensity"], c(15, 25))
})


test_that("mz() and intensity() return NumericList objects", {
  dbcon <- test_dbcon()
  clear_db(dbcon)
  
  # Use character IDs
  dbcon$ms_spectrum_coll$insert(
    data.frame(spectrum_id_ = c("1", "2"), stringsAsFactors = FALSE)
  )
  
  dbcon$ms_peaks_coll$insert(
    data.frame(
      spectrum_id_ = c("1", "2"),
      mz = I(list(c(50, 100), c(75, 125))),
      intensity = I(list(c(5, 10), c(7, 14))),
      stringsAsFactors = FALSE
    )
  )
  
  backend <- backendInitialize(new("MsBackendMongoDb"), dbcon = dbcon)
  backend@spectraIds <- c("1", "2")
  backend@id_map     <- backend@spectraIds
  backend@nspectra   <- length(backend@spectraIds)
  backend@.collections <- list(
    ms_spectrum_coll = dbcon$ms_spectrum_coll,
    ms_peaks_coll    = dbcon$ms_peaks_coll
  )
  backend@peak_fun <- .fetch_peaks_data_long_mongo
  
  mz_vals <- mz(backend)
  int_vals <- intensity(backend)
  
  expect_s4_class(mz_vals, "NumericList")
  expect_s4_class(int_vals, "NumericList")
  
  expect_equal(as.numeric(mz_vals[[1]]), c(50, 100))
  expect_equal(as.numeric(mz_vals[[2]]), c(75, 125))
  
  expect_equal(as.numeric(int_vals[[1]]), c(5, 10))
  expect_equal(as.numeric(int_vals[[2]]), c(7, 14))
})


test_that("subsetting extracts the correct spectra", {
  dbcon <- test_dbcon()
  clear_db(dbcon)
  
  dbcon$ms_spectrum_coll$insert(
    data.frame(spectrum_id_ = c("10", "20", "30"), stringsAsFactors = FALSE)
  )
  
  backend <- backendInitialize(new("MsBackendMongoDb"), dbcon = dbcon)
  backend@spectraIds <- c("10", "20", "30")
  backend@id_map     <- backend@spectraIds
  backend@nspectra   <- length(backend@spectraIds)
  
  # subset second element
  backend2 <- backend[2]
  
  expect_equal(backend2@spectraIds, "20")
  expect_equal(backend2@id_map, "20")
  expect_equal(length(backend2), 1L)
})


test_that("spectraData() returns merged scalar and peaks data", {
  dbcon <- test_dbcon()
  clear_db(dbcon)
  
  # Metadata (character ID)
  dbcon$ms_spectrum_coll$insert(
    data.frame(
      spectrum_id_ = "1",
      msLevel = 2L,
      precursorMz = 123.45,
      stringsAsFactors = FALSE
    )
  )
  
  # Peaks
  dbcon$ms_peaks_coll$insert(
    data.frame(
      spectrum_id_ = "1",
      mz = I(list(c(100, 200))),
      intensity = I(list(c(10, 20))),
      stringsAsFactors = FALSE
    )
  )
  
  backend <- backendInitialize(new("MsBackendMongoDb"), dbcon = dbcon)
  backend@spectraIds <- "1"
  backend@id_map     <- backend@spectraIds
  backend@nspectra   <- length(backend@spectraIds)
  backend@.collections <- list(
    ms_spectrum_coll = dbcon$ms_spectrum_coll,
    ms_peaks_coll    = dbcon$ms_peaks_coll
  )
  backend@peak_fun <- .fetch_peaks_data_long_mongo
  
  d <- spectraData(backend)
  
  expect_true("spectrum_id_" %in% colnames(d))
  expect_true("msLevel" %in% colnames(d))
  expect_true("precursorMz" %in% colnames(d))
  expect_true("mz" %in% colnames(d))
  expect_true("intensity" %in% colnames(d))
  
  expect_equal(d$mz[[1]], c(100, 200))
  expect_equal(d$intensity[[1]], c(10, 20))
})



test_that("subsetting with numeric, logical, and character indices works with peaksData and spectraData", {
  dbcon <- test_dbcon()
  clear_db(dbcon)
  
  # Insert spectra metadata
  dbcon$ms_spectrum_coll$insert(
    data.frame(
      spectrum_id_ = c("spec1", "spec2", "spec3"),
      precursorMz  = c(150, 160, 170),
      msLevel      = c(2, 2, 2),
      stringsAsFactors = FALSE
    )
  )
  
  # Insert peaks
  dbcon$ms_peaks_coll$insert(
    data.frame(
      spectrum_id_ = c("spec1", "spec2", "spec3"),
      mz          = I(list(c(100, 200), c(150, 250), c(175, 275))),
      intensity   = I(list(c(10, 20), c(15, 25), c(17, 27))),
      stringsAsFactors = FALSE
    )
  )
  
  be <- backendInitialize(new("MsBackendMongoDb"), dbcon = dbcon)
  
  # Numeric subsetting
  be_num <- be[1:2]
  expect_equal(be_num@spectraIds, c("spec1", "spec2"))
  expect_equal(be_num@nspectra, 2)
  
  peaks_num <- peaksData(be_num)
  expect_length(peaks_num, 2)
  expect_equal(peaks_num[[1]][, "mz"], c(100, 200))
  expect_equal(peaks_num[[2]][, "mz"], c(150, 250))
  
  spdata_num <- spectraData(be_num, columns = c("spectrum_id_", "precursorMz", "mz", "intensity"))
  expect_equal(nrow(spdata_num), 2)
  expect_equal(spdata_num$spectrum_id_, c("spec1", "spec2"))
  
  # Logical subsetting
  be_log <- be[c(TRUE, FALSE, TRUE)]
  expect_equal(be_log@spectraIds, c("spec1", "spec3"))
  
  peaks_log <- peaksData(be_log)
  expect_equal(peaks_log[[1]][, "mz"], c(100, 200))
  expect_equal(peaks_log[[2]][, "mz"], c(175, 275))
  
  spdata_log <- spectraData(be_log, columns = c("spectrum_id_", "precursorMz", "mz", "intensity"))
  expect_equal(spdata_log$spectrum_id_, c("spec1", "spec3"))
  
  # Character subsetting
  be_char <- be[c("spec2")]
  expect_equal(be_char@spectraIds, "spec2")
  
  peaks_char <- peaksData(be_char)
  expect_equal(peaks_char[[1]][, "mz"], c(150, 250))
  expect_equal(peaks_char[[1]][, "intensity"], c(15, 25))
  
  spdata_char <- spectraData(be_char, columns = c("spectrum_id_", "precursorMz", "mz", "intensity"))
  expect_equal(nrow(spdata_char), 1)
  expect_equal(spdata_char$spectrum_id_, "spec2")
  expect_s4_class(spdata_char$mz, "NumericList")
  expect_equal(as.numeric(spdata_char$mz[[1]]), c(150, 250))
  expect_equal(as.numeric(spdata_char$intensity[[1]]), c(15, 25))
  
  clear_db(dbcon)
})

test_that("dataStorage returns correct character vector with two collections", {
  dbcon <- test_dbcon()
  clear_db(dbcon)
  
  be <- MsBackendMongoDb()
  be <- backendInitialize(be, dbcon = dbcon)
  
  # Empty backend
  expect_equal(dataStorage(be), character(0))
  
  # Insert spectra and peaks
  dbcon$ms_spectrum_coll$insert(
    data.frame(
      spectrum_id_ = c("spec1", "spec2", "spec3"),
      precursorMz  = c(150, 160, 170),
      msLevel      = c(2, 2, 2),
      stringsAsFactors = FALSE
    )
  )
  
  dbcon$ms_peaks_coll$insert(
    data.frame(
      spectrum_id_ = c("spec1", "spec2", "spec3"),
      mz          = I(list(c(100, 200), c(150, 250), c(175, 275))),
      intensity   = I(list(c(10, 20), c(15, 25), c(17, 27))),
      stringsAsFactors = FALSE
    )
  )
  
  be <- backendInitialize(be, dbcon = dbcon)
  storage <- dataStorage(be)
  expect_type(storage, "character")
  expect_length(storage, 3)
  expect_true(all(storage == "ms_spectrum_coll"))
  
  clear_db(dbcon)
})


test_that("reset restores the backend with two collections", {
  dbcon <- test_dbcon()
  clear_db(dbcon)
  
  # Insert spectra and peaks
  dbcon$ms_spectrum_coll$insert(
    data.frame(
      spectrum_id_ = c("spec1", "spec2", "spec3"),
      precursorMz  = c(150, 160, 170),
      msLevel      = c(2, 2, 2),
      stringsAsFactors = FALSE
    )
  )
  
  dbcon$ms_peaks_coll$insert(
    data.frame(
      spectrum_id_ = c("spec1", "spec2", "spec3"),
      mz          = I(list(c(100, 200), c(150, 250), c(175, 275))),
      intensity   = I(list(c(10, 20), c(15, 25), c(17, 27))),
      stringsAsFactors = FALSE
    )
  )
  
  be <- MsBackendMongoDb()
  be <- backendInitialize(be, dbcon = dbcon)
  
  expect_equal(be@nspectra, 3L)
  
  # Subset backend
  be_sub <- be[1:2]
  expect_equal(be_sub@nspectra, 2L)
  
  # Reset
  be_reset <- reset(be_sub)
  expect_equal(be_reset@nspectra, 3L)
  expect_equal(be_reset@spectraIds, c("spec1", "spec2", "spec3"))
  expect_equal(be_reset@id_map, c("spec1", "spec2", "spec3"))
  
  # peaksData should return correct peaks after reset
  peaks <- peaksData(be_reset)
  expect_equal(peaks[[3]][, "mz"], c(175, 275))
  expect_equal(peaks[[3]][, "intensity"], c(17, 27))
  
  clear_db(dbcon)
})

