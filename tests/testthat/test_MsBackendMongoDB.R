
library(testthat)

test_dbcon <- function() {
  mongo(collection = "compounds_info_coll", db = "test_spectra_db", url = "mongodb://localhost")
}

clear_db <- function(dbcon) {
  dbcon$compounds_info_coll$drop()
}


# Sample spectra-like data for testing
sample_df <- data.frame(
  spectrum_id = 1:2,
  compound_id = c(101L, 102L),
  stringsAsFactors = FALSE
)
sample_df$peaks <- list(
  list(mz = c(100, 200), intensity = c(10, 20)),
  list(mz = c(300), intensity = c(50))
)


test_that("MsBackendMongoDB constructor works", {
  dbcon <- list(compounds_info_coll = test_dbcon())
  clear_db(dbcon)
  
  backend <- MsBackendMongoDB(dbcon = dbcon)
  expect_s4_class(backend, "MsBackendMongoDB")
  expect_equal(backend@nspectra, 0L)
  expect_equal(length(backend@spectraIds), 0L)
})


test_that("MsBackendMongoDB validity checks work", {
  
  # Create a test MongoDB connection
  dbcon <- list(compounds_info_coll = test_dbcon())
  
  # Helper to clear the test collection
  clear_db <- function(dbcon) dbcon$compounds_info_coll$drop()
  clear_db(dbcon)
  
  # Insert a simple document to have something in MongoDB
  dbcon$compounds_info_coll$insert(
    list(
      spectrum_id = 1L,
      peaks = list(list(mz = c(100,200), intensity = c(10,20)))
    )
  )
  

  # Case 1: Valid backend
  be <- MsBackendMongoDB(dbcon = dbcon, spectraIds = 1L)
  expect_silent(validObject(be))
  

  # Case 2: Mismatched nspectra vs spectraIds
  be_wrong_nspectra <- be
  be_wrong_nspectra@nspectra <- 2L
  be_wrong_nspectra@localData <- data.frame() 
  expect_error(
    validObject(be_wrong_nspectra),
    "Number of spectraIds does not match nspectra"
  )
  

  # Case 3: localData with wrong number of rows
  be_wrong_local <- be
  be_wrong_local@localData <- data.frame(dummy = 1:2)  # nspectra = 1
  expect_error(
    validObject(be_wrong_local),
    "Number of rows in local data and number of spectra don't match"
  )
  
  
  # Case 4: Invalid dbcon (NULL)
  be_invalid_db <- be
  be_invalid_db@dbcon <- NULL
  expect_error(
    validObject(be_invalid_db),
    "'dbcon' is NULL or empty"
  )
  
  # Case 5: Invalid dbcon (not mongo object)
  be_invalid_db2 <- be
  be_invalid_db2@dbcon <- list(not_a_mongo = 1)
  expect_error(
    validObject(be_invalid_db2),
    "'dbcon' should be a mongolite::mongo object or a list of mongo objects"
  )

  clear_db(dbcon)
})



test_that("backendInitialize works and stores spectraIds", {
  dbcon <- list(compounds_info_coll = test_dbcon())
  clear_db(dbcon)
  
  # Insert test data
  .insert_backend_mongo(dbcon, sample_df)
  
  # Pass dbcon at initialization
  backend <- MsBackendMongoDB(dbcon = dbcon)
  backend <- backendInitialize(backend, dbcon = dbcon)
  
  expect_s4_class(backend, "MsBackendMongoDB")
  expect_equal(backend@nspectra, 2L)
  expect_equal(backend@spectraIds, 1:2)
  expect_true("compounds_info_coll" %in% names(backend@.collections))
})

test_that("peaksData returns standardized peaks", {
  dbcon <- list(compounds_info_coll = test_dbcon())
  clear_db(dbcon)
  
  .insert_backend_mongo(dbcon, sample_df)
  
  backend <- MsBackendMongoDB(dbcon = dbcon)
  backend <- backendInitialize(backend, dbcon = dbcon)
  peaks <- peaksData(backend)
  
  expect_length(peaks, 2)
  
  ## Spectrum 1
  expect_true(is.matrix(peaks[[1]]))
  expect_equal(colnames(peaks[[1]]), c("mz", "intensity"))
  expect_type(peaks[[1]][, "mz"], "double")
  expect_type(peaks[[1]][, "intensity"], "double")
  
  ## Spectrum 2
  expect_true(is.matrix(peaks[[2]]))
  expect_equal(colnames(peaks[[2]]), c("mz", "intensity"))
  expect_type(peaks[[2]][, "mz"], "double")
  expect_type(peaks[[2]][, "intensity"], "double")
})

test_that("peaksVariables, MsBackendMongoDB works", {
  backend <- MsBackendMongoDB(dbcon = list(compounds_info_coll = test_dbcon()))
  backend <- backendInitialize(backend, dbcon = backend@dbcon)
  
  expect_equal(peaksVariables(backend), c("mz", "intensity"))
})

test_that("intensity<-, MsBackendMongoDB works", {
  backend <- MsBackendMongoDB(dbcon = list(compounds_info_coll = test_dbcon()))
  backend <- backendInitialize(backend, dbcon = backend@dbcon)
  
  expect_error(intensity(backend) <- 1:5, "replace")
})

test_that("mz<-, MsBackendMongoDB works", {
  backend <- MsBackendMongoDB(dbcon = list(compounds_info_coll = test_dbcon()))
  backend <- backendInitialize(backend, dbcon = backend@dbcon)
  
  expect_error(mz(backend) <- 1:5, "replace")
})



test_that("spectraData returns requested columns", {
  dbcon <- list(compounds_info_coll = test_dbcon())
  clear_db(dbcon)
  .insert_backend_mongo(dbcon, sample_df)
  
  backend <- MsBackendMongoDB(dbcon = dbcon)
  backend <- backendInitialize(backend, dbcon = dbcon)
  
  spdata <- spectraData(backend, columns = c("compound_id", "spectrum_id"))
  expect_true(all(c("compound_id", "spectrum_id") %in% colnames(spdata)))
})


test_that("subsetting and extractByIndex works", {
  dbcon <- list(compounds_info_coll = test_dbcon())
  clear_db(dbcon)
  .insert_backend_mongo(dbcon, sample_df)
  
  backend <- MsBackendMongoDB(dbcon = dbcon)
  backend <- backendInitialize(backend, dbcon = dbcon)
  
  sub_backend <- backend[1]
  expect_s4_class(sub_backend, "MsBackendMongoDB")
  expect_equal(sub_backend@spectraIds, 1L)
})

test_that("spectraNames returns spectrum IDs", {
  dbcon <- list(compounds_info_coll = test_dbcon())
  clear_db(dbcon)
  .insert_backend_mongo(dbcon, sample_df)
  
  backend <- MsBackendMongoDB(dbcon = dbcon)
  backend <- backendInitialize(backend, dbcon = dbcon)
  
  names <- spectraNames(backend)
  expect_equal(names, as.character(1:2))
})

test_that("dataStorage returns a description of the backend", {
  # Create a test MongoDB connection
  dbcon <- list(compounds_info_coll = test_dbcon())
  
  # Initialize the backend
  backend <- MsBackendMongoDB(dbcon = dbcon)
  backend <- backendInitialize(backend, dbcon = dbcon)
  
  # Check that dataStorage returns a character string
  storage_desc <- dataStorage(backend)
  expect_type(storage_desc, "character")
  expect_true(grepl("MongoDB", storage_desc))
  expect_true(grepl("compounds_info_coll", storage_desc))
  
  backend_no_db <- backend
  backend_no_db@dbcon <- NULL
  expect_equal(dataStorage(backend_no_db), character())
})



test_that("reset restores backend", {
  dbcon <- list(compounds_info_coll = test_dbcon())
  clear_db(dbcon)
  .insert_backend_mongo(dbcon, sample_df)
  
  backend <- MsBackendMongoDB(dbcon = dbcon)
  backend <- backendInitialize(backend, dbcon = dbcon)
  backend_reset <- reset(backend)
  
  expect_s4_class(backend_reset, "MsBackendMongoDB")
  expect_equal(backend_reset@spectraIds, 1:2)
})


clear_db(list(compounds_info_coll = test_dbcon()))






































