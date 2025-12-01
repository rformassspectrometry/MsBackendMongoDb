library(testthat)

test_dbcon <- function() {
  temp_db <- paste0("test_db_", as.integer(Sys.time()))
  list(
    compounds_info_coll = mongo(
      collection = "compounds_info_coll",
      db = temp_db,
      url = "mongodb://localhost:27017"
    )
  )
}

clear_db <- function(dbcon) {
  dbcon$compounds_info_coll$drop()
}

test_that(".connect_mongodb returns valid mongo connection list", {
  conns <- .connect_mongodb(db = paste0("test_db_", as.integer(Sys.time())))
  expect_true(is.list(conns))
  expect_true(inherits(conns$compounds_info_coll, "mongo"))
})


test_that(".valid_mongocon validates connections correctly", {
  expect_match(.valid_mongocon(NULL), "'dbcon' is NULL or empty")
  expect_match(.valid_mongocon(5), "'dbcon' should be a mongolite::mongo object or a list of mongo objects")
  
  temp_db <- paste0("test_db_", as.integer(Sys.time()))
  
  con_wrong <- mongo(collection = "other_coll", db = temp_db, url = "mongodb://localhost")
  res <- .valid_mongocon(list(other_coll = con_wrong))
  expect_match(res, "Missing required collection: compounds_info_coll")
  
  con_required <- mongo(collection = "compounds_info_coll", db = temp_db, url = "mongodb://localhost")
  expect_null(.valid_mongocon(con_required))
  
  con_wrong$drop()
  con_required$drop()
})


test_that(".dbcon accessor returns correct dbcon", {
  dbcon <- test_dbcon()
  be <- MsBackendMongoDB(dbcon = dbcon, spectraIds = 1L)
  expect_identical(.dbcon(be), dbcon)
  clear_db(dbcon)
})

test_that(".encode_peaks works with multiple formats", {
  df <- data.frame(mz = c(100, 200), intensity = c(10, 20))
  expect_equal(.encode_peaks(df)$mz, c(100, 200))
  
  mat <- matrix(c(100,200,10,20), ncol = 2)
  expect_equal(.encode_peaks(mat)$intensity, c(10,20))
  
  lst <- list(mz = c(100,200), intensity = c(10,20))
  expect_equal(.encode_peaks(lst)$mz, c(100,200))
  
  expect_equal(.encode_peaks(NULL)$intensity, numeric())
  
  expect_error(.encode_peaks(1:5), "Unsupported peaks format")
})

test_that(".fetch_peaks extracts peaks correctly", {
  doc <- list(mz = c(50, 100), intensity = c(5, 10))
  expect_equal(.fetch_peaks(doc)$mz, c(50, 100))
  
  expect_equal(.fetch_peaks(NULL)$intensity, numeric())
  expect_error(.fetch_peaks(list(a=1,b=2)), "Document missing mz/intensity")
})

test_that(".available_peaks_variables_mongo returns column names", {
  dbcon <- test_dbcon()
  clear_db(dbcon)
  
  dbcon$compounds_info_coll$insert(list(
    spectrum_id = 1L,
    peaks = list(list(mz = c(100,200), intensity = c(10,20)))
  ))
  
  be <- MsBackendMongoDB(dbcon = dbcon, spectraIds = 1L)
  
  vars <- .available_peaks_variables_mongo(dbcon)
  expect_true(all(c("mz", "intensity") %in% vars))
})


test_that(".fetch_peaks_data_long_mongo fetches peaks correctly", {
  dbcon <- test_dbcon()
  clear_db(dbcon)
  
  dbcon$compounds_info_coll$insert(list(
    spectrum_id = 1L,
    peaks = list(list(mz = c(100,200), intensity = c(10,20)))
  ))
  
  be <- MsBackendMongoDB(dbcon = dbcon, spectraIds = 1L)
  
  res <- .fetch_peaks_data_long_mongo(be)
  
  expect_true(is.matrix(res[[1]]))
  expect_equal(colnames(res[[1]]), c("mz", "intensity"))
  
  expect_equal(res[[1]][, "mz"], c(100,200))
  expect_equal(res[[1]][, "intensity"], c(10,20))
  
  clear_db(dbcon)
})


test_that(".insert_spectra_mongo inserts spectra correctly with dynamic fields", {
 
  dbcon <- test_dbcon()
  clear_db(dbcon)
  
  # Create a DataFrame of spectra data
  spd <- DataFrame(
    spectrum_id = 1:2,
    msLevel = c(1L, 2L),
    polarity = c(-1L, 1L),
    compound_id = c(1001L, 1002L),
    precursor_mz = c(345.2, 567.8),
    acquisitionNum = c(10L, 20L),
    collision_energy = c(20, 30)
  )

  spd$mz <- list(
    c(10, 20),
    c(30, 40)
  )
  spd$intensity <- list(
    c(1, 2),
    c(5, 6)
  )

  sp <- Spectra(spd)
  
  expect_invisible(.insert_spectra_mongo(dbcon, sp))
  out <- dbcon$compounds_info_coll$find('{}')
  

  expect_equal(length(out$spectrum_id), 2)
  expect_true(all(c("spectrum_id", "msLevel", "peaks") %in% names(out)))
  expect_true(all(c("polarity", "compound_id", "precursor_mz", "acquisitionNum", "collision_energy") %in% names(out)))
  
  # Check peaks format
  expect_true(all(sapply(out$peaks, function(p) {
    p <- if (is.list(p) && length(p) == 1 && is.list(p[[1]])) p[[1]] else p
    all(c("mz", "intensity") %in% names(p))
  })))
  
  # Cleanup
  clear_db(dbcon)
})



test_that(".spectra_data_mongo returns expected structure", {
  
  # Setup MongoDB connection
  dbcon <- test_dbcon()
  clear_db(dbcon)
  
  # Insert test spectrum
  dbcon$compounds_info_coll$insert(list(
    spectrum_id = 1L,
    msLevel = 2L,
    precursor_mz = 150.0,
    peaks = list(list(
      mz = c(50, 100),
      intensity = c(5, 10)
    ))
  ))
  
  # Create backend
  be <- MsBackendMongoDB(dbcon = dbcon, spectraIds = 1L)
  
  # Call the function including peaks
  res <- .spectra_data_mongo(be, columns = c("spectrum_id", "msLevel", "precursor_mz", "peaks"))
  
  # Check structure
  expect_s4_class(res, "DataFrame")
  expect_equal(res$spectrum_id, 1L)
  expect_equal(res$msLevel, 2L)
  expect_equal(res$precursor_mz, 150)
  
  # Check peaks column
  expect_s4_class(res$peaks, "List")
  expect_true(is.list(res$peaks[[1]]))
  expect_named(res$peaks[[1]], c("mz", "intensity"))
  expect_equal(res$peaks[[1]]$mz, c(50, 100))
  expect_equal(res$peaks[[1]]$intensity, c(5, 10))
  
  
  # Clean up
  clear_db(dbcon)
})



test_that(".fetch_spectra_data_mongo returns expected structure", {
  
  # Setup: create test MongoDB connection
  dbcon <- test_dbcon()
  clear_db(dbcon)
  
  # Insert test document
  dbcon$compounds_info_coll$insert(list(
    spectrum_id = 1L,
    msLevel = 2L,
    precursor_mz = 150.0,
    peaks = list(list(
      mz = c(50, 100),
      intensity = c(5, 10)
    ))
  ))
  
  # Create backend
  be <- MsBackendMongoDB(dbcon = dbcon, spectraIds = 1L)
  
  # Call the function without peaks
  res <- .fetch_spectra_data_mongo(be, columns = c("spectrum_id", "msLevel", "precursor_mz"))
  
  # Check that result is a DataFrame
  expect_s4_class(res, "DataFrame")
  
  # Check scalar values
  expect_equal(res$spectrum_id, 1L)
  expect_equal(res$msLevel, 2L)
  expect_equal(res$precursor_mz, 150)
  
  # Ensure peaks column is not present
  expect_false("peaks" %in% colnames(res))
  
  # Clean up
  clear_db(dbcon)
})



test_that(".insert_backend_mongo and .createMsBackendMongoDB insert correctly with dynamic fields", {
  
  dbcon <- test_dbcon()
  clear_db(dbcon)
  
  df <- data.frame(
    spectrum_id = 1:2,
    msLevel = c(1L, 2L),
    polarity = c(-1, 1),  # extra field
    rtime = c(123.4, 567.8), # another extra field
    stringsAsFactors = FALSE
  )
  
  # Add peaks as list-column
  df$peaks <- I(list(
    list(mz=c(10, 20), intensity=c(1, 2)),
    list(mz=c(30), intensity=c(5))
  ))

  res <- .createMsBackendMongoDB(dbcon, df)
  expect_type(res, "list")
  expect_equal(res$spectrum_id, max(df$spectrum_id))
  
  out <- dbcon$compounds_info_coll$find('{}')

  expect_equal(length(out$spectrum_id), nrow(df))

  expect_true(all(c("polarity", "rtime") %in% names(out)))

  expect_true(all(sapply(out$peaks, function(p) {
    p <- if (is.list(p) && length(p) == 1 && is.list(p[[1]])) p[[1]] else p
    all(c("mz", "intensity") %in% names(p))
  })))
  
  clear_db(dbcon)
})

test_that(".update_spectra_mongo updates and adds spectra correctly", {
  
  # Setup test DB
  dbcon <- test_dbcon()
  clear_db(dbcon)
  
  # Initial Spectra object
  spd <- DataFrame(
    spectrum_id = 1:2,
    msLevel = c(1L, 2L),
    polarity = c(-1L, 1L),
    compound_id = c(1001L, 1002L),
    precursor_mz = c(345.2, 567.8),
    acquisitionNum = c(10L, 20L),
    collision_energy = c(20, 30)
  )
  spd$mz <- list(c(10, 20), c(30, 40))
  spd$intensity <- list(c(1, 2), c(5, 6))
  sp <- Spectra(spd)
  .insert_spectra_mongo(dbcon, sp)

  spd2 <- spd[1, , drop = FALSE]
  spd2$msLevel <- 10L 
  spd2$new_field <- "added_value"
  sp2 <- Spectra(spd2)

  .update_spectra_mongo(dbcon, sp2)

  spd3 <- DataFrame(
    spectrum_id = 3,
    msLevel = 3L,
    polarity = 1L,
    compound_id = 1003L,
    precursor_mz = 789.1,
    acquisitionNum = 30L,
    collision_energy = 40
  )
  spd3$mz <- list(c(50, 60))
  spd3$intensity <- list(c(7, 8))
  sp3 <- Spectra(spd3)
  
  .update_spectra_mongo(dbcon, sp3)
  out <- dbcon$compounds_info_coll$find('{}')
  expect_equal(length(out$spectrum_id), 3)
  expect_equal(out$msLevel[out$spectrum_id == 1], 10)
  expect_true("new_field" %in% names(out))
  expect_equal(out$new_field[out$spectrum_id == 1], "added_value")
  expect_true(3 %in% out$spectrum_id)
  
  # Check peaks format
  expect_true(all(sapply(out$peaks, function(p) {
    p <- if (is.list(p) && length(p) == 1 && is.list(p[[1]])) p[[1]] else p
    all(c("mz", "intensity") %in% names(p))
  })))
  
  # Cleanup
  clear_db(dbcon)
})



test_that(".combine_mongo merges multiple backends", {
  dbcon <- test_dbcon()
  clear_db(dbcon)
  
  for (i in 1:3) dbcon$compounds_info_coll$insert(list(spectrum_id=i, peaks=list(list(mz=numeric(), intensity=numeric()))))
  
  b1 <- MsBackendMongoDB(dbcon=dbcon, spectraIds=1)
  b2 <- MsBackendMongoDB(dbcon=dbcon, spectraIds=2)
  b3 <- MsBackendMongoDB(dbcon=dbcon, spectraIds=3)
  
  combined <- .combine_mongo(list(b1,b2,b3))
  expect_s4_class(combined, "MsBackendMongoDB")
  expect_equal(combined@spectraIds, 1:3)
  
  clear_db(dbcon)
})


test_that(".create_indices_mongo works", {
  dbcon <- test_dbcon()
  expect_true(.create_indices_mongo(dbcon))
})
