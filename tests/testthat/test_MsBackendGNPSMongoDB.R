with_gnps_fixture <- function(check) {
  conn <- tryCatch({
    connection <- mongolite::mongo(
      collection = basename(tempfile(pattern = "gnps_backend_")),
      db = "test_db",
      url = "mongodb://localhost"
    )
    connection$run('{"ping": 1}')
    connection
  }, error = function(e) NULL)
  
  testthat::skip_if(is.null(conn), "Local MongoDB is unavailable")
  on.exit(conn$drop(), add = TRUE)
  
  conn$insert(data.frame(
    spectrum_id = c("GNPS_TEST_1", "GNPS_TEST_2"),
    Precursor_MZ = c("500.12", "600.34"),
    Ion_Mode = c("positive", "negative"),
    ms_level = c("2", "2"),
    peaks_json = c(
      "[(123.4, 567), (124.5, 678)]",
      "[[125.6, 789], [126.7, 890]]"
    )
  ))
  
  check(conn)
}

testthat::test_that("backendInitialize() creates a valid GNPS backend", {
  with_gnps_fixture(function(conn) {
    backend <- ProtGenerics::backendInitialize(
      MsBackendGnpsMongoDb(),
      dbcon = conn
    )
    
    testthat::expect_s4_class(backend, "MsBackendGnpsMongoDb")
    testthat::expect_true(methods::validObject(backend))
    testthat::expect_length(backend, 2L)
  })
})

testthat::test_that("spectraData() maps GNPS metadata to Spectra variables", {
  with_gnps_fixture(function(conn) {
    sps <- Spectra::Spectra(conn, source = MsBackendGnpsMongoDb())
    metadata <- Spectra::spectraData(
      sps,
      columns = c("spectrum_id", "precursorMz", "polarity", "msLevel")
    )
    
    expected_ids <- c("GNPS_TEST_1", "GNPS_TEST_2")
    testthat::expect_setequal(metadata$spectrum_id, expected_ids)
    index <- match(expected_ids, metadata$spectrum_id)
    
    testthat::expect_equal(metadata$precursorMz[index], c(500.12, 600.34))
    testthat::expect_identical(metadata$polarity[index], c(1L, 0L))
    testthat::expect_identical(metadata$msLevel[index], c(2L, 2L))
  })
})

testthat::test_that("peaksData() decodes both GNPS peak formats", {
  with_gnps_fixture(function(conn) {
    sps <- Spectra::Spectra(conn, source = MsBackendGnpsMongoDb())
    peaks <- Spectra::peaksData(sps)
    
    testthat::expect_length(peaks, 2L)
    testthat::expect_equal(
      peaks[["GNPS_TEST_1"]],
      cbind(mz = c(123.4, 124.5), intensity = c(567, 678))
    )
    testthat::expect_equal(
      peaks[["GNPS_TEST_2"]],
      cbind(mz = c(125.6, 126.7), intensity = c(789, 890))
    )
  })
})

testthat::test_that("subsetting preserves spectrum order and repeated IDs", {
  with_gnps_fixture(function(conn) {
    sps <- Spectra::Spectra(conn, source = MsBackendGnpsMongoDb())
    ids <- Spectra::spectraData(sps, columns = "spectrum_id")$spectrum_id
    
    requested_ids <- c("GNPS_TEST_2", "GNPS_TEST_1", "GNPS_TEST_2")
    selected <- sps[match(requested_ids, ids)]
    
    metadata <- Spectra::spectraData(
      selected, columns = c("spectrum_id", "precursorMz")
    )
    peaks <- Spectra::peaksData(selected)
    
    testthat::expect_identical(metadata$spectrum_id, requested_ids)
    testthat::expect_equal(metadata$precursorMz, c(600.34, 500.12, 600.34))
    testthat::expect_identical(names(peaks), requested_ids)
    testthat::expect_identical(peaks[[1]], peaks[[3]])
  })
})

# Integration test using a local MongoDB instance.
# The GNPS public spectral library (ALL_GNPS_cleaned.json) was imported into
# the gnps collection of the BD database using this command:
#
# mongoimport.exe --drop -v --host localhost --db BD --collection gnps --type json --jsonArray --file "path\ALL_GNPS_cleaned.json"
#
# Each document in the collection stores the metadata and peaks for one
# spectrum. The test connects to the collection, creates a Spectra object
# backed by MsBackendGnpsMongoDb, and verifies the precursor m/z and peak
# data for a known GNPS spectrum ID.
testthat::test_that("GNPS backend reads the imported ALL_GNPS library", {
  testthat::skip_if(
    Sys.getenv("TEST_GNPS_LIBRARY") != "true",
    "Set TEST_GNPS_LIBRARY=true to test the local BD.gnps collection"
  )
  
  conn <- mongolite::mongo(
    collection = "gnps",
    db = "BD",
    url = "mongodb://localhost"
  )
  testthat::expect_gt(conn$count(), 0)
  
  sps <- Spectra::Spectra(conn, source = MsBackendGnpsMongoDb())
  testthat::expect_equal(length(sps), conn$count())
  
  # Find a known spectrum by ID, independently of collection order.
  id <- "CCMSLIB00000001553"
  ids <- Spectra::spectraData(sps, columns = "spectrum_id")$spectrum_id
  index <- match(id, ids)
  testthat::expect_false(is.na(index))
  
  selected <- sps[index]
  metadata <- Spectra::spectraData(
    selected, columns = c("spectrum_id", "precursorMz")
  )
  testthat::expect_identical(metadata$spectrum_id, id)
  testthat::expect_equal(metadata$precursorMz, 689)
  
  # Decode the original peak text independently of the backend.
  raw <- conn$find(
    query = jsonlite::toJSON(list(spectrum_id = id), auto_unbox = TRUE),
    fields = '{"peaks_json": 1, "_id": 0}',
    limit = 2L
  )
  testthat::expect_equal(nrow(raw), 1L)
  
  expected <- jsonlite::fromJSON(chartr("()", "[]", raw$peaks_json[[1L]]))
  colnames(expected) <- c("mz", "intensity")
  
  actual <- Spectra::peaksData(selected)[[1L]]
  testthat::expect_equal(actual, expected, tolerance = 0)
})

