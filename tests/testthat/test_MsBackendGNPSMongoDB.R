with_reference_fixture <- function(format, check) {
  conn <- tryCatch({
    connection <- mongolite::mongo(
      collection = basename(tempfile(pattern = "reference_backend_")),
      db = "test_db",
      url = "mongodb://localhost"
    )
    connection$run('{"ping": 1}')
    connection
  }, error = function(e) NULL)
  
  testthat::skip_if(is.null(conn), "Local MongoDB is unavailable")
  on.exit(conn$drop(), add = TRUE)
  
  if (format == "gnps") {
    fixture <- data.frame(
      spectrum_id = c("GNPS_TEST_1", "GNPS_TEST_2"),
      Precursor_MZ = c("500.12", "600.34"),
      Ion_Mode = c("positive", "negative"),
      ms_level = c("2", "2"),
      peaks_json = c(
        "[(123.4, 567), (124.5, 678)]",
        "[[125.6, 789], [126.7, 890]]"
      )
    )
    conn$insert(fixture)
  } else {
    # Insert JSON directly to preserve nested m/z–intensity arrays.
    fixture <- lapply(2:5, function(level) {
      list(
        precursor_mz = 500 - level,
        polarity = "-",
        ms_level = level,
        compound_name = "MSNLIB_TEST",
        smiles = "CCO",
        inchikey = "LFQSCWFLJHTTHZ-UHFFFAOYSA-N",
        feature_id = "test_acquisition_feature_1",
        merge_type = "SINGLE_BEST_SCAN",
        peaks = list(
          c(100 + level, 10 * level),
          c(200 + level, 20 * level)
        )
      )
    })
    
    documents <- vapply(
      fixture,
      function(document) {
        as.character(jsonlite::toJSON(document, auto_unbox = TRUE))
      },
      character(1)
    )
    
    conn$insert(documents)
  }
  
  check(conn)
}


testthat::test_that("backendInitialize() creates a valid GNPS backend", {
  with_reference_fixture("gnps", function(conn) {
    backend <- ProtGenerics::backendInitialize(
      MsBackendGnpsMongoDb(),
      dbcon = conn,
      format = "gnps"
    )
    
    testthat::expect_s4_class(backend, "MsBackendGnpsMongoDb")
    testthat::expect_true(methods::validObject(backend))
    testthat::expect_length(backend, 2L)
    
    ids <- Spectra::spectraNames(backend)
    testthat::expect_length(unique(ids), 2L)
    testthat::expect_true(all(grepl("^[[:xdigit:]]{24}$", ids)))
  })
})


testthat::test_that("spectraData() maps GNPS metadata correctly", {
  with_reference_fixture("gnps", function(conn) {
    sps <- Spectra::Spectra(
      conn,
      source = MsBackendGnpsMongoDb(),
      format = "gnps"
    )
    
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
  with_reference_fixture("gnps", function(conn) {
    sps <- Spectra::Spectra(
      conn,
      source = MsBackendGnpsMongoDb(),
      format = "gnps"
    )
    
    metadata <- Spectra::spectraData(sps, columns = "spectrum_id")
    index <- match(c("GNPS_TEST_1", "GNPS_TEST_2"), metadata$spectrum_id)
    selected <- sps[index]
    peaks <- Spectra::peaksData(selected)
    
    testthat::expect_length(peaks, 2L)
    testthat::expect_identical(names(peaks), Spectra::spectraNames(selected))
    
    testthat::expect_equal(
      peaks[[1L]],
      cbind(mz = c(123.4, 124.5), intensity = c(567, 678))
    )
    testthat::expect_equal(
      peaks[[2L]],
      cbind(mz = c(125.6, 126.7), intensity = c(789, 890))
    )
  })
})


testthat::test_that("subsetting preserves order and repeated spectra", {
  with_reference_fixture("gnps", function(conn) {
    sps <- Spectra::Spectra(
      conn,
      source = MsBackendGnpsMongoDb(),
      format = "gnps"
    )
    
    metadata <- Spectra::spectraData(sps, columns = "spectrum_id")
    requested_ids <- c("GNPS_TEST_2", "GNPS_TEST_1", "GNPS_TEST_2")
    index <- match(requested_ids, metadata$spectrum_id)
    expected_names <- Spectra::spectraNames(sps)[index]
    
    selected <- sps[index]
    result <- Spectra::spectraData(
      selected,
      columns = c("spectrum_id", "precursorMz")
    )
    peaks <- Spectra::peaksData(selected)
    
    testthat::expect_identical(result$spectrum_id, requested_ids)
    testthat::expect_equal(result$precursorMz, c(600.34, 500.12, 600.34))
    testthat::expect_identical(names(peaks), expected_names)
    testthat::expect_identical(peaks[[1L]], peaks[[3L]])
  })
})


testthat::test_that("spectraData() maps MSnLib metadata for MS2 to MS5", {
  with_reference_fixture("msnlib", function(conn) {
    sps <- Spectra::Spectra(
      conn,
      source = MsBackendGnpsMongoDb(),
      format = "msnlib"
    )
    
    metadata <- Spectra::spectraData(
      sps,
      columns = c(
        "spectrum_id", "precursorMz", "polarity", "msLevel",
        "smiles", "inchikey", "feature_id", "merge_type"
      )
    )
    
    testthat::expect_length(sps, 4L)
    testthat::expect_true(methods::validObject(sps))
    testthat::expect_setequal(metadata$msLevel, 2:5)
    
    metadata <- metadata[order(metadata$msLevel), , drop = FALSE]
    
    testthat::expect_equal(metadata$precursorMz, 500 - (2:5))
    testthat::expect_identical(metadata$polarity, rep(0L, 4L))
    testthat::expect_identical(metadata$smiles, rep("CCO", 4L))
    testthat::expect_identical(
      metadata$inchikey,
      rep("LFQSCWFLJHTTHZ-UHFFFAOYSA-N", 4L)
    )
    testthat::expect_identical(
      metadata$feature_id,
      rep("test_acquisition_feature_1", 4L)
    )
    testthat::expect_identical(
      metadata$merge_type,
      rep("SINGLE_BEST_SCAN", 4L)
    )
    testthat::expect_setequal(
      metadata$spectrum_id,
      Spectra::spectraNames(sps)
    )
  })
})


testthat::test_that("peaksData() reads the MSnLib peaks field", {
  with_reference_fixture("msnlib", function(conn) {
    sps <- Spectra::Spectra(
      conn,
      source = MsBackendGnpsMongoDb(),
      format = "msnlib"
    )
    
    levels <- Spectra::spectraData(sps, columns = "msLevel")$msLevel
    selected <- sps[match(2:5, levels)]
    peaks <- Spectra::peaksData(selected)
    
    testthat::expect_identical(names(peaks), Spectra::spectraNames(selected))
    
    for (level in 2:5) {
      testthat::expect_equal(
        peaks[[level - 1L]],
        cbind(
          mz = c(100 + level, 200 + level),
          intensity = c(10 * level, 20 * level)
        )
      )
    }
    
    mz_only <- Spectra::peaksData(selected, columns = "mz")
    testthat::expect_identical(colnames(mz_only[[1L]]), "mz")
    testthat::expect_equal(mz_only[[1L]][, "mz"], c(102, 202))
  })
})


# Optional integration test using the existing BD.gnps collection.
# This test reads the collection without modifying its documents.
testthat::test_that("GNPS backend reads the imported ALL_GNPS library", {
  testthat::skip_if(
    Sys.getenv("TEST_GNPS_LIBRARY") != "true",
    "Set TEST_GNPS_LIBRARY=true to test BD.gnps"
  )
  
  conn <- mongolite::mongo(
    collection = "gnps",
    db = "BD",
    url = "mongodb://localhost"
  )
  
  testthat::expect_gt(conn$count(), 0)
  
  sps <- Spectra::Spectra(
    conn,
    source = MsBackendGnpsMongoDb(),
    format = "gnps"
  )
  testthat::expect_equal(length(sps), conn$count())
  
  # Locate one reference directly, avoiding retrieval of all metadata.
  id <- "CCMSLIB00000001553"
  raw <- conn$find(
    query = jsonlite::toJSON(list(spectrum_id = id), auto_unbox = TRUE),
    fields = '{"_id": 1, "peaks_json": 1}',
    limit = 2L
  )
  
  testthat::expect_equal(nrow(raw), 1L)
  if (nrow(raw) != 1L)
    stop("Expected exactly one document for the reference spectrum.")
  
  index <- match(
    as.character(raw[["_id"]][[1L]]),
    Spectra::spectraNames(sps)
  )
  testthat::expect_false(is.na(index))
  
  selected <- sps[index]
  metadata <- Spectra::spectraData(
    selected,
    columns = c("spectrum_id", "precursorMz")
  )
  
  testthat::expect_identical(metadata$spectrum_id, id)
  testthat::expect_equal(metadata$precursorMz, 689)
  
  expected <- jsonlite::fromJSON(
    chartr("()", "[]", raw$peaks_json[[1L]])
  )
  colnames(expected) <- c("mz", "intensity")
  
  actual <- Spectra::peaksData(selected)[[1L]]
  testthat::expect_equal(actual, expected, tolerance = 0)
})