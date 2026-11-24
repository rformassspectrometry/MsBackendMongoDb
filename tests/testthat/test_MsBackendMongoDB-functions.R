test_that("connectMsBackendMongoDb returns valid mongo connection list", {
    conns <- connectMsBackendMongoDb(db = paste0("test_db_",
                                                 as.integer(Sys.time())))
    expect_true(is.list(conns))
    expect_true(inherits(conns$ms_spectrum_coll, "mongo"))
    expect_true(inherits(conns$ms_peaks_coll, "mongo"))
    rm(conns)

    dbcon <- test_dbcon()
    clear_db(dbcon)

    ## Insert peaks as list-columns with character spectrum_id_
    dbcon$ms_peaks_coll$insert(
                            data.frame(
                                spectrum_id_ = c("SP1", "SP2"),
                                mz = I(list(c(100, 200), c(150, 250))),
                                intensity = I(list(c(10, 20), c(15, 25))),
                                stringsAsFactors = FALSE
                            )
                        )
    expect_equal(length(dbcon$ms_spectrum_coll$find()), 0)
    expect_equal(length(dbcon$ms_peaks_coll$find()), 3)

    con <- connectMsBackendMongoDb(db = "test_spectra_db")
    expect_equal(length(con$ms_spectrum_coll$find()), 0)
    expect_equal(length(con$ms_peaks_coll$find()), 3)
    con <- connectMsBackendMongoDb(db = "test_spectra_db", clean = TRUE)
    expect_equal(length(con$ms_spectrum_coll$find()), 0)
    expect_equal(length(con$ms_peaks_coll$find()), 0)
    clear_db(con)
})

test_that(".valid_mongocon validates connections correctly", {
    expect_null(.valid_mongocon(NULL))
    expect_match(.valid_mongocon(5),
                 "'dbcon' should be a mongolite::mongo object or a list")

    temp_db <- paste0("test_db_", as.integer(Sys.time()))
    con_wrong <- mongo(collection = "other_coll",
                       db = temp_db, url = "mongodb://localhost")
    res <- .valid_mongocon(list(other_coll = con_wrong))
    expect_match(res, "Missing required collection: ms_spectrum_coll, ")

    con_required <- connectMsBackendMongoDb(db = temp_db)
    expect_null(.valid_mongocon(con_required))

    con_wrong$drop()
    con_required$ms_spectrum_coll$drop()
    con_required$ms_peaks_coll$drop()
})

test_that(".dbcon accessor returns correct dbcon", {
    dbcon <- test_dbcon()
    be <- MsBackendMongoDb(dbcon = dbcon)
    expect_identical(.dbcon(be), dbcon)
    clear_db(dbcon)
})

test_that(".encode_peaks works with multiple formats", {
    df <- data.frame(mz = c(100, 200), intensity = c(10, 20))
    res <- .encode_peaks(df)
    expect_true(is.list(res))
    expect_equal(names(res), c("mz", "intensity"))
    expect_equal(res$mz, c(100, 200))

    mat <- matrix(c(100, 200, 10, 20), ncol = 2)
    res <- .encode_peaks(mat)
    expect_equal(res$mz, c(100, 200))

    lst <- list(mz = c(100, 200), intensity = c(10, 20))
    res <- .encode_peaks(lst)
    expect_equal(res$mz, c(100, 200))

    expect_equal(.encode_peaks(NULL)$mz, numeric())
    expect_error(.encode_peaks(1:5), "Unsupported peaks format")
})

test_that(".fetch_peaks extracts peaks correctly", {
    doc <- list(mz = c(50, 100), intensity = c(5, 10))
    expect_equal(.fetch_peaks(doc)$mz, c(50, 100))
    expect_equal(.fetch_peaks(NULL)$intensity, numeric())
    expect_error(.fetch_peaks(list(a = 1, b = 2)),
                 "Document missing mz/intensity")
})

test_that(".available_peaks_variables_mongo returns column names", {
    dbcon <- test_dbcon()
    clear_db(dbcon)

    dbcon$ms_peaks_coll$insert(list(
                            spectrum_id_ = "SP1",
                            mz = c(100, 200),
                            intensity = c(10, 20)
                        ))

    be <- MsBackendMongoDb(dbcon = dbcon)
    vars <- .available_peaks_variables_mongo(be)
    expect_equal(vars, c("mz", "intensity"))

    clear_db(dbcon)
})

test_that(".fetch_peaks_data_long_mongo handles multiple spectrum_id_", {
  dbcon <- test_dbcon()
  clear_db(dbcon)

  # Insert peaks as list-columns with character spectrum_id_
  dbcon$ms_peaks_coll$insert(
    data.frame(
      spectrum_id_ = c("SP1", "SP2"),
      mz = I(list(c(100, 200), c(150, 250))),
      intensity = I(list(c(10, 20), c(15, 25))),
      stringsAsFactors = FALSE
    )
  )

  # Create backend and set spectraIds
  be <- MsBackendMongoDb()
  be@spectraIds <- c("SP1", "SP2")
  be@id_map     <- c("SP1", "SP2")
  be@nspectra   <- 2L

  # Set collections and peak_fun
  be@.collections <- list(
    ms_spectrum_coll = dbcon$ms_spectrum_coll,
    ms_peaks_coll    = dbcon$ms_peaks_coll
  )
  be@peak_fun <- .fetch_peaks_data_long_mongo

  # Call the function
  res <- .fetch_peaks_data_long_mongo(be)

  # Check that the peaks are correct
  expect_equal(res[[1]][, "mz"], c(100, 200))
  expect_equal(res[[1]][, "intensity"], c(10, 20))
  expect_equal(res[[2]][, "mz"], c(150, 250))
  expect_equal(res[[2]][, "intensity"], c(15, 25))

  clear_db(dbcon)
})

test_that(".fetch_spectra_data_mongo works with scalars + peaks", {
  dbcon <- test_dbcon()
  clear_db(dbcon)

  # Insert scalar metadata with character spectrum_id_
  dbcon$ms_spectrum_coll$insert(
    data.frame(
      spectrum_id_ = c("SP1", "SP2"),
      msLevel      = c(2L, 1L),
      stringsAsFactors = FALSE
    )
  )

  #Insert peaks separately
  dbcon$ms_peaks_coll$insert(
    data.frame(
      spectrum_id_ = c("SP1", "SP2"),
      mz           = I(list(c(100, 200), c(150, 250))),
      intensity    = I(list(c(10, 20), c(15, 25))),
      stringsAsFactors = FALSE
    )
  )

  # Initialize backend
  backend <- MsBackendMongoDb()
  backend@spectraIds <- c("SP1", "SP2")
  backend@id_map     <- c("SP1", "SP2")
  backend@nspectra   <- 2L
  backend@.collections <- list(
    ms_spectrum_coll = dbcon$ms_spectrum_coll,
    ms_peaks_coll    = dbcon$ms_peaks_coll
  )
  backend@dbcon <- backend@.collections
  backend@peak_fun <- .fetch_peaks_data_long_mongo

  # Fetch metadata and peaks
  spdata <- .fetch_spectra_data_mongo(backend, columns = c("spectrum_id_", "msLevel", "mz", "intensity"))

  expect_equal(nrow(spdata), 2)
  expect_equal(spdata$spectrum_id_, c("SP1", "SP2"))
  expect_equal(spdata$msLevel, c(2L, 1L))

  # Peaks are returned as NumericList
  expect_s4_class(spdata$mz, "NumericList")
  expect_s4_class(spdata$intensity, "NumericList")

  expect_equal(as.numeric(spdata$mz[[1]]), c(100, 200))
  expect_equal(as.numeric(spdata$mz[[2]]), c(150, 250))
  expect_equal(as.numeric(spdata$intensity[[1]]), c(10, 20))
  expect_equal(as.numeric(spdata$intensity[[2]]), c(15, 25))

  clear_db(dbcon)
})

test_that(".insert_new_backend_mongo inserts or updates correctly", {
  dbcon <- test_dbcon()
  clear_db(dbcon)

  # First insert spectrum_id_ as character
  df1 <- data.frame(
    spectrum_id_ = c("SP1", "SP2"),
    msLevel = c(2L, 1L),
    precursor_mz = c(300, 400),
    stringsAsFactors = FALSE
  )
  df1$peaks <- list(
    list(mz = c(100, 200), intensity = c(500, 1000)),
    list(mz = c(150, 250), intensity = c(600, 1200))
  )

  .insert_new_backend_mongo(dbcon, df1)

  # Update spectrum_id_ = "SP2"
  df2 <- data.frame(
    spectrum_id_ = "SP2",
    msLevel = 3L,
    precursor_mz = 450,
    stringsAsFactors = FALSE
  )
  df2$peaks <- list(list(mz = c(160, 260), intensity = c(700, 1300)))

  .insert_new_backend_mongo(dbcon, df2)

  #check spectra Data
  scalars <- dbcon$ms_spectrum_coll$find('{}')
  expect_equal(nrow(scalars), 2)
  expect_equal(scalars$msLevel[scalars$spectrum_id_ == "SP2"], 3L)
  expect_equal(scalars$precursor_mz[scalars$spectrum_id_ == "SP2"], 450)


  # Check peaks
  peaks <- dbcon$ms_peaks_coll$find('{}')
  idx <- which(peaks$spectrum_id_ == "SP2")
  expect_true(length(idx) == 1)
  expect_equal(peaks$mz[[idx]], c(160, 260))
  expect_equal(peaks$intensity[[idx]], c(700, 1300))

  # Test .createMsBackendMongoDb with same data
  .createMsBackendMongoDb(dbcon, df1)
  last_peaks <- dbcon$ms_peaks_coll$find('{}')
  expect_true(all(c(100,200) %in% last_peaks$mz[[1]]))

  clear_db(dbcon)
})

test_that("spectrum_id_ auto increments as spec'nb' strings", {

  dbcon <- list(
    ms_spectrum_coll = mongo(collection = "ms_spectrum_coll", db = "test_db", url = "mongodb://localhost"),
    ms_peaks_coll    = mongo(collection = "ms_peaks_coll", db = "test_db", url = "mongodb://localhost")
  )

  dbcon$ms_spectrum_coll$drop()
  dbcon$ms_peaks_coll$drop()

  df1 <- data.frame(
    precursorMz = c(100, 200),
    msLevel = c(2, 2),
    peaks = I(list(
      list(mz = c(50, 60), intensity = c(10, 20)),
      list(mz = c(70, 80), intensity = c(30, 40))
    )),
    stringsAsFactors = FALSE
  )

  .createMsBackendMongoDb(dbcon, df1)

  ids1 <- dbcon$ms_spectrum_coll$distinct("spectrum_id_")
  expect_equal(ids1, c("SP1", "SP2"))

  df2 <- data.frame(
    precursorMz = c(300, 400),
    msLevel = c(2, 2),
    peaks = I(list(
      list(mz = c(90, 100), intensity = c(50, 60)),
      list(mz = c(110, 120), intensity = c(70, 80))
    )),
    stringsAsFactors = FALSE
  )

  .createMsBackendMongoDb(dbcon, df2)

  ids2 <- dbcon$ms_spectrum_coll$distinct("spectrum_id_")
  # Existing spec1, spec2 + new spec3, spec4
  expect_equal(ids2, c("SP1", "SP2", "SP3", "SP4"))

  #Peaks also inserted correctly
  peaks_docs <- dbcon$ms_peaks_coll$find('{}')
  expect_equal(sort(peaks_docs$spectrum_id_), c("SP1","SP2","SP3","SP4"))

  # Clean up
  dbcon$ms_spectrum_coll$drop()
  dbcon$ms_peaks_coll$drop()
})


test_that("combine multiple MsBackendMongoDb backends", {

    dbcon <- list(
        ms_spectrum_coll = mongo(collection = "ms_spectrum_coll",
                                 db = "test_db", url = "mongodb://localhost"),
        ms_peaks_coll = mongo(collection = "ms_peaks_coll",
                              db = "test_db", url = "mongodb://localhost")
    )

    dbcon$ms_spectrum_coll$drop()
    dbcon$ms_peaks_coll$drop()

    ## Backend 1
    df1 <- data.frame(
        spectrum_id_ = c("SP1", "SP2"),
        precursorMz = c(100, 200),
        msLevel = c(2, 2),
        peaks = I(list(
            list(mz=c(10,20), intensity=c(1,2)),
            list(mz=c(30,40), intensity=c(3,4))
        )),
        stringsAsFactors = FALSE
    )
    .createMsBackendMongoDb(dbcon, df1)
    mb1 <- MsBackendMongoDb(dbcon = dbcon, collections = dbcon)
    mb1@spectraIds <- df1$spectrum_id_
    mb1@id_map <- df1$spectrum_id_
    mb1@nspectra <- nrow(df1)

    ## Backend 2
    df2 <- data.frame(
        spectrum_id_ = c("SP3"),
        precursorMz = 300,
        msLevel = 2,
        peaks = I(list(list(mz=c(50,60), intensity=c(5,6)))),
        stringsAsFactors = FALSE
    )
    .createMsBackendMongoDb(dbcon, df2)
    mb2 <- MsBackendMongoDb(dbcon = dbcon, collections = dbcon)
    mb2@spectraIds <- df2$spectrum_id_
    mb2@id_map <- df2$spectrum_id_
    mb2@nspectra <- nrow(df2)

    ## Combine
    mb_combined <- .combine_mongo(list(mb1, mb2))

    ## Tests
    expect_s4_class(mb_combined, "MsBackendMongoDb")
    expect_equal(mb_combined@spectraIds, c("SP1", "SP2", "SP3"))
    expect_equal(mb_combined@id_map, c("SP1", "SP2", "SP3"))
    expect_equal(mb_combined@nspectra, 3)
    expect_equal(mb_combined@.collections, dbcon)

    ## Clean up
    dbcon$ms_spectrum_coll$drop()
    dbcon$ms_peaks_coll$drop()
})

test_that(".reformat_mz_intensity works", {
    a <- data.frame(msLevel = 1L, rtime = c(12.3, 23.2, 13.4))
    res <- .reformat_mz_intensity(a)
    expect_true(is.data.frame(a))
    expect_equal(a$msLevel, res$msLevel)
    expect_equal(a$rtime, res$rtime)
    expect_true(any(colnames(res) == "peaks"))
    expect_equal(res$peaks, list(list(mz = numeric(), intensity = numeric()),
                                 list(mz = numeric(), intensity = numeric()),
                                 list(mz = numeric(), intensity = numeric())))
    a$mz <- list(c(1.2, 2.5, 12.1), c(123.4, 12.4), c(133.1, 343, 2455))
    expect_error(.reformat_mz_intensity(a), "columns need to be provided")
    a$intensity <- list(c(123, 4, 12), c(213, 432), c(234, 123, 543))
    res <- .reformat_mz_intensity(a)
    expect_equal(a$msLevel, res$msLevel)
    expect_equal(a$rtime, res$rtime)
    expect_true(any(colnames(res) == "peaks"))
    expect_equal(res$peaks[[1L]]$mz, a$mz[[1L]])
    expect_equal(res$peaks[[2L]]$mz, a$mz[[2L]])
    expect_equal(res$peaks[[3L]]$mz, a$mz[[3L]])
    expect_equal(res$peaks[[1L]]$intensity, a$intensity[[1L]])
    expect_equal(res$peaks[[2L]]$intensity, a$intensity[[2L]])
    expect_equal(res$peaks[[3L]]$intensity, a$intensity[[3L]])
})

library(testthat)
library(mongolite)

# Helper: Create a test MongoDB connection
setup_test_db <- function() {
  m <- mongo(collection = "gnps_test", db = "test_db", url = "mongodb://localhost")
  m$drop() # Nettoyage avant le test

  # Convertir la liste en data.frame
  test_spectra <- data.frame(
    spectrum_id = c("SP1", "SP2"),
    precursor_mz = c(500.12, 600.34),
    msLevel = c(2L, 2L),
    peaks_json = c("[[123.4, 567], [124.5, 678]]", "[[125.6, 789], [126.7, 890]]"),
    stringsAsFactors = FALSE
  )

  m$insert(test_spectra)
  m
}

# Helper: Clean up after test
teardown_test_db <- function(m) {
  m$drop()
}

# The metadata helper receives a backend, not a MongoDB connection.
# Each test owns its fixture and drops only that temporary collection.
with_gnps_metadata_backend <- function(check) {
  connection <- setup_test_db()
  on.exit(teardown_test_db(connection), add = TRUE)

  sps <- Spectra::Spectra(
    connection,
    source = MsBackendGnpsMongoDb(),
    format = "gnps"
  )

  check(sps)
}

test_that(".gnps_mongo_metadata retrieves selected source fields", {
  with_gnps_metadata_backend(function(sps) {
    backend <- methods::slot(sps, "backend")

    metadata <- .gnps_mongo_metadata(
      backend,
      fields = c("spectrum_id", "precursor_mz", "msLevel")
    )

    expect_true(is.data.frame(metadata))
    expect_equal(nrow(metadata), 2L)
    expect_setequal(metadata$spectrum_id, c("SP1", "SP2"))
    expect_identical(
      as.character(metadata[["_id"]]),
      backend@spectraIds
    )

    index <- match(c("SP1", "SP2"), metadata$spectrum_id)
    expect_equal(metadata$precursor_mz[index], c(500.12, 600.34))
    expect_equal(metadata$msLevel[index], c(2L, 2L))
    expect_false("peaks_json" %in% names(metadata))
  })
})

test_that(".gnps_mongo_metadata projects only requested fields and _id", {
  with_gnps_metadata_backend(function(sps) {
    backend <- methods::slot(sps, "backend")

    metadata <- .gnps_mongo_metadata(
      backend,
      fields = c("spectrum_id", "precursor_mz")
    )

    expect_identical(
      names(metadata),
      c("_id", "spectrum_id", "precursor_mz")
    )
    expect_false("msLevel" %in% names(metadata))
    expect_false("peaks_json" %in% names(metadata))
  })
})

test_that(".gnps_mongo_metadata preserves subset order and repeated spectra", {
  with_gnps_metadata_backend(function(sps) {
    fields <- c("spectrum_id", "precursor_mz")
    original <- .gnps_mongo_metadata(
      methods::slot(sps, "backend"),
      fields = fields
    )

    selection <- c(2L, 1L, 2L)
    backend <- methods::slot(sps[selection], "backend")
    metadata <- .gnps_mongo_metadata(backend, fields = fields)

    expect_equal(nrow(metadata), length(selection))
    expect_identical(
      as.character(metadata[["_id"]]),
      as.character(original[["_id"]])[selection]
    )
    expect_identical(
      metadata$spectrum_id,
      original$spectrum_id[selection]
    )
    expect_equal(
      metadata$precursor_mz,
      original$precursor_mz[selection]
    )

    # A single-spectrum request is represented by backend subsetting.
    one <- .gnps_mongo_metadata(
      methods::slot(sps[1L], "backend"),
      fields = fields
    )
    expect_equal(nrow(one), 1L)
    expect_identical(one$spectrum_id, original$spectrum_id[1L])
  })
})

test_that(".gnps_mongo_metadata handles an empty backend and missing fields", {
  with_gnps_metadata_backend(function(sps) {
    empty <- .gnps_mongo_metadata(
      methods::slot(sps[integer()], "backend"),
      fields = "spectrum_id"
    )
    expect_identical(empty, data.frame())

    metadata <- .gnps_mongo_metadata(
      methods::slot(sps, "backend"),
      fields = c("spectrum_id", "absent_metadata_field")
    )
    expect_identical(
      metadata$absent_metadata_field,
      rep(NA_character_, length(sps))
    )
  })
})

testthat::test_that(".gnps_mongo_peaks retrieves and subsets peaks", {
  m <- setup_test_db()
  on.exit(teardown_test_db(m), add = TRUE)

  backend <- ProtGenerics::backendInitialize(
    MsBackendGnpsMongoDb(),
    dbcon = m,
    format = "gnps"
  )

  # Relier les identifiants SP1/SP2 aux identifiants MongoDB internes.
  metadata <- .gnps_mongo_metadata(
    backend,
    fields = "spectrum_id"
  )

  index <- match(c("SP1", "SP2"), metadata$spectrum_id)
  testthat::expect_false(anyNA(index))

  ordered_backend <- ProtGenerics::extractByIndex(
    backend, index
  )

  # Récupérer tous les pics dans l'ordre SP1, SP2.
  peaks <- .gnps_mongo_peaks(ordered_backend)

  testthat::expect_type(peaks, "list")
  testthat::expect_length(peaks, 2L)
  testthat::expect_identical(
    names(peaks),
    ordered_backend@spectraIds
  )

  testthat::expect_equal(
    unname(peaks[[1L]]),
    unname(cbind(
      mz = c(123.4, 124.5),
      intensity = c(567, 678)
    ))
  )

  testthat::expect_equal(
    unname(peaks[[2L]]),
    unname(cbind(
      mz = c(125.6, 126.7),
      intensity = c(789, 890)
    ))
  )

  testthat::expect_identical(
    colnames(peaks[[1L]]),
    c("mz", "intensity")
  )

  # Remplacement de limit = 0 : backend vide.
  empty_backend <- ProtGenerics::extractByIndex(
    ordered_backend, integer()
  )

  testthat::expect_identical(
    .gnps_mongo_peaks(empty_backend),
    list()
  )

  # Sélection d'un seul spectre.
  one_backend <- ProtGenerics::extractByIndex(
    ordered_backend, 1L
  )

  testthat::expect_equal(
    .gnps_mongo_peaks(one_backend),
    peaks[1L]
  )

  # Préserver l'ordre et les répétitions.
  repeated_backend <- ProtGenerics::extractByIndex(
    ordered_backend, c(2L, 1L, 2L)
  )

  repeated <- .gnps_mongo_peaks(repeated_backend)

  testthat::expect_equal(
    repeated,
    peaks[c(2L, 1L, 2L)]
  )

  # Sélection des colonnes.
  mz_only <- .gnps_mongo_peaks(
    ordered_backend,
    columns = "mz"
  )

  testthat::expect_identical(
    colnames(mz_only[[1L]]),
    "mz"
  )

  testthat::expect_equal(
    mz_only[[1L]],
    peaks[[1L]][, "mz", drop = FALSE]
  )

  testthat::expect_error(
    .gnps_mongo_peaks(
      ordered_backend,
      columns = "invalid"
    ),
    "Peak columns must be"
  )
})


  if (FALSE) {
    # Manual checks using locally imported GNPS and MSnLib collections.
    library(mongolite)

    # GNPS reference library

    # Import command for a JSON array:
    # mongoimport.exe --drop --host localhost --db BD --collection gnps --jsonArray --file "path/ALL_GNPS_cleaned.json"

    gnps <- mongolite::mongo(
      collection = "gnps",
      db = "BD",
      url = "mongodb://localhost"
    )

    sps_gnps <- Spectra::Spectra(
      gnps,
      source = MsBackendGnpsMongoDb(),
      format = "gnps"
    )

    # Retrieve metadata for up to six spectra.
    gnps_subset <- sps_gnps[
      seq_len(min(6L, length(sps_gnps)))
    ]

    gnps_metadata <- Spectra::spectraData(
      gnps_subset,
      columns = c(
        "spectrum_id", "msLevel", "precursorMz",
        "polarity", "inchikey"
      )
    )

    print(gnps_metadata)

    # Retrieve peaks for up to five spectra.
    gnps_peaks <- Spectra::peaksData(
      sps_gnps[seq_len(min(5L, length(sps_gnps)))]
    )

    print(names(gnps_peaks))

    if (length(gnps_peaks))
      print(head(gnps_peaks[[1L]]))

    # Find a known spectrum using its GNPS identifier.
    gnps_ids <- as.character(
      Spectra::spectraData(
        sps_gnps,
        columns = "spectrum_id"
      )$spectrum_id
    )

    index <- which(gnps_ids == "CCMSLIB00000001547")

    if (length(index)) {
      specific_peaks <- Spectra::peaksData(sps_gnps[index])
      print(head(specific_peaks[[1L]]))
    } else {
      message("The requested GNPS spectrum is not in this collection.")
    }

    # Note: the current backend may assign MS2 to missing GNPS levels.
    print(table(
      Spectra::msLevel(sps_gnps),
      useNA = "ifany"
    ))

    # MSnLib reference library

    # Import command for a JSON array:
    # mongoimport.exe --drop --host localhost --db BD_MSnLib_test --collection gnps_MSnLib_test --jsonArray --file "path/20241003_enamdisc_neg_msn.json"

    msn <- mongolite::mongo(
      collection = "gnps_MSnLib_test",
      db = "BD_MSnLib_test",
      url = "mongodb://localhost"
    )

    sps_msn <- Spectra::Spectra(
      msn,
      source = MsBackendGnpsMongoDb(),
      format = "msnlib"
    )

    print(table(
      Spectra::msLevel(sps_msn),
      useNA = "ifany"
    ))

    # Retrieve peaks for up to five spectra.
    msn_peaks <- Spectra::peaksData(
      sps_msn[seq_len(min(5L, length(sps_msn)))]
    )

    print(names(msn_peaks))

    if (length(msn_peaks))
      print(head(msn_peaks[[1L]]))

    # Retrieve metadata and paths for up to six spectra.
    msn_subset <- sps_msn[
      seq_len(min(6L, length(sps_msn)))
    ]

    path_columns <- c(
      "msn_precursor_mzs",
      "msn_collision_energies",
      "msn_fragmentation_methods",
      "msn_isolation_windows"
    )

    msn_metadata <- Spectra::spectraData(
      msn_subset,
      columns = c(
        "spectrum_id", "feature_id", "msLevel",
        "scan_number", "merge_type", path_columns
      )
    )

    print(msn_metadata)

    # Inspect the first available nonempty precursor path.
    path_index <- which(
      lengths(msn_metadata$msn_precursor_mzs) > 0L
    )

    if (length(path_index)) {
      index <- path_index[1L]

      print(lapply(
        msn_metadata[, path_columns, drop = FALSE],
        function(x) x[[index]]
      ))
    }

    # Verify order, repeated spectra, and preserved paths.
    if (length(msn_subset) >= 2L) {
      selection <- c(2L, 1L, 2L)

      repeated <- Spectra::spectraData(
        msn_subset[selection],
        columns = c("spectrum_id", path_columns)
      )

      stopifnot(
        identical(
          repeated$spectrum_id,
          msn_metadata$spectrum_id[selection]
        )
      )

      for (column in path_columns) {
        stopifnot(
          identical(
            repeated[[column]],
            msn_metadata[[column]][selection]
          )
        )
      }

      repeated_peaks <- as.list(
        Spectra::peaksData(msn_subset[selection])
      )

      stopifnot(
        length(repeated_peaks) == length(selection),
        identical(repeated_peaks[[1L]], repeated_peaks[[3L]])
      )
    }

}

test_that(".revmap works", {
    res <- .revmap(.SPECTRA_2_GNPS)
    expect_equal(names(res), unname(.SPECTRA_2_GNPS))
    expect_equal(unname(res), names(.SPECTRA_2_GNPS))
})
