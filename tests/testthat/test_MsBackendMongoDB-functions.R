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

    ## Check that the peaks are correct
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

    expect_equal(nrow(spdata), 2)
    expect_equal(spdata$spectrum_id_, c("SP1", "SP2"))
    expect_equal(spdata$msLevel, c(2L, 1L))

    ## Peaks are returned as NumericList
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
    ## Existing spec1, spec2 + new spec3, spec4
    expect_equal(ids2, c("SP1", "SP2", "SP3", "SP4"))

    ## Peaks also inserted correctly
    peaks_docs <- dbcon$ms_peaks_coll$find('{}')
    expect_equal(sort(peaks_docs$spectrum_id_), c("SP1", "SP2", "SP3", "SP4"))

    ## Clean up
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

test_that("gnps_fetch_spectra_data works as expected", {
  m <- setup_test_db()
  on.exit(teardown_test_db(m))
  
  # Test 1: Fetch all spectra
  spectra <- gnps_fetch_spectra_data(m)
  expect_is(spectra, "data.frame")
  expect_equal(nrow(spectra), 2)
  expect_true(all(c("SP1", "SP2") %in% spectra$spectrum_id))
  
  # Test 2: Fetch with limit
  spectra_limited <- gnps_fetch_spectra_data(m, limit = 1)
  expect_equal(nrow(spectra_limited), 1)
  
  # Test 3: Fetch specific fields
  spectra_fields <- gnps_fetch_spectra_data(m, fields = c("spectrum_id", "precursor_mz"))
  expect_true(all(c("spectrum_id", "precursor_mz") %in% colnames(spectra_fields)))
  expect_false("msLevel" %in% colnames(spectra_fields))
  
  # Test 4: Fetch with query
  spectra_query <- gnps_fetch_spectra_data(m, query = list(msLevel = 2L))
  expect_equal(nrow(spectra_query), 2)
})

test_that("gnps_fetch_peaks_data works correctly", {
  m <- setup_test_db()
  on.exit(teardown_test_db(m))
  
  # Test 1: Fetch with limit = 0
  no_peaks <- gnps_fetch_peaks_data(m, limit = 0, id_field = "spectrum_id")
  expect_is(no_peaks, "list")
  expect_equal(length(no_peaks), 0)
  
  # Test 2: Fetch with NULL query and limit
  all_peaks <- gnps_fetch_peaks_data(m, query = NULL, id_field = "spectrum_id")
  expect_is(all_peaks, "list")
  expect_equal(length(all_peaks), 2)  # On s'attend à 2 spectres, pas 3
  
  # Test 3: Fetch with limit = 2 (more than 0)
  limited_peaks <- gnps_fetch_peaks_data(m, limit = 2, id_field = "spectrum_id")
  expect_is(limited_peaks, "list")
  expect_equal(length(limited_peaks), 2)
  
  # Test 4: Check the content of the limited peaks
  expect_true(all(c("SP1", "SP2") %in% names(limited_peaks)))
  expect_equal(limited_peaks[["SP1"]][1, "mz"], 123.4, check.attributes = FALSE)
  expect_equal(limited_peaks[["SP1"]][1, "intensity"], 567, check.attributes = FALSE)
  
  # Test 5: Check the content of the second spectrum
  expect_equal(limited_peaks[["SP2"]][1, "mz"], 125.6, check.attributes = FALSE)
  expect_equal(limited_peaks[["SP2"]][1, "intensity"], 789, check.attributes = FALSE)
})


# Manual checks using locally imported GNPS and MSnLib collections.
library(mongolite)

# GNPS reference library.
# Import command for a JSON array:
# mongoimport.exe --drop --host localhost --db BD --collection gnps --jsonArray --file "path\ALL_GNPS_cleaned.json"

gnps <- mongo(
  collection = "gnps",
  db = "BD",
  url = "mongodb://localhost"
)

# Retrieve selected metadata.
gnps_metadata <- gnps_fetch_spectra_data(
  gnps,
  fields = c("spectrum_id", "Precursor_MZ"),
  limit = 6
)
gnps_metadata

# Retrieve peaks for five spectra.
gnps_peaks <- gnps_fetch_peaks_data(
  gnps,
  limit = 5,
  id_field = "spectrum_id"
)
names(gnps_peaks)

if (length(gnps_peaks))
  print(head(gnps_peaks[[1L]]))

# Retrieve peaks for a known GNPS spectrum.
specific_peaks <- gnps_fetch_peaks_data(
  gnps,
  query = list(spectrum_id = "CCMSLIB00000001547"),
  id_field = "spectrum_id"
)

if (length(specific_peaks))
  print(head(specific_peaks[[1L]]))

# Access the collection through Spectra.
sps_gnps <- Spectra::Spectra(
  gnps,
  source = MsBackendGnpsMongoDb(),
  format = "gnps"
)

table(Spectra::msLevel(sps_gnps), useNA = "ifany")

gnps_subset <- sps_gnps[1:6]
stopifnot(length(gnps_subset) == 6L)

Spectra::spectraData(
  gnps_subset,
  columns = c(
    "spectrum_id", "msLevel", "precursorMz", "polarity", "inchikey"
  )
)

head(Spectra::peaksData(gnps_subset)[[1L]])


# MSnLib reference library containing MS2–MS5 spectra.
# Import command for a JSON array:
# mongoimport.exe --drop --host localhost --db BD_MSnLib_test --collection gnps_MSnLib_test --jsonArray --file "path\20241003_enamdisc_neg_msn.json"

msn <- mongo(
  collection = "gnps_MSnLib_test",
  db = "BD_MSnLib_test",
  url = "mongodb://localhost"
)

# Retrieve peaks for five spectra using MongoDB ObjectIds.
msn_peaks <- gnps_fetch_peaks_data(
  msn,
  limit = 5,
  id_field = "_id"
)
names(msn_peaks)

if (length(msn_peaks))
  print(head(msn_peaks[[1L]]))

# Access MSn spectra and fragmentation paths through Spectra.
sps_msn <- Spectra::Spectra(
  msn,
  source = MsBackendGnpsMongoDb(),
  format = "msnlib"
)

table(Spectra::msLevel(sps_msn), useNA = "ifany")

msn_subset <- sps_msn[1:6]
stopifnot(length(msn_subset) == 6L)

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
msn_metadata

head(Spectra::peaksData(msn_subset)[[1L]])

# Inspect one spectrum's complete fragmentation path.
lapply(msn_metadata[, path_columns, drop = FALSE], function(x) x[[5L]])

# Verify that subsetting preserves every path and repeated spectra.
selection <- c(5L, 6L, 5L)

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