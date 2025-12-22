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
      spectrum_id_ = c("1", "2"),
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
  expect_equal(length(backend), 2)
  expect_equal(backend@spectraVariables,
               c("spectrum_id_", "msLevel", "mz", "intensity"))
  expect_equal(rtime(backend), c(NA_real_, NA_real_))
  expect_equal(msLevel(backend), c(1L, 2L))
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
  expect_equal(backend@spectraIds, c("1", "2"))
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

  ## Empty peaks data
  clear_db(dbcon)
  dbcon$ms_spectrum_coll$insert(
    data.frame(spectrum_id_ = c("1", "2"), stringsAsFactors = FALSE)
  )
  be <- backendInitialize(MsBackendMongoDb(), dbcon = dbcon)
  res <- peaksData(be)
  expect_length(res, 2)
  expect_type(res, "list")
  expect_equal(names(res), be@spectraIds)
  expect_true(is.matrix(res[[1L]]))
  expect_equal(colnames(res[[1L]]), c("mz", "intensity"))
  expect_equal(nrow(res[[1L]]), 0L)
  expect_true(is.matrix(res[[2L]]))
  expect_equal(colnames(res[[2L]]), c("mz", "intensity"))
  expect_equal(nrow(res[[2L]]), 0L)
  clear_db(dbcon)
})

test_that("peaksVariables works", {
  dbcon <- test_dbcon()
  clear_db(dbcon)

  be <- MsBackendMongoDb()
  expect_equal(peaksVariables(be), character())

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

  be <- backendInitialize(be, dbcon = dbcon)
  res <- peaksVariables(be)
  expect_equal(res, c("mz", "intensity"))

  clear_db(dbcon)
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

  be <- MsBackendMongoDb()
  res <- mz(be)
  expect_s4_class(res, "NumericList")
  expect_length(res, 0)
  res <- intensity(be)
  expect_s4_class(res, "NumericList")
  expect_length(res, 0)

  backend <- backendInitialize(new("MsBackendMongoDb"), dbcon = dbcon)

  mz_vals <- mz(backend)
  int_vals <- intensity(backend)

  expect_s4_class(mz_vals, "NumericList")
  expect_s4_class(int_vals, "NumericList")

  expect_equal(as.numeric(mz_vals[[1]]), c(50, 100))
  expect_equal(as.numeric(mz_vals[[2]]), c(75, 125))

  expect_equal(as.numeric(int_vals[[1]]), c(5, 10))
  expect_equal(as.numeric(int_vals[[2]]), c(7, 14))
  clear_db(dbcon)
})

test_that("mz<- throws an error", {
  be <- MsBackendMongoDb()
  expect_error(mz(be) <- 3, "Can not replace")
})

test_that("intensity<- throws an error", {
  be <- MsBackendMongoDb()
  expect_error(intensity(be) <- 3, "Can not replace")
})

test_that("spectraNames works", {
  dbcon <- test_dbcon()
  clear_db(dbcon)

  dbcon$ms_spectrum_coll$insert(
    data.frame(spectrum_id_ = c("10", "20", "30"), stringsAsFactors = FALSE)
  )

  be <- backendInitialize(MsBackendMongoDb(), dbcon = dbcon)
  res <- spectraNames(be)

  expect_equal(res, c("10", "20", "30"))
  expect_equal(res, be@spectraIds)

  clear_db(dbcon)
})

test_that("spectraNames<- throws an error", {
  dbcon <- test_dbcon()
  clear_db(dbcon)

  dbcon$ms_spectrum_coll$insert(
    data.frame(spectrum_id_ = c("10", "20", "30"), stringsAsFactors = FALSE)
  )

  be <- backendInitialize(MsBackendMongoDb(), dbcon = dbcon)
  expect_error(spectraNames(be) <- c(1, 34, 2), "Replacing")

  clear_db(dbcon)
})

test_that("spectraData() returns merged scalar and peaks data", {
  dbcon <- test_dbcon()
  clear_db(dbcon)

  dbcon$ms_spectrum_coll$insert(
    data.frame(spectrum_id_ = c("1", "2"),
               precursorMz = c(123.23, 1322.4),
               msLevel = c(1L, 1L),
               other_col = c("a", "b")
    ))

  dbcon$ms_peaks_coll$insert(
    data.frame(
      spectrum_id_ = c("1", "2"),
      mz = I(list(c(50, 100), c(75, 125))),
      intensity = I(list(c(5, 10), c(7, 14))),
      stringsAsFactors = FALSE
    )
  )

  be <- backendInitialize(MsBackendMongoDb(), dbcon = dbcon)

  d <- spectraData(be)
  expect_true(all(c("spectrum_id_", "precursorMz", "msLevel", "other_col",
                    "mz", "intensity") %in% colnames(d)))
  expect_true(all(names(coreSpectraVariables()) %in% colnames(d)))
  expect_equal(d$msLevel, c(1L, 1L))
  expect_equal(d$precursorMz, c(123.23, 1322.4))
  expect_equal(d$other_col, c("a", "b"))
  expect_equal(d$mz[[1L]], c(50, 100))
  expect_equal(d$mz[[2L]], c(75, 125))
  expect_equal(d$intensity[[1L]], c(5, 10))
  expect_equal(d$intensity[[2L]], c(7, 14))

  # spectraData with selected columns
  d <- spectraData(be, c("other_col", "mz", "msLevel"))
  expect_s4_class(d, "DataFrame")
  expect_equal(colnames(d), c("other_col", "mz", "msLevel"))
  expect_equal(d$other_col, c("a", "b"))
  expect_equal(d$msLevel, c(1L, 1L))
  expect_equal(d$mz[[1L]], c(50, 100))
  expect_equal(d$mz[[2L]], c(75, 125))

  ## spectraData with only peaks variables
  d <- spectraData(be, c("intensity"))
  expect_s4_class(d, "DataFrame")
  expect_equal(colnames(d), c("intensity"))
  expect_equal(d$intensity[[1L]], c(5, 10))
  expect_equal(d$intensity[[2L]], c(7, 14))

  ## spectraData with only spectra variables
  d <- spectraData(be, c("msLevel", "rtime"))
  expect_s4_class(d, "DataFrame")
  expect_equal(colnames(d), c("msLevel", "rtime"))
  expect_equal(d$msLevel, c(1L, 1L))
  expect_equal(d$rtime, c(NA_real_, NA_real_))

  ## cache: only non-present variables
  d <- spectraData(be, c("rtime", "polarity"))
  expect_s4_class(d, "DataFrame")
  expect_equal(colnames(d), c("rtime", "polarity"))
  expect_identical(d$rtime, c(NA_real_, NA_real_))
  expect_identical(d$polarity, c(NA_integer_, NA_integer_))

  ## cache: cache rtime
  be$rtime <- c(123.4, 1234.5)
  d <- spectraData(be, c("rtime", "polarity"))
  expect_s4_class(d, "DataFrame")
  expect_equal(colnames(d), c("rtime", "polarity"))
  expect_identical(d$rtime, c(123.4, 1234.5))
  expect_identical(d$polarity, c(NA_integer_, NA_integer_))

  ## cache: overwriting existing variable
  be$msLevel <- 2L
  d <- spectraData(be, c("rtime", "msLevel"))
  expect_s4_class(d, "DataFrame")
  expect_equal(colnames(d), c("rtime", "msLevel"))
  expect_identical(d$rtime, c(123.4, 1234.5))
  expect_identical(d$msLevel, c(2L, 2L))

  clear_db(dbcon)
})

test_that("subsetting extracts the correct spectra", {
  dbcon <- test_dbcon()
  clear_db(dbcon)

  dbcon$ms_spectrum_coll$insert(
    data.frame(spectrum_id_ = c("10", "20", "30"), stringsAsFactors = FALSE)
  )

  backend <- backendInitialize(new("MsBackendMongoDb"), dbcon = dbcon)

  # subset second element
  backend2 <- backend[2]

  expect_equal(backend2@spectraIds, "20")
  expect_equal(backend2@id_map, "20")
  expect_equal(length(backend2), 1L)
  clear_db(dbcon)
})

test_that("extractByIndex works", {
  dbcon <- test_dbcon()
  clear_db(dbcon)

  # Insert metadata as character
  dbcon$ms_spectrum_coll$insert(
    data.frame(spectrum_id_ = c("1", "2", "3"), msLevel = 2L, rtime =c(1.1,2,3))
  )
  # Insert peaks as character
  dbcon$ms_peaks_coll$insert(
    data.frame(
      spectrum_id_ = c("1", "2", "3"),
      mz = I(list(c(100, 200, 201.3), c(150, 250), c(12.1, 14, 15.4, 19))),
      intensity = I(list(c(10, 20, 39), c(15, 25), c(120, 140, 150, 190)))
    )
  )

  be <- backendInitialize(MsBackendMongoDb(), dbcon = dbcon)
  expect_equal(be@spectraIds, c("1", "2", "3"))

  be_sub <- be[2]
  expect_equal(be_sub@spectraIds, "2")
  expect_equal(rtime(be_sub), rtime(be)[2L])
  expect_equal(mz(be_sub), mz(be)[2L])
  expect_equal(intensity(be_sub), intensity(be)[2L])
  expect_equal(peaksData(be_sub), peaksData(be)[2L])

  ## arbitrary order
  be_sub <- be[c(3, 1)]
  expect_equal(be_sub@spectraIds, c("3", "1"))
  expect_equal(rtime(be_sub), rtime(be)[c(3, 1)])
  expect_equal(mz(be_sub), mz(be)[c(3, 1)])
  expect_equal(intensity(be_sub), intensity(be)[c(3, 1)])
  expect_equal(peaksData(be_sub), peaksData(be)[c(3, 1)])

  ## duplicated values
  be_sub <- be[c(3, 1, 3)]
  expect_equal(be_sub@spectraIds, c("3", "1", "3"))
  expect_equal(rtime(be_sub), rtime(be)[c(3, 1, 3)])
  expect_equal(mz(be_sub), mz(be)[c(3, 1, 3)])
  expect_equal(intensity(be_sub), intensity(be)[c(3, 1, 3)])
  expect_equal(peaksData(be_sub), peaksData(be)[c(3, 1, 3)])

  clear_db(dbcon)
})

test_that("subsetting with numeric, logical, and character indices works", {
  dbcon <- test_dbcon()
  clear_db(dbcon)

  # Insert spectra metadata
  dbcon$ms_spectrum_coll$insert(
    data.frame(
      spectrum_id_ = c("spec1", "spec2", "spec3"),
      precursorMz = c(150, 160, 170),
      msLevel = c(2, 2, 2),
      rtime = c(412, 12.23, 4)
    )
  )

  # Insert peaks
  dbcon$ms_peaks_coll$insert(
    data.frame(
      spectrum_id_ = c("spec1", "spec2", "spec3"),
      mz          = I(list(c(100, 200), c(150, 250), c(175, 275))),
      intensity   = I(list(c(10, 20), c(15, 25), c(17, 27)))
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

  spdata_num <- spectraData(be_num, columns = c("spectrum_id_", "precursorMz",
                                                "mz", "intensity", "rtime"))
  expect_equal(nrow(spdata_num), 2)
  expect_equal(spdata_num$spectrum_id_, c("spec1", "spec2"))
  expect_equal(spdata_num$rtime, be$rtime[1:2])

  # Logical subsetting
  be_log <- be[c(TRUE, FALSE, TRUE)]
  expect_equal(be_log@spectraIds, c("spec1", "spec3"))

  peaks_log <- peaksData(be_log)
  expect_equal(peaks_log[[1]][, "mz"], c(100, 200))
  expect_equal(peaks_log[[2]][, "mz"], c(175, 275))

  spdata_log <- spectraData(be_log, columns = c("spectrum_id_", "precursorMz",
                                                "mz", "intensity", "rtime"))
  expect_equal(spdata_log$spectrum_id_, c("spec1", "spec3"))
  expect_equal(spdata_log$rtime, be$rtime[c(1, 3)])

  # Character subsetting
  be_char <- be[c("spec2")]
  expect_equal(be_char@spectraIds, "spec2")

  peaks_char <- peaksData(be_char)
  expect_equal(peaks_char[[1]][, "mz"], c(150, 250))
  expect_equal(peaks_char[[1]][, "intensity"], c(15, 25))

  spdata_char <- spectraData(be_char, columns = c("spectrum_id_", "precursorMz",
                                                  "mz", "intensity", "rtime"))
  expect_equal(nrow(spdata_char), 1)
  expect_equal(spdata_char$spectrum_id_, "spec2")
  expect_s4_class(spdata_char$mz, "NumericList")
  expect_equal(as.numeric(spdata_char$mz[[1]]), c(150, 250))
  expect_equal(as.numeric(spdata_char$intensity[[1]]), c(15, 25))
  expect_equal(spdata_char$rtime, be$rtime[2])

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

test_that("backendInitialize with data provided works", {
    df <- data.frame(msLevel = 1L, rtime = c(12.3, 23.4, 321.1),
                     centroided = TRUE)
    df$mz <- list(c(12.2, 14.4, 134.1),
                  c(131.1, 432.1),
                  c(46.2, 122.1, 143.4, 155.1))
    df$intensity <- list(c(12, 14, 155),
                         c(124.3, 151),
                         c(43, 155.1, 532, 12))
    dbcon <- test_dbcon()
    clear_db(dbcon)

    be <- backendInitialize(MsBackendMongoDb(), dbcon = dbcon, data = df)
    expect_equal(length(be), nrow(df))
    expect_true(all(be@spectraVariables %in%
                    c("spectrum_id_", "msLevel", "rtime", "centroided",
                      "mz", "intensity")))
    sd <- spectraData(be)
    expect_equal(sd$rtime, df$rtime)
    expect_equal(sd$msLevel, df$msLevel)
    expect_equal(sd$centroided, df$centroided)

    expect_equal(df$rtime, rtime(be))
    expect_equal(df$msLevel, msLevel(be))
    expect_equal(df$centroided, centroided(be))
    expect_equal(df$mz, unname(as.list(mz(be))))
    expect_equal(df$intensity, unname(as.list(intensity(be))))

    clear_db(dbcon)
})
