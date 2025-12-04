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
    rm(conns)
})

test_that(".valid_mongocon validates connections correctly", {
    expect_match(.valid_mongocon(NULL), "'dbcon' is NULL or empty")
    expect_match(.valid_mongocon(5),
                 "'dbcon' should be a mongolite::mongo object or a list")

    temp_db <- paste0("test_db_", as.integer(Sys.time()))

    con_wrong <- mongo(collection = "other_coll", db = temp_db,
                       url = "mongodb://localhost")
    res <- .valid_mongocon(list(other_coll = con_wrong))
    expect_match(res, "Missing required collection: compounds_info_coll")

    con_required <- mongo(collection = "compounds_info_coll",
                          db = temp_db, url = "mongodb://localhost")
    expect_null(.valid_mongocon(con_required))

    con_wrong$drop()
    con_required$drop()
    rm(con_wrong)
    rm(con_required)
})

test_that(".dbcon accessor returns correct dbcon", {
    dbcon <- test_dbcon()
    be <- MsBackendMongoDb(dbcon = dbcon, spectraIds = 1L)
    expect_identical(.dbcon(be), dbcon)
    clear_db(dbcon)
    rm(dbcon)
})

test_that(".encode_peaks works with multiple formats", {
    df <- data.frame(mz = c(100, 200), intensity = c(10, 20))
    res <- .encode_peaks(df)
    expect_true(is.list(res))
    expect_equal(names(res), c("mz", "intensity"))
    expect_equal(res$mz, c(100, 200))

    mat <- matrix(c(100, 200, 10,20), ncol = 2)
    res <- .encode_peaks(mat)
    expect_true(is.list(res))
    expect_equal(names(res), c("mz", "intensity"))
    expect_equal(res$mz, c(100, 200))

    lst <- list(mz = c(100, 200), intensity = c(10, 20))
    res <- .encode_peaks(lst)
    expect_true(is.list(res))
    expect_equal(names(res), c("mz", "intensity"))
    expect_equal(res$mz, c(100, 200))

    res <- .encode_peaks(NULL)
    expect_true(is.list(res))
    expect_equal(names(res), c("mz", "intensity"))
    expect_equal(res$mz, numeric())

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

    l <- list(spectrum_id = 1L,
              peaks = list(list(mz = c(100,200), intensity = c(10,20)))
              )
    dbcon$compounds_info_coll$insert(l)

    be <- MsBackendMongoDb(dbcon = dbcon, spectraIds = 1L)

    vars <- MsBackendMongoDb:::.available_peaks_variables_mongo(dbcon)
    expect_equal(vars, c("mz", "intensity"))
})

test_that(".fetch_peaks_data_long_mongo fetches peaks correctly", {
    dbcon <- test_dbcon()
    clear_db(dbcon)

    l <- list(spectrum_id = 1L,
              peaks = list(list(mz = c(100,200), intensity = c(10,20))))
    dbcon$compounds_info_coll$insert(l)

    be <- MsBackendMongoDb(dbcon = dbcon, spectraIds = 1L)

    res <- MsBackendMongoDb:::.fetch_peaks_data_long_mongo(be)

    expect_true(is.matrix(res[[1]]))
    expect_equal(colnames(res[[1]]), c("mz", "intensity"))

    expect_equal(res[[1]][, "mz"], c(100,200))
    expect_equal(res[[1]][, "intensity"], c(10,20))

    clear_db(dbcon)
})

test_that(".insert_spectra_mongo inserts spectra correctly w dynamic fields", {
    dbcon <- test_dbcon()
    clear_db(dbcon)

    ## Create a DataFrame of spectra data
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
        c(30, 40, 44.2)
    )
    spd$intensity <- list(
        c(1, 2),
        c(5, 6, 12)
    )

    sp <- Spectra(spd)

    expect_invisible(MsBackendMongoDb:::.insert_spectra_mongo(dbcon, sp))
    out <- dbcon$compounds_info_coll$find('{}')

    expect_equal(length(out$spectrum_id), 2)
    expect_equal(out$spectrum_id, spd$spectrum_id)
    ## Q1: peaks are stored as "peaks" column?
    expect_true(all(c("spectrum_id", "msLevel", "peaks") %in% names(out)))
    expect_true(all(c("polarity", "compound_id", "precursor_mz",
                      "acquisitionNum", "collision_energy") %in% names(out)))

    ## Check peaks format
    expect_true(all(sapply(out$peaks, function(p) {
        p <- if (is.list(p) && length(p) == 1 && is.list(p[[1]])) p[[1]] else p
        all(c("mz", "intensity") %in% names(p))
    })))

    ## Cleanup
    clear_db(dbcon)
})

test_that(".spectra_data_mongo returns expected structure", {
    ## Setup MongoDB connection
    dbcon <- test_dbcon()
    clear_db(dbcon)

    ## Insert test spectrum
    dbcon$compounds_info_coll$insert(list(
                                  spectrum_id = 1L,
                                  msLevel = 2L,
                                  precursor_mz = 150.0,
                                  peaks = list(list(
                                      mz = c(50, 100),
                                      intensity = c(5, 10)
                                  ))
                              ))

    ## Create backend
    be <- MsBackendMongoDb(dbcon = dbcon, spectraIds = 1L)

    ## Call the function including peaks
    res <- MsBackendMongoDb:::.spectra_data_mongo(
        be, columns = c("spectrum_id", "msLevel", "precursor_mz", "peaks"))

    ## Check structure
    expect_s4_class(res, "DataFrame")
    expect_equal(res$spectrum_id, 1L)
    expect_equal(res$msLevel, 2L)
    expect_equal(res$precursor_mz, 150)

    ## Q1: should the result from .spectra_data_mongo not be the `spectraData()`
    ##     data frame? with a column `$mz` and `$intensity` instead of a
    ##     `$peaks` column?
    ## Check peaks column
    expect_s4_class(res$peaks, "List")
    expect_true(is.list(res$peaks[[1]]))
    expect_named(res$peaks[[1]], c("mz", "intensity"))
    expect_equal(res$peaks[[1]]$mz, c(50, 100))
    expect_equal(res$peaks[[1]]$intensity, c(5, 10))


    ## Clean up
    clear_db(dbcon)
})

test_that(".fetch_spectra_data_mongo returns expected structure", {

    ## Setup: create test MongoDB connection
    dbcon <- test_dbcon()
    clear_db(dbcon)

    ## Insert test document
    dbcon$compounds_info_coll$insert(list(
                                  spectrum_id = 1L,
                                  msLevel = 2L,
                                  precursor_mz = 150.0,
                                  peaks = list(list(
                                      mz = c(50, 100),
                                      intensity = c(5, 10)
                                  ))
                              ))

    ## Create backend
    be <- MsBackendMongoDb(dbcon = dbcon, spectraIds = 1L)

    ## Call the function without peaks
    res <- MsBackendMongoDb:::.fetch_spectra_data_mongo(
                                  be, columns = c("spectrum_id", "msLevel", "precursor_mz"))

    ## Check that result is a DataFrame
    expect_s4_class(res, "DataFrame")

    ## Check scalar values
    expect_equal(res$spectrum_id, 1L)
    expect_equal(res$msLevel, 2L)
    expect_equal(res$precursor_mz, 150)

    ## Ensure peaks column is not present
    expect_false("peaks" %in% colnames(res))

    ## Clean up
    clear_db(dbcon)
})

test_that(".insert_backend_mongo and .createMsBackendMongoDB work", {
    dbcon <- test_dbcon()
    clear_db(dbcon)

    df <- data.frame(
        spectrum_id = 1:2, # Q2: this column should be created (or replaced) by .createMsBackendMongoDb
        msLevel = c(1L, 2L),
        polarity = c(-1, 1),  # extra field
        rtime = c(123.4, 567.8), # another extra field
        stringsAsFactors = FALSE
    )

    ## Add peaks as list-column
    df$peaks <- I(list(
        list(mz = c(10, 20), intensity = c(1, 2)),
        list(mz = c(30), intensity = c(5))
    ))

    res <- MsBackendMongoDb:::.createMsBackendMongoDb(dbcon, df)
    expect_type(res, "list")
    expect_equal(res$spectrum_id, max(df$spectrum_id))

    out <- dbcon$compounds_info_coll$find('{}')
    ## Cleanup
    clear_db(dbcon)
})

test_that(".combine_mongo merges multiple backends", {
    dbcon <- test_dbcon()
    clear_db(dbcon)

    for (i in 1:3) dbcon$compounds_info_coll$insert(list(spectrum_id=i, peaks=list(list(mz=numeric(), intensity=numeric()))))

    b1 <- MsBackendMongoDb(dbcon=dbcon, spectraIds=1)
    b2 <- MsBackendMongoDb(dbcon=dbcon, spectraIds=2)
    b3 <- MsBackendMongoDb(dbcon=dbcon, spectraIds=3)

    combined <- MsBackendMongoDb:::.combine_mongo(list(b1, b2, b3))
    expect_s4_class(combined, "MsBackendMongoDb")
    expect_equal(combined@spectraIds, 1:3)

    clear_db(dbcon)
})

test_that(".create_indices_mongo works", {
    dbcon <- test_dbcon()
    expect_true(MsBackendMongoDb:::.create_indices_mongo(dbcon))
})
