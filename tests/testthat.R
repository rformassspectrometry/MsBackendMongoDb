## Test the MsBackendMongoDb backend
library(testthat)
library(MsBackendMongoDb)
library(mongolite)
library(Spectra)
library(msdata)

#' [TODO] we can run the unit tests only if a mongodb server is running on the
#' system. We should check how to best tackle that. Maybe looking at the unit
#' tests of the mongolite package?

test_check("MsBackendMongoDb")

#' 2025-12-16
#'
#' 1) [OK]: new format to store metadata and peaks data separately
#'
#' 2) [Q?]: what is @id_map slot? is this redundant with @spectraIds
#'
#' 3) [TODO@jo]: create a class union of NULL and mongo/list instead of @dbcon
#'    being ANY
#'
#' 4) [Q?]: are peaks stored as:
#'    list(
#'         list(mz = numeric(), intensity = numeric()),
#'         list(mz = numeric(), intensity = numeric())
#'    )
#'
#' 5) [TODO@jo]: fix `backendInitialize()`: fixed spectra variables, create
#'    @localData .
#'
#' TODO: figure out why backendInitialize with data does not work - returns an
#' empty backend. Check first how the unit test with `backendInitialize()` works

#' Questions - look for Q<index> for reference/mention in the code.
#'
#' 1) the format in which data is stored is a bit unclear:
#'    - `.insert_spectra_mongo()` stores the data as a `data.frame`? all as a
#'      single element? and where are the peaks stored? as $mz, $intensity
#'      columns? seems as "column" $peaks which is a `list` of matrices... is
#'      that a good idea? performance wise for MongoDB?
#'    - `.fetch_peaks_data_long_mongo()` expects the data as a `list`?
#'    - the m/z and intensity values are stored as `$peaks` list? Look for "Q1"
#'      in test_MsBackendMongoDB-functions.R
#'
#' 2) suggestion: .createMsBackendMongoDb adds (or replaces) a column
#'    `"spectrum_id"` with an incremental integer. I would not require this
#'    column to be present in the submitted `data.frame`. If the column is
#'    already present, it's content should be replaced with an integer
#'    `1:nrow(df)`. Look for "Q2" in test_MsBackendMongoDb-functions.R
#'
#' 3) suggestion: use a different name for the "collection"; maybe
#'    "ms_spectrum_coll"; would it make sense to have a separate collection
#'    for the spectra metadata and the MS peaks?
#'
#' 4) the constructor functions should retrieve the `spectraIds` from the
#'    database, they should not be provided from the user. Actually - need
#'    to check. The `spectraIds` should be initialized and retrieved by
#'    `backendInitialize()`.
#'
#' 5) Empty `MsBackendMongoDb()` should not throw an error - thus we should
#'    support `@dbcon` to be `NULL`. See "Q5" in test_MsBackendMongoDB.R
#'
#' 6) `spectraData()` should also return the m/z and intensity values, as a
#'    `NumericList` in columns `"mz"` and `"intensity"`. See "Q6" in
#'    test_MsBackendMongoDB.R
#'
#' 7) Subsetting with `[` should also subset the database/collection, not just
#'    the `@spectraIds`; question/discussion: should we use the same approach
#'    as with MsBackendSql, i.e., use the `@spectraIds` in the query to retrieve
#'    the data from the database, or should we directly change the content of
#'    the database? See "Q7" in test_MsBackendMongoDB.R
#'
#' 8) `dataStorage()` must return a `character` vector with the same length than
#'    there are spectra in the backend. Also, if the backend is empty (no
#'    spectra) present, it should return `character()`. See "Q8" in
#'    test_MsBackendMongoDB.R
#'
#' 9) `reset()` should *restore* the backend to its original state. I think the
#'    problem at the moment is that `@dbcon` is not the `mongo` connection, but
#'    a `list`, with the mongo collection inside. So, maybe the `if ()` in
#'    the `reset` method (MsBackendMongoDB.R line 357) should be either
#'    `if (length(object@dbcon))` or specifically check
#'    `if (inherits(object@dbcon$compounds_info_coll, "mongo"))`. See "Q9" in
#'    test_MsBackendMongoDB.R
