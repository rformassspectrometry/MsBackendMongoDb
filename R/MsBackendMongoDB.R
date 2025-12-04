#' @title `Spectra` MS backend storing data in a MongoDb database
#'
#' @aliases MsBackendMongoDb-class
#' @aliases backendInitialize,MsBackendMongoDb-method
#' @aliases dataStorage,MsBackendMongoDb-method
#' @aliases peaksData,MsBackendMongoDb-method
#' @aliases peaksVariables,MsBackendMongoDb-method
#' @aliases intensity,MsBackendMongoDb-method
#' @aliases intensity<-,MsBackendMongoDb-method
#' @aliases mz,MsBackendMongoDb-method
#' @aliases mz<-,MsBackendMongoDb-method
#' @aliases spectraData,MsBackendMongoDb-method
#' @aliases reset,MsBackendMongoDb-method
#' @aliases spectraNames,MsBackendMongoDb-method
#' @aliases spectraNames<-,MsBackendMongoDb-method
#' @aliases [,MsBackendMongoDb-method
#' @aliases extractByIndex,MsBackendMongoDb-method
#'
#' @description
#'
#' `MsBackendMongoDb` is an implementation of the [Spectra::MsBackend()] class
#' for [Spectra::Spectra()] objects, storing and retrieving MS data from a
#' MongoDb database.
#'
#' This backend allows read-only access to spectra and peaks stored in MongoDb,
#' but supports temporary modifications of spectra variables via caching.
#'
#' @note
#'
#' `MsBackendMongoDb` keeps a connection to a MongoDb collection(s) and fetches
#' spectra and peaks data on demand. By storing only the primary keys (`spectrum_id`)
#' in memory, it ensures minimal memory usage, while peaks and metadata are fetched
#' dynamically.
#'
#' The backend inherits from [Spectra::MsBackendCached()] and supports temporary
#' modification of spectra variables via the `$<-` operator. Original data in the
#' database cannot be changed (intensities and m/z values are read-only).
#'
#' @section Creation of backend objects:
#'
#' New backend objects can be created using:
#'
#' ```r
#' backend <- MsBackendMongoDb(dbcon = list(compounds_info_coll = mongo_connection))
#' backend <- backendInitialize(backend, dbcon = backend@dbcon)
#' ```
#'
#' - `dbcon`: A list of MongoDb collections (e.g. from `mongolite::mongo()`).
#' - `backendInitialize()`: populates the object with spectrum IDs and caches
#'   metadata. If `data` is provided, it can insert data into the collection.
#'
#' @section Accessing data:
#'
#' - `peaksData(object, columns = c("mz", "intensity"))`: returns a `list` of
#'   matrices containing the peak data for each spectrum.
#' - `peaksVariables(object)`: returns available columns for `peaksData`.
#' - `intensity(object)`, `mz(object)`: return `SimpleList` objects with numeric
#'   intensity or m/z values for each spectrum.
#' - `spectraData(object, columns)`: returns a `DataFrame` with requested spectrum
#'   metadata columns.
#' - `spectraNames(object)`: returns `spectrum_id` values as character.
#'
#' @section Read-only data:
#'
#' - `intensity<-` and `mz<-` are **not supported**.
#' - `spectraNames<-` is **not supported**.
#'
#' @section Subsetting and extraction:
#'
#' - `[i]` and `extractByIndex(object, i)` subset the backend by spectrum indices.
#'   Original data in the database is never changed.
#' - `reset(object)` restores the backend to its original state, removing
#'   subsetting and cached variables.
#'
#' @section Miscellaneous:
#'
#' - `dataStorage(object)` returns a string describing the MongoDb collections
#'   used by the backend.
#'
#' @section Implementation notes:
#'
#' Internally, the backend stores:
#' - `spectraIds`: integer vector of spectrum IDs
#' - `dbcon`: MongoDb connection(s)
#' - `.collections`: list of collection names
#' - `peak_fun`: function used to fetch peak data
#' - `localData`: cached spectrum variables
#' - `nspectra`: total number of spectra
#'
#' Data access functions use `peak_fun` to fetch peaks on demand.
#'
#' @param dbcon `List()` of MongoDb collections, e.g., `
#'       list(compounds_info_coll = mongo_conn)`
#' @param object A `MsBackendMongoDb` instance
#' @param columns `Character()` vector of columns to fetch for `peaksData` or
#'       `spectraData`
#' @param i Indices to subset the backend
#' @param value Replacement values for setter functions
#' @param name Name of the spectra variable to modify
#' @param data Optional `DataFrame` with data to insert into MongoDb
#' @param drop `Logical()`, ignored for subsetting
#' @param ... Additional arguments passed internally
#'
#' @return Depends on the method. See individual function documentation.
#'
#' @importClassesFrom DBI DBIConnection
#' @importClassesFrom S4Vectors DataFrame
#' @importClassesFrom Spectra MsBackendCached
#' @import mongolite
#' @import S4Vectors
#' @import Spectra
#'
#' @author Ahlam Mentag
#'
#' @md
#'
#' @name MsBackendMongoDb
#'
#' @exportClass MsBackendMongoDb
NULL

setClass(
  "MsBackendMongoDb",
  contains = "MsBackendCached",
  slots = c(
    dbcon = "ANY",
    spectraIds = "integer",
    .collections = "list",
    peak_fun = "function",
    localData = "data.frame",
    nspectra = "integer"
  ),
  prototype = prototype(
    dbcon = NULL,
    spectraIds = integer(),
    .collections = list(),
    peak_fun = .fetch_peaks_data_long_mongo,
    localData = data.frame(),
    nspectra = 0L,
    readonly = TRUE,
    version = "0.2"
  )
)


#' @importFrom methods .valueClassTest is new validObject
#' @noRd
setValidity("MsBackendMongoDb", function(object) {

  # Check dbcon
  res <- .valid_mongocon(object@dbcon)
  if (!is.null(res)) return(res)

  # Check spectraIds vs nspectra
  if (length(object@spectraIds) != object@nspectra)
    return("Number of spectraIds does not match nspectra")

  # Check localData
  if (nrow(object@localData) != 0L && nrow(object@localData) != object@nspectra)
    return("Number of rows in local data and number of spectra don't match")

  TRUE
})



#' @importMethodsFrom Spectra show
#' @exportMethod show
#' @rdname MsBackendMongoDb
setMethod("show", "MsBackendMongoDb", function(object) {
  callNextMethod()
  if (!is.null(.dbcon(object))) {
    cat(
      "MongoDb collections: ",
      paste(names(object@.collections), collapse = ", "),
      "\n"
    )
  }
})


#' @exportMethod backendInitialize
#' @importMethodsFrom ProtGenerics backendInitialize
#' @importFrom DBI dbGetQuery
#' @rdname MsBackendMongoDb
setMethod(
  "backendInitialize", "MsBackendMongoDb",
  function(object, dbcon, data, ...) {
    if (missing(dbcon))
      stop("Parameter 'dbcon' is required for 'MsBackendMongoDb'")

    # Insert data if provided
    if (!missing(data)) {
      .createMsBackendMongoDb(dbcon, x = data)
    }

    # Validate MongoDb connection
    msg <- .valid_mongocon(dbcon)
    if (length(msg)) stop(msg)

    # Store collection names
    object@.collections <- list(compounds_info_coll = "compounds_info_coll")

    # Fetch spectrum IDs
    spectrum_ids <- as.integer(
      dbcon[["compounds_info_coll"]]$find(
        '{}',
        fields = '{"spectrum_id": 1}',
        sort = '{"spectrum_id": 1}'
      )$spectrum_id
    )

    # Assign spectrum IDs and nspectra BEFORE callNextMethod
    object@spectraIds <- spectrum_ids
    object@nspectra <- length(spectrum_ids)
    object@dbcon <- dbcon
    object@peak_fun <- .fetch_peaks_data_long_mongo

    # Ensure localData has correct number of rows
    object@localData <- data.frame(dummy = rep(NA_integer_, object@nspectra))

    # Initialize parent backend cache
    object <- callNextMethod(
      object,
      nspectra = object@nspectra,
      spectraVariables = c(
        "spectrum_id", "msLevel", "polarity", "compound_id",
        "precursor_mz", "instrument", "instrument_type",
        "acquisitionNum", "precScanNum", "collision_energy",
        "predicted", "splash", "dataOrigin", "original_id"
      )
    )

    validObject(object)
    object
  }
)

#' @exportMethod dataStorage
#' @importMethodsFrom ProtGenerics dataStorage
#' @importFrom DBI dbGetInfo
#' @rdname MsBackendMongoDb
setMethod("dataStorage", "MsBackendMongoDb",
          function(object) {
            if (!is.null(.dbcon(object))) {
              paste(
                "MongoDB:",
                paste(names(object@.collections), collapse = ", "),
                collapse = ""
              )
            } else character()
          })

#' @exportMethod [
#' @importFrom MsCoreUtils i2index
#' @importFrom methods slot<-
#' @importFrom MsCoreUtils i2index
#' @importFrom S4Vectors extractROWS
#' @rdname MsBackendMongoDb
setMethod("[", "MsBackendMongoDb",
          function(x, i, j, ..., drop = FALSE) {
            if (missing(i))
              return(x)
            i <- MsCoreUtils::i2index(i, length(x), x@spectraIds)
            extractByIndex(x, i)
          })

#' @rdname MsBackendMongoDb
#' @importMethodsFrom ProtGenerics extractByIndex
#' @export
setMethod("extractByIndex", c("MsBackendMongoDb", "ANY"),
          function(object, i) {
            slot(object, "spectraIds", check = FALSE) <- object@spectraIds[i]
            callNextMethod(object, i = i)
          })

#' @importMethodsFrom ProtGenerics peaksData
#' @exportMethod peaksData
#' @rdname MsBackendMongoDb
# Override peaksData to use peak_fun automatically
setMethod("peaksData", "MsBackendMongoDb",
          function(object, columns = c("mz", "intensity")) {
            object@peak_fun(object, columns)
          })

#' @importMethodsFrom ProtGenerics peaksVariables
#' @exportMethod peaksVariables
#' @rdname MsBackendMongoDb
setMethod("peaksVariables", "MsBackendMongoDb",
          function(object)
            .available_peaks_variables_mongo(object))

#' @exportMethod intensity
#' @importMethodsFrom ProtGenerics intensity
#' @rdname MsBackendMongoDb
#'
setMethod("intensity", "MsBackendMongoDb", function(object) {
  peaks <- .fetch_peaks_data_long_mongo(object, "intensity", drop = TRUE)
  peaks <- lapply(peaks, function(x) if (is.matrix(x)) as.numeric(x[,1]) else as.numeric(x))
  S4Vectors::SimpleList(peaks, compress = FALSE)
})


#' @exportMethod intensity<-
#' @importMethodsFrom ProtGenerics intensity<-
#' @rdname MsBackendMongoDb
setReplaceMethod("intensity", "MsBackendMongoDb",
                 function(object, value) {
                   stop("Can not replace original intensity values in the database.")
                 })

#' @exportMethod mz
#' @importMethodsFrom ProtGenerics mz
#' @rdname MsBackendMongoDb
#'
setMethod("mz", "MsBackendMongoDb", function(object) {
  # Get raw peaks
  peaks <- .fetch_peaks_data_long_mongo(object, "mz", drop = TRUE)
  # Ensure every element is numeric
  peaks <- lapply(peaks, function(x) if (is.matrix(x)) as.numeric(x[,1]) else as.numeric(x))
  S4Vectors::SimpleList(peaks, compress = FALSE)
})

#' @exportMethod mz<-
#' @importMethodsFrom ProtGenerics mz<-
#' @rdname MsBackendMongoDb
setReplaceMethod("mz", "MsBackendMongoDb",
                 function(object, value) {
                   stop("Can not replace original data in the database.")
                 })

#' @rdname MsBackendMongoDb
#' @export
#'
#' @importFrom methods callNextMethod
setReplaceMethod("$", "MsBackendMongoDb",
                 function(x, name, value) {
                   if (name %in% c("spectrum_id"))
                     stop("Spectra IDs can not be changed.", call. = FALSE)
                   callNextMethod()
                 })

#' @importMethodsFrom ProtGenerics spectraData spectraVariables
#' @exportMethod spectraData
#' @rdname MsBackendMongoDb
setMethod("spectraData", "MsBackendMongoDb",
          function(object, columns = spectraVariables(object)) {
            .spectra_data_mongo(object, columns = columns)
          })

#' @exportMethod reset
#' @importMethodsFrom Spectra reset
#' @rdname MsBackendMongoDb
setMethod("reset", "MsBackendMongoDb",
          function(object) {
            message("Restoring original data ...", appendLF = FALSE)
            if (inherits(object@dbcon, "mongo"))
              object <- backendInitialize(MsBackendMongoDb(), object@dbcon)
            message("DONE")
            object
          })

#' @exportMethod spectraNames
#' @importMethodsFrom ProtGenerics spectraNames
#' @rdname MsBackendMongoDb
setMethod("spectraNames", "MsBackendMongoDb",
          function(object) {
            as.character(object@spectraIds)
          })

#' @exportMethod spectraNames<-
#' @importMethodsFrom ProtGenerics spectraNames<-
#' @rdname MsBackendMongoDb
setReplaceMethod("spectraNames", "MsBackendMongoDb",
                 function(object, value) {
                   stop("Replacing spectraNames is not supported for ",
                        class(object)[1L])
                 })
