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
#' spectra and peaks data on demand. By storing only the primary keys
#' (`spectrum_id`) in memory, it ensures minimal memory usage, while peaks and
#' metadata are fetched dynamically.
#'
#' The backend inherits from [Spectra::MsBackendCached()] and supports temporary
#' modification of spectra variables via the `$<-` operator. Original data in
#' the database cannot be changed (intensities and m/z values are read-only).
#'
#' @section Creation of backend objects:
#'
#' New backend objects can be created using:
#'
#' ```r
#' con <- connectMsBackendMongoDb(db = <database name>, url = <db url>)
#' backend <- backendInitialize(MsBackendMongoDb(), dbcon = con)
#' ```
#'
#' - `dbcon`: A list of MongoDb collections (e.g. from `mongolite::mongo()`) as
#'   created by the [connectMsBackendMongoDb()].
#'
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
#' - `spectraData(object, columns)`: returns a `DataFrame` with requested
#'   spectrum metadata columns.
#' - `spectraNames(object)`: returns `spectrum_id` values as character.
#'
#' @section Read-only data:
#'
#' - `intensity<-` and `mz<-` are **not supported**.
#' - `spectraNames<-` is **not supported**.
#'
#' @section Subsetting and extraction:
#'
#' - `[i]` and `extractByIndex(object, i)` subset the backend by spectrum
#'   indices. Original data in the database is never changed.
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
#' @param dbcon `list()` of MongoDb collections, e.g.,
#'     `list(compounds_info_coll = mongo_conn)`
#'
#' @param object A `MsBackendMongoDb` instance
#'
#' @param x for `[i]`: a `MsBackendMongoDb` instance
#'
#' @param columns `Character()` vector of columns to fetch for `peaksData` or
#'       `spectraData`
#'
#' @param i `integer()` Indices to subset the backend
#'
#' @param j `integer()` Ignored.
#'
#' @param value Replacement values for setter functions
#'
#' @param data Optional `DataFrame` with data to insert into MongoDb
#'
#' @param drop `logical(1)`, ignored for subsetting
#'
#' @param ... Additional arguments passed internally
#'
#' @return Depends on the method. See individual function documentation.
#'
#' @importClassesFrom DBI DBIConnection
#' @importClassesFrom S4Vectors DataFrame
#' @importClassesFrom Spectra MsBackendCached
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
        spectraIds = "character",
        .collections = "list",
        peak_fun = "function",
        localData = "data.frame",
        nspectra = "integer",
        id_map = "character"
    ),
    prototype = prototype(
        dbcon = NULL,
        spectraIds = character(),
        .collections = list(),
        peak_fun = NULL,
        localData = data.frame(),
        nspectra = 0L,
        id_map = character(0),
        readonly = TRUE,
        version = "0.2"
    )
)

#' @importFrom methods .valueClassTest is new validObject
#' @noRd
setValidity("MsBackendMongoDb", function(object) {

    ## Check dbcon
    msg <- .valid_mongocon(object@dbcon)
    if (!is.null(msg)) return(msg)

    ## nspectra matches spectraIds
    if (length(object@spectraIds) != object@nspectra)
        return("Number of spectraIds does not match nspectra")

    ## localData row check
    if (nrow(object@localData) != 0L &&
        nrow(object@localData) != object@nspectra)
        return("Number of rows in localData does not match nspectra")

    ## id_map consistency
    if (length(object@id_map) != object@nspectra)
        return("Length of id_map must match nspectra")

    TRUE
})

#' @importMethodsFrom Spectra show
#' @importFrom methods callNextMethod
#' @exportMethod show
#' @rdname MsBackendMongoDb
setMethod("show", "MsBackendMongoDb", function(object) {
    callNextMethod()
    if (!is.null(object@dbcon)) {
        cat("MongoDb collections: ",
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

        ## Insert data if provided
        if (!missing(data)) {
            .createMsBackendMongoDb(dbcon, x = data)
        }

        ## Validate MongoDb connection
        msg <- .valid_mongocon(dbcon)
        if (!is.null(msg)) stop(msg)

        ## Store collections names
        object@.collections <- dbcon

        ## Fetch spectrum_id_ as character
        spectrum_ids <- as.character(
            dbcon[["ms_spectrum_coll"]]$distinct("spectrum_id_")
        )

        ## Assign spectrum IDs and nspectra
        object@spectraIds <- spectrum_ids
        object@id_map <- spectrum_ids
        object@dbcon <- dbcon
        object@peak_fun <- .fetch_peaks_data_long_mongo

        ## Get available metadata column names from the ms_spectrum_coll
        meta_doc <- dbcon[["ms_spectrum_coll"]]$find(limit = 1)

        if (nrow(meta_doc)) {
            svars <- setdiff(
                colnames(meta_doc),
                c("_id", "mz", "intensity")
            )
        } else {
            svars <- character()
        }

        if (!"spectrum_id_" %in% svars)
            svars <- c("spectrum_id_", svars)

        ## Initialize parent backend cache
        object <- callNextMethod(
            object,
            nspectra = length(spectrum_ids),
            spectraVariables = union(svars, c("mz", "intensity"))
        )

        validObject(object)
        object
    }
)

#' @exportMethod dataStorage
#' @importMethodsFrom ProtGenerics dataStorage
#' @importFrom DBI dbGetInfo
#' @rdname MsBackendMongoDb
setMethod("dataStorage", "MsBackendMongoDb", function(object) {
    if (object@nspectra == 0L) return(character(0))
    ## Each spectrum comes from the same collection
    rep("ms_spectrum_coll", object@nspectra)
})

#' @importMethodsFrom ProtGenerics peaksData
#' @exportMethod peaksData
#' @rdname MsBackendMongoDb
setMethod("peaksData", "MsBackendMongoDb",
          function(object, columns = c("mz", "intensity")) {
              object@peak_fun(object, columns)
          })

#' @importMethodsFrom ProtGenerics peaksVariables
#' @exportMethod peaksVariables
#' @rdname MsBackendMongoDb
setMethod("peaksVariables", "MsBackendMongoDb", function(object) {
    if (!is.null(object@dbcon))
        .available_peaks_variables_mongo(object)
    else character()
})

#' @exportMethod mz
#' @importMethodsFrom ProtGenerics mz
#' @rdname MsBackendMongoDb
#'
setMethod("mz", "MsBackendMongoDb", function(object) {
    peaks <- object@peak_fun(object, columns = c("mz", "intensity"))
    NumericList(lapply(peaks, function(mat) {
        if (is.null(mat) || nrow(mat) == 0) numeric(0) else mat[, "mz"]
    }), compress = FALSE)
})

#' @exportMethod mz<-
#' @importMethodsFrom ProtGenerics mz<-
#' @rdname MsBackendMongoDb
setReplaceMethod("mz", "MsBackendMongoDb",
                 function(object, value) {
                     stop("Can not replace original data in the database.")
                 })

#' @exportMethod intensity
#' @importMethodsFrom ProtGenerics intensity
#' @rdname MsBackendMongoDb
#'
setMethod("intensity", "MsBackendMongoDb", function(object) {
    peaks <- object@peak_fun(object, columns = c("mz", "intensity"))
    NumericList(lapply(peaks, function(mat) {
        if (is.null(mat) || nrow(mat) == 0) numeric(0)
        else mat[, "intensity"]
    }), compress = FALSE)
})

#' @exportMethod intensity<-
#' @importMethodsFrom ProtGenerics intensity<-
#' @rdname MsBackendMongoDb
setReplaceMethod("intensity", "MsBackendMongoDb",
                 function(object, value) {
                     stop("Can not replace original intensity
                        values in the database.")
                 })

#' @exportMethod spectraNames
#' @importMethodsFrom ProtGenerics spectraNames
#' @rdname MsBackendMongoDb
setMethod("spectraNames", "MsBackendMongoDb",
          function(object) as.character(object@spectraIds))
setReplaceMethod("spectraNames", "MsBackendMongoDb",
                 function(object, value) {
                     stop("Replacing spectraNames is not supported.")
                 })

#' @exportMethod spectraNames<-
#' @importMethodsFrom ProtGenerics spectraNames<-
#' @rdname MsBackendMongoDb
setReplaceMethod("spectraNames", "MsBackendMongoDb",
                 function(object, value) {
                     stop("Replacing spectraNames is not supported for ",
                          class(object)[1L])
                 })

#' @importMethodsFrom ProtGenerics spectraData spectraVariables
#' @exportMethod spectraData
#' @rdname MsBackendMongoDb
setMethod("spectraData", "MsBackendMongoDb",
          function(object, columns = spectraVariables(object)) {
                                        # Fetch scalars + peaks
              .fetch_spectra_data_mongo(object, columns)
          }
          )

#' @exportMethod [
#' @importFrom MsCoreUtils i2index
#' @importFrom methods slot<-
#' @importFrom MsCoreUtils i2index
#' @importFrom S4Vectors extractROWS
#' @rdname MsBackendMongoDb
setMethod("[", "MsBackendMongoDb",
          function(x, i, j, ..., drop = FALSE) {
              if (missing(i)) return(x)

              ## Convert i to numeric index
              i <- i2index(i, length(x), x@spectraIds)

              ## Subset backend object
              extractByIndex(x, i)
          }
          )

#' @rdname MsBackendMongoDb
#' @importFrom fastmatch fmatch
#' @importMethodsFrom ProtGenerics extractByIndex
#' @export
setMethod("extractByIndex", c("MsBackendMongoDb", "ANY"),
          function(object, i) {
              ## Subset internal IDs
              object@spectraIds <- object@spectraIds[i]
              object@id_map <- object@id_map[i]
              object@nspectra <- length(object@spectraIds)

              if (nrow(object@localData) > 0) {
                  object@localData <- object@localData[i, , drop = FALSE]
              }

              object
          }
          )

#' @exportMethod reset
#'
#' @importMethodsFrom Spectra reset
#' @rdname MsBackendMongoDb
setMethod("reset", "MsBackendMongoDb", function(object) {
    if (!length(object@dbcon) ||
        !all(sapply(c("ms_spectrum_coll","ms_peaks_coll"),
                    function(coll) inherits(object@dbcon[[coll]], "mongo")))) {
        stop("Cannot reset: invalid or missing
                             MongoDB connection.")
    }

    ## Re-fetch all spectrum_ids from database
    spectrum_ids <- as.character(
        object@dbcon$ms_spectrum_coll$distinct("spectrum_id_"))

    object@spectraIds <- spectrum_ids
    object@id_map     <- spectrum_ids
    object@nspectra   <- length(spectrum_ids)
    object@localData  <- data.frame(dummy = rep(NA_integer_, object@nspectra))

    ## Restore peak_fun
    object@peak_fun <- .fetch_peaks_data_long_mongo

    validObject(object)
    object
})

#' @importMethodsFrom Spectra supportsSetBackend
#'
#' @exportMethod supportsSetBackend
#'
#' @rdname MsBackendMongoDb
setMethod("supportsSetBackend", "MsBackendMongoDb", function(object, ...) {
    TRUE
})

#' @importMethodsFrom ProtGenerics setBackend
#'
#' @importFrom Spectra processingChunkFactor
#'
#' @noRd
setMethod(
    "setBackend", c("Spectra", "MsBackendMongoDb"),
    function(object, backend, f = processingChunkFactor(object), dbcon, ...,
             BPPARAM = BiocParallel::SerialParam()) {
        backend_class <- class(object@backend)[1L]
        if (missing(dbcon))
            stop("Parameter 'dbcon' is required for 'MsBackendMongoDb'")
        object@backend <- backendInitialize(
            backend, data = spectraData(object@backend),
            dbcon = dbcon, ...)
        object@processing <- Spectra:::.logging(object@processing,
                                                "Switch backend from ",
                                                backend_class, " to ",
                                                class(object@backend))
        object
    })

#' @importMethodsFrom Spectra tic
#'
#' @noRd
setMethod("tic", "MsBackendMongoDb", function(object, initial = TRUE) {
    as.numeric(callNextMethod())
})
