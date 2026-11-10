#' @title Read directly imported GNPS spectra from MongoDB
#'
#' @description
#' `MsBackendGnpsMongoDb` extends `MsBackendMongoDb` to provide read-only
#' access to GNPS spectral library data imported directly into MongoDB.
#' The backend uses an existing MongoDB connection to expose the spectra
#' through a `Spectra` object in R.
#'
#' @details
#' The backend reads from a single MongoDB collection in which each document
#' contains the metadata and fragment peaks for one spectrum. It uses the
#' GNPS `spectrum_id` field to identify spectra and maps source fields such as
#' `Precursor_MZ` and `Ion_Mode` to the `Spectra` variables `precursorMz`
#' and `polarity`, respectively.
#'
#' Spectrum metadata and fragment peaks are retrieved on demand.
#' `spectraData()` returns spectrum variables, and `peaksData()` decodes the
#' stored `peaks_json` field into a two-column matrix of `mz` and `intensity`
#' values for each requested spectrum. The backend preserves the requested
#' spectrum order when subsetting, including repeated indices.
#'
#' Import the GNPS library into MongoDB before creating the backend.
#' 
#' @param object A `MsBackendGnpsMongoDb` backend.
#' @param dbcon A `mongolite::mongo()` connection to the GNPS collection.
#' @param ... Additional arguments passed to the backend initializer.
#'
#' @include MsBackendMongoDB.R
#' @export
setClass("MsBackendGnpsMongoDb", contains = "MsBackendMongoDb")

#' @rdname MsBackendGnpsMongoDb
#' @export
MsBackendGnpsMongoDb <- function() {
  new("MsBackendGnpsMongoDb")
}

.gnps_mongo_query <- function(ids) {
  list(spectrum_id = list("$in" = as.list(unique(ids))))
}

.gnps_mongo_peaks <- function(object, columns = c("mz", "intensity")) {
  ids <- object@spectraIds
  if (!length(ids))
    return(list())
  
  peaks <- gnps_fetch_peaks_data(
    object@dbcon,
    query = .gnps_mongo_query(ids)
  )
  index <- match(ids, names(peaks))
  if (anyNA(index))
    stop("Could not retrieve peaks for every requested GNPS spectrum.")
  
  lapply(peaks[index], function(peak) peak[, columns, drop = FALSE])
}

#' @rdname MsBackendGnpsMongoDb
#' @exportMethod backendInitialize
setMethod(
  "backendInitialize", "MsBackendGnpsMongoDb",
  function(object, dbcon, ...) {
    if (!inherits(dbcon, "mongo"))
      stop("'dbcon' must be a mongolite connection to a GNPS collection.")
    
    # Project only the IDs. MongoDB's distinct command returns all IDs
    # in one response, which can be too large for a full GNPS library.
    ids <- as.character(
      dbcon$find(fields = '{"spectrum_id": 1, "_id": 0}')$spectrum_id
    )
    if (length(ids) != dbcon$count() || anyNA(ids) ||
        any(!nzchar(ids)) || anyDuplicated(ids))
      stop("Each GNPS document must have a unique 'spectrum_id'.")
    
    object@dbcon <- dbcon
    object@.collections <- list(gnps = dbcon)
    object@spectraIds <- ids
    object@id_map <- ids
    object@nspectra <- length(ids)
    object@localData <- data.frame()
    object@peak_fun <- .gnps_mongo_peaks
    
    # Initialize the cache without invoking the two-collection initializer.
    initialize_cache <- methods::selectMethod(
      "backendInitialize", "MsBackendCached"
    )
    object <- initialize_cache(
      object,
      nspectra = length(ids),
      spectraVariables = c(
        "spectrum_id", "precursorMz", "polarity", "msLevel",
        "mz", "intensity"
      )
    )
    validObject(object)
    object
  }
)

setMethod("peaksVariables", "MsBackendGnpsMongoDb", function(object) {
  c("mz", "intensity")
})

setMethod(
  "peaksData", "MsBackendGnpsMongoDb",
  function(object, columns = c("mz", "intensity")) {
    .gnps_mongo_peaks(object, columns)
  }
)

setMethod(
  "spectraData", "MsBackendGnpsMongoDb",
  function(object, columns = spectraVariables(object)) {
    ids <- object@spectraIds
    result <- S4Vectors::DataFrame(row.names = seq_along(ids))
    
    if ("spectrum_id" %in% columns)
      result$spectrum_id <- ids
    
    if (any(c("precursorMz", "polarity", "msLevel") %in% columns)) {
      meta <- if (length(ids)) {
        gnps_fetch_spectra_data(
          object@dbcon,
          query = .gnps_mongo_query(ids),
          fields = c("spectrum_id", "Precursor_MZ", "Ion_Mode",
                     "ms_level")
        )
      } else {
        data.frame()
      }
      
      index <- match(ids, meta$spectrum_id)
      if (anyNA(index))
        stop("Could not retrieve metadata for every requested GNPS spectrum.")
      
      if ("precursorMz" %in% columns) {
        value <- as.character(meta$Precursor_MZ)[index]
        result$precursorMz <- suppressWarnings(as.numeric(value))
      }
      if ("polarity" %in% columns) {
        mode <- tolower(trimws(as.character(meta$Ion_Mode)[index]))
        result$polarity <- match(mode, c("negative", "positive")) - 1L
      }
      if ("msLevel" %in% columns) {
        value <- as.character(meta$ms_level)[index]
        result$msLevel <- suppressWarnings(as.integer(value))
      }
    }
    
    peak_columns <- intersect(columns, peaksVariables(object))
    if (length(peak_columns)) {
      peaks <- peaksData(object, columns = peak_columns)
      for (column in peak_columns) {
        result[[column]] <- IRanges::NumericList(
          lapply(peaks, function(peak) peak[, column])
        )
      }
    }
    
    result[, columns, drop = FALSE]
  }
)

setMethod(
  "extractByIndex", c("MsBackendGnpsMongoDb", "ANY"),
  function(object, i) {
    object <- callNextMethod()
    object@peak_fun <- .gnps_mongo_peaks
    object
  }
)

setMethod("reset", "MsBackendGnpsMongoDb", function(object) {
  backendInitialize(object, dbcon = object@dbcon)
})

setMethod("dataStorage", "MsBackendGnpsMongoDb", function(object) {
  rep("gnps", length(object@spectraIds))
})