#' Access GNPS and MSnLib spectra stored in MongoDB
#'
#' @description
#' `MsBackendGnpsMongoDb` extends `MsBackendMongoDb` to provide read-only
#' access to reference spectra imported into a single MongoDB collection.
#'
#' @details
#' MongoDB ObjectIds identify documents internally. Metadata and peaks are
#' retrieved on demand and returned in the requested spectrum order.
#'
#' For MSnLib, precursor paths, collision-energy paths, fragmentation
#' methods, isolation windows, and source scan numbers are preserved.
#' Paths are returned as list columns with one vector per spectrum.
#' Parent scan references are not inferred automatically.
#'
#' Missing GNPS MS levels are assumed to represent MS/MS spectra.
#' This assumption is specific to the GNPS reference library used here.
#'
#' @param object A `MsBackendGnpsMongoDb` object.
#' @param dbcon A mongolite connection to the collection.
#' @param format Source layout, either `"gnps"` or `"msnlib"`.
#' @param columns Spectrum or peak variables to retrieve.
#' @param i Spectrum indices.
#' @param ... Additional arguments.
#'
#' @include MsBackendMongoDB.R
#' @importFrom methods new setClass setMethod validObject selectMethod
#' @importFrom methods callNextMethod
#' @exportClass MsBackendGnpsMongoDb
#' @name MsBackendGnpsMongoDb
NULL

setClass(
  "MsBackendGnpsMongoDb",
  contains = "MsBackendMongoDb",
  slots = c(sourceFormat = "character"),
  prototype = list(
    sourceFormat = "gnps",
    peak_fun = function(...) list()
  )
)

#' @rdname MsBackendGnpsMongoDb
#' @export
MsBackendGnpsMongoDb <- function() {
  new("MsBackendGnpsMongoDb")
}

#' @rdname MsBackendGnpsMongoDb
#' @exportMethod backendInitialize
#'
#' @importFrom MetaboCoreUtils translate
setMethod(
  "backendInitialize", "MsBackendGnpsMongoDb",
  function(object, dbcon, format = c("gnps", "msnlib"), ...) {
    format <- match.arg(format)

    if (!inherits(dbcon, "mongo"))
      stop("'dbcon' must be a mongolite connection.")

    # Retrieve document identifiers.
    documents <- dbcon$find(fields = '{"_id": 1}')
    ids <- as.character(documents[["_id"]])

    first_document <- dbcon$find(
      fields = '{"peaks":0,"peaks_json":0}',
      limit = 1L
    )

    metadata_fields <- translate(setdiff(names(first_document), "_id"),
                                 .revmap(.SPECTRA_2_GNPS), warning = FALSE)

    if (length(ids) != dbcon$count() ||
        anyNA(ids) ||
        any(!grepl("^[[:xdigit:]]{24}$", ids)) ||
        anyDuplicated(ids)) {
      stop("Documents must have unique MongoDB ObjectIds.")
    }

    object@sourceFormat <- format
    object@dbcon <- dbcon
    object@.collections <- list(reference = dbcon)
    object@spectraIds <- ids
    object@id_map <- ids
    object@nspectra <- as.integer(length(ids))
    object@localData <- data.frame()
    object@peak_fun <- .gnps_mongo_peaks

    initialize_cache <- methods::selectMethod(
      "backendInitialize", "MsBackendCached"
    )

    object <- initialize_cache(
      object,
      nspectra = length(ids),
      spectraVariables = unique(c(
        metadata_fields,
        "spectrum_id",
        "mz",
        "intensity"
      ))
    )

    validObject(object)
    object
  }
)


#' @rdname MsBackendGnpsMongoDb
#' @exportMethod peaksVariables
setMethod("peaksVariables", "MsBackendGnpsMongoDb", function(object) {
    c("mz", "intensity")
})


#' @rdname MsBackendGnpsMongoDb
#' @exportMethod peaksData
setMethod(
  "peaksData", "MsBackendGnpsMongoDb",
  function(object, columns = c("mz", "intensity")) {
    .gnps_mongo_peaks(object, columns)
  }
)

#' Named `character` with names being spectra variable names and values the
#' corresponding fields in GNPS.
#'
#' This vector should be used to rename GNPS fields to the corresponding
#' spectraVariables
#'
#' @noRd
.SPECTRA_2_GNPS <- c(
    adduct = "Adduct",
    charge = "Charge",
    compound_name = "Compound_Name",
    compound_source = "Compound_Source",
    exactmass = "ExactMass",
    inchi = "INCHI",
    precursorMz = "Precursor_MZ",
    smiles = "Smiles",
    polarity = "Ion_Mode",
    msLevel = "ms_level"
)

.SPECTRA_2_MSNLIB <- c(
    spectrum_id = "_id",
    precursorMz = "precursor_mz",
    msLevel = "ms_level"
)

#' Named `character` with data types of spectra variables. Only non-character
#' spectra variables need to be defined/converted.
#'
#' This vector should be used in the `spectraData()` function to ensure spectra
#' variables (extracted as `character` from GNPS/MongoDB) are converted into
#' the correct data type.
#'
#' @noRd
.SPECTRA_DATATYPES <- c(
    precursorMz = "double",
    exactmass = "double",
    charge = "integer",
    msLevel = "integer"
)

#' @rdname MsBackendGnpsMongoDb
#'
#' @exportMethod spectraData
#'
#' @importFrom IRanges CharacterList NumericList
#'
#' @importFrom S4Vectors List make_zero_col_DFrame
setMethod(
  "spectraData", "MsBackendGnpsMongoDb",
  function(object, columns = spectraVariables(object)) {
    mapping <- .gnps_mongo_fields(object@sourceFormat)
    ## 1) cached variables: get them from the backend
    res <- getMethod("spectraData", "MsBackendCached")(object,columns = columns)
    if (is.null(res))
        res <- make_zero_col_DFrame(length(object))
    ## 2) database variables:
    ##    - need to rename from spectra variable to gnps fields to fetch them
    ##    - rename them back to spectra variables
    ##    - fix data types for non-character spectra variables.
    db_cols <- intersect(columns, object@spectraVariables)
    meta_cols <- translate(db_cols, mapping, warning = FALSE)
    numeric_paths <- c(
      "msn_precursor_mzs",
      "msn_collision_energies",
      "msn_isolation_windows"
    )
    if (length(db_cols)) {
      peak_cols <- intersect(c("mz", "intensity"), db_cols)
      db_cols <- setdiff(db_cols, peak_cols)
      metadata <- .gnps_mongo_metadata(
          object, fields = translate(db_cols, mapping, warning = FALSE))
      colnames(metadata) <- translate(colnames(metadata), .revmap(mapping),
                                      warning = FALSE)
      for (column in colnames(metadata)) {
        if (column %in% names(.SPECTRA_DATATYPES)) {
          value <- as(metadata[[column]], .SPECTRA_DATATYPES[column])
          if (column == "msLevel" && object@sourceFormat == "gnps")
              value[is.na(value)] <- 2L
        } else if (column %in% numeric_paths) {
          value <- NumericList(
            lapply(as.list(metadata[[column]]), function(path) {
              path <- as.numeric(unlist(path, use.names = FALSE))
              if (!length(path) || all(is.na(path)))
                return(numeric())
              path
            }), compress = FALSE)
        } else if (column == "msn_fragmentation_methods") {
          value <- CharacterList(
            lapply(as.list(metadata[[column]]), function(path) {
              path <- as.character(unlist(path, use.names = FALSE))
              if (!length(path) || all(is.na(path)))
                return(character())
              path
            })
          )
        } else if (column == "scan_number") {
          ## Preserve multiple scan references for merged spectra.
          value <- List(as.list(value))
        } else if (column == "msLevel") {
          ## Assumption for this GNPS reference library.
          if (object@sourceFormat == "gnps")
            value[is.na(value)] <- 2L
        } else if (column == "polarity") {
          mode <- tolower(trimws(as.character(metadata[[column]])))
          value <- rep(NA_integer_, length(object))
          value[mode %in% c("-", "negative", "neg")] <- 0L
          value[mode %in% c("+", "positive", "pos")] <- 1L
        } else value <- metadata[[column]]
        if (is.list(value)) {
          value <- List(value)
        }
        res[[column]] <- value
      }
      ## Peaks
      if (length(peak_cols)) {
        peaks <- .gnps_mongo_peaks(object, columns = peak_cols)
        for (column in peak_cols) {
          res[[column]] <- NumericList(
            lapply(peaks, function(peak) peak[, column])
          , compress = FALSE)
        }
      }
    }
    res[, columns, drop = FALSE]
  })

#' @rdname MsBackendGnpsMongoDb
#' @exportMethod extractByIndex
setMethod(
  "extractByIndex", c("MsBackendGnpsMongoDb", "ANY"),
  function(object, i) {
    object <- callNextMethod()
    object@peak_fun <- .gnps_mongo_peaks
    object
  }
)


#' @rdname MsBackendGnpsMongoDb
#' @exportMethod reset
setMethod("reset", "MsBackendGnpsMongoDb", function(object) {
  backendInitialize(
    object,
    dbcon = object@dbcon,
    format = object@sourceFormat
  )
})


#' @rdname MsBackendGnpsMongoDb
#' @exportMethod dataStorage
setMethod("dataStorage", "MsBackendGnpsMongoDb", function(object) {
  rep("MongoDB reference collection", length(object@spectraIds))
})
