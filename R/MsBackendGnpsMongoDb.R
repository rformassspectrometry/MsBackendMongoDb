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


# Map backend variables to source fields.
.gnps_mongo_fields <- function(format) {
  if (format == "msnlib") {
    return(c(
      spectrum_id = "_id",
      precursorMz = "precursor_mz",
      polarity = "polarity",
      msLevel = "ms_level",
      compound_name = "compound_name",
      smiles = "smiles",
      inchikey = "inchikey",
      feature_id = "feature_id",
      merge_type = "merge_type",
      adduct = "adduct",
      collision_energy = "collision_energy",
      scan_number = "scan_number",
      msn_precursor_mzs = "msn_precursor_mzs",
      msn_collision_energies = "msn_collision_energies",
      msn_fragmentation_methods = "msn_fragmentation_methods",
      msn_isolation_windows = "msn_isolation_windows"
    ))
  }
  
  c(
    spectrum_id = "spectrum_id",
    precursorMz = "Precursor_MZ",
    polarity = "Ion_Mode",
    msLevel = "ms_level",
    compound_name = "Compound_Name",
    smiles = "Smiles",
    inchikey = "InChIKey_smiles",
    adduct = "Adduct",
    collision_energy = "collision_energy"
  )
}


# Preserve the ObjectId type when querying MongoDB.
.gnps_mongo_query <- function(ids) {
  object_ids <- lapply(unique(ids), function(id) {
    list("$oid" = id)
  })
  
  list("_id" = list("$in" = object_ids))
}


# Retrieve source metadata and restore order and repeated indices.
.gnps_mongo_metadata <- function(object, fields) {
  ids <- object@spectraIds
  
  if (!length(ids))
    return(data.frame())
  
  fields <- unique(c("_id", fields))
  projection <- as.list(rep(1L, length(fields)))
  names(projection) <- fields
  
  unique_ids <- unique(ids)
  batches <- split(
    unique_ids,
    ceiling(seq_along(unique_ids) / 1000L)
  )
  
  # These fields can hold several values for one spectrum.
  list_fields <- intersect(
    fields,
    c(
      "scan_number",
      "msn_precursor_mzs",
      "msn_collision_energies",
      "msn_fragmentation_methods",
      "msn_isolation_windows"
    )
  )
  
  documents <- lapply(batches, function(batch) {
    document <- object@dbcon$find(
      query = jsonlite::toJSON(
        .gnps_mongo_query(batch),
        auto_unbox = TRUE
      ),
      fields = jsonlite::toJSON(projection, auto_unbox = TRUE)
    )
    
    # Normalize list columns before combining batches. A path field can
    # be absent from an entire batch containing only MS2 spectra.
    for (field in fields) {
      if (field %in% list_fields) {
        value <- document[[field]]
        
        if (is.null(value))
          value <- rep(list(NULL), nrow(document))
        
        document[[field]] <- I(as.list(value))
        
      } else if (!field %in% names(document)) {
        document[[field]] <- rep(NA_character_, nrow(document))
      }
    }
    
    document[, fields, drop = FALSE]
  })
  
  documents <- do.call(rbind, documents)
  index <- match(ids, as.character(documents[["_id"]]))
  
  if (anyNA(index))
    stop("Some spectra are missing from the MongoDB collection.")
  
  documents[index, , drop = FALSE]
}


# Retrieve peaks through the existing GNPS/MSnLib peak decoder.
.gnps_mongo_peaks <- function(object, columns = c("mz", "intensity")) {
  if (any(!columns %in% c("mz", "intensity")))
    stop("Peak columns must be 'mz' and/or 'intensity'.")
  
  ids <- object@spectraIds
  
  if (!length(ids))
    return(list())
  
  unique_ids <- unique(ids)
  batches <- split(
    unique_ids,
    ceiling(seq_along(unique_ids) / 1000L)
  )
  
  peak_batches <- lapply(batches, function(batch) {
    gnps_fetch_peaks_data(
      object@dbcon,
      query = .gnps_mongo_query(batch),
      id_field = "_id"
    )
  })
  
  peaks <- do.call(c, unname(peak_batches))
  index <- match(ids, names(peaks))
  
  if (anyNA(index))
    stop("Could not retrieve peaks for every requested spectrum.")
  
  result <- lapply(peaks[index], function(peak) {
    peak[, columns, drop = FALSE]
  })
  
  names(result) <- ids
  result
}


#' @rdname MsBackendGnpsMongoDb
#' @exportMethod backendInitialize
setMethod(
  "backendInitialize", "MsBackendGnpsMongoDb",
  function(object, dbcon, format = c("gnps", "msnlib"), ...) {
    format <- match.arg(format)
    
    if (!inherits(dbcon, "mongo"))
      stop("'dbcon' must be a mongolite connection.")
    
    documents <- dbcon$find(fields = '{"_id": 1}')
    ids <- as.character(documents[["_id"]])
    
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
      spectraVariables = c(
        names(.gnps_mongo_fields(format)),
        "mz", "intensity"
      )
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


#' @rdname MsBackendGnpsMongoDb
#' @exportMethod spectraData
setMethod(
  "spectraData", "MsBackendGnpsMongoDb",
  function(object, columns = spectraVariables(object)) {
    mapping <- .gnps_mongo_fields(object@sourceFormat)
    
    standard_types <- c(
      msLevel = "integer",
      rtime = "numeric",
      acquisitionNum = "integer",
      scanIndex = "integer",
      dataStorage = "character",
      dataOrigin = "character",
      centroided = "logical",
      smoothed = "logical",
      polarity = "integer",
      precScanNum = "integer",
      precursorMz = "numeric",
      precursorIntensity = "numeric",
      precursorCharge = "integer",
      collisionEnergy = "numeric",
      isolationWindowLowerMz = "numeric",
      isolationWindowTargetMz = "numeric",
      isolationWindowUpperMz = "numeric"
    )
    
    supported <- union(
      names(standard_types),
      c(names(mapping), "mz", "intensity")
    )
    
    unknown <- setdiff(columns, supported)
    
    if (length(unknown)) {
      stop(
        "Unsupported spectrum variables: ",
        paste(unknown, collapse = ", ")
      )
    }
    
    n <- length(object@spectraIds)
    result <- S4Vectors::DataFrame(row.names = seq_len(n))
    
    # Initialize standard variables without source mappings.
    standard_columns <- setdiff(
      intersect(columns, names(standard_types)),
      c(names(mapping), "mz", "intensity", "dataStorage")
    )
    
    for (column in standard_columns) {
      result[[column]] <- switch(
        standard_types[[column]],
        integer = rep(NA_integer_, n),
        numeric = rep(NA_real_, n),
        logical = rep(NA, n),
        character = rep(NA_character_, n)
      )
    }
    
    if ("dataStorage" %in% columns)
      result$dataStorage <- dataStorage(object)
    
    numeric_paths <- c(
      "msn_precursor_mzs",
      "msn_collision_energies",
      "msn_isolation_windows"
    )
    
    metadata_columns <- intersect(columns, names(mapping))
    
    if (length(metadata_columns)) {
      metadata <- .gnps_mongo_metadata(
        object,
        fields = unname(mapping[metadata_columns])
      )
      
      for (column in metadata_columns) {
        source <- mapping[[column]]
        value <- metadata[[source]]
        
        if (is.null(value))
          value <- rep(NA_character_, n)
        
        if (column %in% numeric_paths) {
          value <- IRanges::NumericList(
            lapply(as.list(value), function(path) {
              path <- as.numeric(unlist(path, use.names = FALSE))
              
              if (!length(path) || all(is.na(path)))
                return(numeric())
              
              path
            })
          )
          
        } else if (column == "msn_fragmentation_methods") {
          value <- IRanges::CharacterList(
            lapply(as.list(value), function(path) {
              path <- as.character(unlist(path, use.names = FALSE))
              
              if (!length(path) || all(is.na(path)))
                return(character())
              
              path
            })
          )
          
        } else if (column == "scan_number") {
          # Preserve multiple scan references for merged spectra.
          value <- S4Vectors::List(as.list(value))
          
        } else if (column == "precursorMz") {
          value <- suppressWarnings(as.numeric(as.character(value)))
          
        } else if (column == "msLevel") {
          value <- suppressWarnings(as.integer(as.character(value)))
          
          # Assumption for this GNPS reference library.
          if (object@sourceFormat == "gnps")
            value[is.na(value)] <- 2L
          
        } else if (column == "polarity") {
          mode <- tolower(trimws(as.character(value)))
          value <- rep(NA_integer_, n)
          value[mode %in% c("-", "negative", "neg")] <- 0L
          value[mode %in% c("+", "positive", "pos")] <- 1L
          
        } else if (column == "spectrum_id") {
          value <- as.character(value)
        }
        
        result[[column]] <- value
      }
    }
    
    peak_columns <- intersect(columns, c("mz", "intensity"))
    
    if (length(peak_columns)) {
      peaks <- .gnps_mongo_peaks(object, columns = peak_columns)
      
      for (column in peak_columns) {
        result[[column]] <- IRanges::NumericList(
          lapply(peaks, function(peak) peak[, column])
        )
      }
    }
    
    result[, columns, drop = FALSE]
  }
)


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