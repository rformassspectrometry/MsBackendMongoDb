#' @title Constructor for the MsBackendMongoDb Backend
#'
#' @description
#'
#' Creates and initializes an `MsBackendMongoDb` object, storing references
#' to MongoDb connections, spectra IDs, local data storage, and backend
#' configuration.
#'
#' @param dbcon A `MongoDb connection` or a list of `mongolite::mongo` objects.
#'
#' @param spectraIds `character()` vector of spectrum IDs stored in MongoDb.
#'
#' @param collections Named `list()` of MongoDb collection handles.
#'
#' @return An instance of class `MsBackendMongoDb`.
#'
#' @author Ahlam Mentag
#'
#' @noRd
#'
#' @export
MsBackendMongoDb <- function(dbcon = NULL, collections = list()) {
  new("MsBackendMongoDb",
       dbcon = dbcon,
       spectraIds = character(0),
       .collections = collections,
       peak_fun = .fetch_peaks_data_long_mongo,
       localData = data.frame(),
       nspectra = 0L,
       id_map = character(0),
       spectraVariables = c(
           "spectrum_id_", "msLevel", "polarity",
           "precursor_mz", "instrument", "instrument_type", "acquisitionNum",
           "precScanNum", "collision_energy", "predicted", "splash",
           "dataOrigin", "original_id", "peaks"
      )
  )
}

#' @title Create a connection to MongoDb
#'
#' @description
#'
#' Creates connections to required MongoDb collections used by the
#' [MsBackendMongoDb].
#'
#' @param db `character(1)` scalar, name of the MongoDb database.
#'
#' @param url `character(1)` a MongoDb server URL.
#'
#' @return A named `list()` of `mongo` connection objects.
#'
#' @author Ahlam Mentag
#'
#' @importFrom mongolite mongo
#'
#' @export
connectMsBackendMongoDb <- function(db = "spectra_db",
                                    url = "mongodb://localhost") {
    list(
        ms_spectrum_coll = mongo(collection = "ms_spectrum_coll",
                                 db = db, url = url),
        ms_peaks_coll = mongo(collection = "ms_peaks_coll",
                              db = db, url = url)
    )
}

#' @title Validate a MongoDb Connection Object
#'
#' @description
#'
#' Ensure that the created MongoDb connection is valid.
#'
#' @param x A `mongo` object or a named list of mongo connections.
#'
#' @return `NULL` if valid, otherwise a character error message.
#'
#' @author Ahlam Mentag
#'
#' @noRd
.valid_mongocon <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)

  if (inherits(x, "mongo")) {
    conns <- list(ms_spectrum_coll = x)
  } else if (is.list(x) && all(sapply(x, inherits, "mongo"))) {
    conns <- x
  } else return(paste0("'dbcon' should be a mongolite::mongo object or a ",
                       "list of mongo objects"))

  ## Required collections for the mongodb backend
  missing <- setdiff(c("ms_spectrum_coll", "ms_peaks_coll"), names(conns))
  if (length(missing)) return(paste("Missing required collection:",
                                    paste(missing, collapse = ", ")))

  ok <- tryCatch({ conns[[1]]$run('{"ping": 1}'); TRUE },
                 error = function(e) FALSE)
  if (!ok) return("MongoDb connection not responding")

  NULL
}

#' Extract the MongoDb Connection from a Backend
#'
#' @param x An `MsBackendMongoDb` object.
#'
#' @return `list()` The stored MongoDb connection list.
#'
#' @author Ahlam Mentag
#'
#' @noRd
.dbcon <- function(x) x@dbcon

#' @title Normalize Peaks Into a Standard Format
#'
#' @description
#'
#' Converts various representations of peak lists (nested lists, matrices,
#' data.frames) into a canonical list with numeric `mz` and `intensity`.
#'
#' @param peaks A peak container (list, matrix, data.frame).
#'
#' @return A list with elements `mz` and `intensity`.
#'
#' @author Ahlam Mentag
#'
#' @noRd
.encode_peaks <- function(peaks) {
  if (is.null(peaks)) return(list(mz = numeric(), intensity = numeric()))

  while (is.list(peaks) && !all(c("mz", "intensity") %in% names(peaks))) {
    if (length(peaks) == 0) return(list(mz = numeric(), intensity = numeric()))
    peaks <- peaks[[1]]
  }

  if (is.list(peaks) && all(c("mz", "intensity") %in% names(peaks))) {
    return(list(mz = as.numeric(unlist(peaks$mz)),
                intensity = as.numeric(unlist(peaks$intensity))))
  }

  if (is.matrix(peaks)) return(list(mz = as.numeric(peaks[,1]),
                                    intensity = as.numeric(peaks[,2])))
  if (is.data.frame(peaks)) {
    if (!all(c("mz","intensity") %in% colnames(peaks)))
      stop("peaks data.frame must have columns 'mz' and 'intensity'")
    return(list(mz = as.numeric(peaks$mz),
                intensity = as.numeric(peaks$intensity)))
  }

  stop("Unsupported peaks format")
}

#' Extract mz and intensity From a MongoDb Peak Document
#'
#' @param doc A `list()` containing `mz` and `intensity`.
#'
#' @return A `list()` with numeric `mz` and `intensity`.
#'
#' @author Ahlam Mentag
#'
#' @noRd
.fetch_peaks <- function(doc) {
  if (is.null(doc)) return(list(mz = numeric(), intensity = numeric()))
  if (!all(c("mz","intensity") %in% names(doc))) stop("Document missing ",
                                                      "mz/intensity")
  list(mz = as.numeric(doc$mz), intensity = as.numeric(doc$intensity))
}

#' Get columns from the ms_peaks_coll collection
#'
#' @param x `MsBackendMongoDb`
#'
#' @author Ahlam Mentag
#'
#' @noRd
.available_peaks_variables_mongo <- function(object_or_dbcon,
                                             collection = "ms_peaks_coll") {
  dbcon <- if (inherits(object_or_dbcon, "MsBackendMongoDb"))
    object_or_dbcon@dbcon[[collection]] else object_or_dbcon[[collection]]

  docs <- dbcon$find('{}', fields = '{"mz":1,"intensity":1,"_id":0}', limit = 1)
  if (nrow(docs) == 0) return(character(0))
  c("mz", "intensity")
}

#' @title Fetch spectraData (scalar + peaks) from MongoDb
#'
#' @param x An MsBackendMongoDb backend
#'
#' @param columns Character vector of requested fields
#'
#' @return A DataFrame with requested columns
#'
#' @author Ahlam Mentag
#'
#' @importFrom S4Vectors DataFrame
#'
#' @importFrom S4Vectors make_zero_col_DFrame
#'
#' @importFrom IRanges NumericList
#'
#' @importFrom jsonlite toJSON
#'
#' @importFrom methods getMethod
#'
#' @noRd
.fetch_spectra_data_mongo <- function(x, columns = spectraVariables(x)) {
  ## Get cached spectra data
  res <- getMethod("spectraData", "MsBackendCached")(x, columns = columns)
  if (is.null(res))
      res <- make_zero_col_DFrame(length(x))

  peak_cols <- intersect(columns, peaksVariables(x))
  scalar_cols <- setdiff(columns, c(peak_cols, "mz", "intensity"))
  scalar_cols <- setdiff(scalar_cols, colnames(res)) # only get what's missing

  if (length(scalar_cols)) {
    #fetch scalar metadata
    projection_fields <- unique(c("spectrum_id_", scalar_cols))
    projection <- paste0("{", paste(sprintf('"%s":1', projection_fields),
                                    collapse = ","), ",\"_id\":0}")

    # Filter by spectraIds
    query <- list(spectrum_id_ = list("$in" = as.list(x@spectraIds)))
    docs <- x@dbcon$ms_spectrum_coll$find(
      query = toJSON(query, auto_unbox = TRUE),
      fields = projection
    )

    # Reorder rows to match backend
    docs <- docs[match(x@spectraIds, docs$spectrum_id_), , drop = FALSE]

    res <- cbind(res, docs)
  }
  # fetch peaks if requested
  if (length(peak_cols) && !is.null(x@peak_fun)) {
    peaks_list <- x@peak_fun(x, columns = peak_cols)
    if ("mz" %in% peak_cols) {
      res$mz <- NumericList(lapply(peaks_list, function(p) {
        if (is.null(p) || nrow(p) == 0) numeric(0) else p[, "mz"]
      }), compress = FALSE)
    }
    if ("intensity" %in% peak_cols) {
      res$intensity <- NumericList(lapply(peaks_list, function(p) {
        if (is.null(p) || nrow(p) == 0) numeric(0) else p[, "intensity"]
      }), compress = FALSE)
    }
  }

  # Return DataFrame with requested columns
  DataFrame(res[, columns, drop = FALSE])
}

#' @title Fetch Peak Data From MongoDb for a Backend
#'
#' @description
#'
#' Retrieves peaks for each spectrum ID in an `MsBackendMongoDb` object.
#'
#' @param object An `MsBackendMongoDb` MsBackendMongoDb Backend.
#'
#' @param columns `Character()` vector specifying which related peak
#'        columns to return (mz or/and intensity).
#'
#' @return A list of matrices or numeric vectors depending on `columns`.
#'
#' @author Ahlam Mentag
#'
#' @noRd
.fetch_peaks_data_long_mongo <- function(object, columns = c("mz","intensity"),
                                         drop = FALSE) {
  ids <- object@spectraIds
  if (length(ids) == 0) return(list())

  # Properly quote character IDs for Mongo $in
  ids_json <- paste0('"', ids, '"')

  query <- sprintf('{"spectrum_id_": {"$in": [%s]}}',
                   paste(ids_json, collapse = ","))

  # Query MongoDB
  docs <- object@.collections$ms_peaks_coll$find(
    query = query,
    fields = '{"spectrum_id_": 1, "mz": 1, "intensity": 1, "_id": 0}'
  )

  # Reorder documents to match backend spectraIds
  if(!identical(unname(ids), docs$spectrum_id_)) {
    docs <- docs[fmatch(ids, docs$spectrum_id_), , drop = FALSE]
  }

  # Convert each row to numeric matrix
  peaks_list <- lapply(seq_len(nrow(docs)), function(i) {
    mz_vals <- docs$mz[[i]]
    int_vals <- docs$intensity[[i]]

    if (is.null(mz_vals)) mz_vals <- numeric(0)
    if (is.null(int_vals)) int_vals <- numeric(0)

    cbind(mz = as.numeric(mz_vals), intensity = as.numeric(int_vals))
  })

  names(peaks_list) <- ids
  peaks_list
}

#' @title Create MongoDb Indices for Efficient Spectra Queries
#'
#' @description
#'
#' Ensures that commonly queried fields (`spectrum_id`, `precursor_mz`,
#' `msLevel`) are indexed for fast database operations.
#'
#' @param dbcon A list of mongo connections.
#'
#' @param collection Name of the collection to index.
#'
#' @return `TRUE` after indices are created.
#'
#' @author Ahlam Mentag
#'
#' @noRd
.create_indices_mongo <- function(dbcon) {
  sp_coll <- dbcon[["ms_spectrum_coll"]]
  pk_coll <- dbcon[["ms_peaks_coll"]]

  message("Creating MongoDB indices ...")
  sp_coll$index(add = '{"spectrum_id_": 1}')
  sp_coll$index(add = '{"precursor_mz": 1}')
  sp_coll$index(add = '{"msLevel": 1}')

  pk_coll$index(add = '{"spectrum_id_": 1}')
  message("Indices created")
  TRUE
}

#' Insert or update spectra into MongoDB backend (scalars + peaks)
#'
#' insert spectra data to ms_spectrum_coll and peaks go to ms_peaks_coll.
#'
#' @param dbcon A list of MongoDB connections (must include ms_spectrum_coll
#'        and ms_peaks_coll)
#'
#' @param df A data.frame containing at least spectrum_id_ and peaks
#'
#' @return A list with last spectrum_id_ inserted/updated
#'
#' @author Ahlam Mentag
#'
#' @noRd
.insert_new_backend_mongo <- function(dbcon, df) {
  if (!(is.data.frame(df) | inherits(df, "DataFrame")))
    stop("df must be a 'data.frame' or a 'DataFrame'")

  if (!"spectrum_id_" %in% names(df))
    stop("df must contain a 'spectrum_id_' column")

  if (!"peaks" %in% names(df))
    stop("df must contain a 'peaks' column")

  for (i in seq_len(nrow(df))) {
    sid <- df$spectrum_id_[i]

    scalar_list <- as.list(df[i, setdiff(names(df), "peaks"), drop = FALSE])
    scalar_list <- lapply(scalar_list, function(v) if (length(v) == 1)
                   unlist(v) else v)

    dbcon$ms_spectrum_coll$update(
      query = jsonlite::toJSON(list(spectrum_id_ = sid), auto_unbox = TRUE),
      update = jsonlite::toJSON(list('$set' = scalar_list), auto_unbox = TRUE),
      upsert = TRUE
    )

    peaks <- df$peaks[[i]]
    if (!all(c("mz", "intensity") %in% names(peaks)))
      stop("Each peaks element must have 'mz' and 'intensity'")

    peaks_doc <- list(
      spectrum_id_ = sid,
      mz = as.numeric(peaks$mz),
      intensity = as.numeric(peaks$intensity)
    )

    dbcon$ms_peaks_coll$update(
      query = jsonlite::toJSON(list(spectrum_id_ = sid), auto_unbox = TRUE),
      update = jsonlite::toJSON(list('$set' = peaks_doc), auto_unbox = TRUE),
      upsert = TRUE
    )
  }

  invisible(TRUE)
}

#' Convert Spectra or data.frame and insert into new MongoDB backend
#'
#' @param dbcon A list of Mongo connections (must include ms_spectrum_coll
#'        and ms_peaks_coll)
#'
#' @param x Spectra object or data.frame
#'
#' @return List with last spectrum_id_ inserted/updated
#'
#' @author Ahlam Mentag
#'
#' @noRd
.createMsBackendMongoDb <- function(dbcon, x) {

  if (!is.list(dbcon) || !all(sapply(dbcon, inherits, "mongo")))
    stop("'dbcon' must be a list of mongo connection objects.")

  if (is.null(x))
    stop("Input x must be a Spectra object or a data.frame.")

  # Convert Spectra to data.frame if needed
  if (inherits(x, "Spectra")) {
    x <- spectraData(x, columns = union(spectraVariables(x), peaksVariables(x)))
  }

  if (!(is.data.frame(x) | inherits(x, "DataFrame")))
    stop("Input x must be a data.frame or Spectra object.")

  x <- .reformat_mz_intensity(x)

  # Normalize peaks ... MAYBE NOT NEEDED?
  x$peaks <- lapply(x$peaks, function(p) {
    list(
      mz = as.numeric(p$mz),
      intensity = as.numeric(p$intensity)
    )
  })

  existing_ids <- character(0)
  if (dbcon$ms_spectrum_coll$count('{}') > 0) {
    existing_ids <- dbcon$ms_spectrum_coll$distinct("spectrum_id_")
  }

  n_existing <- length(existing_ids)
  n_new <- nrow(x)

  x$spectrum_id_ <- paste0("spec", seq_len(n_new) + n_existing)

  if (!is.data.frame(x)) x <- as.data.frame(x)
  .insert_new_backend_mongo(dbcon, x)
}

#' Small helper function to reformat eventually present "mz" and "intensity"
#' columns in the format stored into the MongoDB.
#'
#' @author Johannes Rainer
#'
#' @noRd
.reformat_mz_intensity <- function(x) {
    if (any(have <- c("mz", "intensity") %in% colnames(x))) {
        if (!all(have)) stop("Both \"ms\" and \"intensity\" columns ",
                             "need to be provided")
        x$peaks <- mapply(x$mz, x$intensity,
                          FUN = function(a, b) list(mz = a, intensity = b),
                          SIMPLIFY = FALSE, USE.NAMES = FALSE)
        x$mz <- NULL
        x$intensity <- NULL
    } else {
        if (!any(colnames(x) == "peaks")) {
            ep <- list(mz = numeric(), intensity = numeric())
            x$peaks <- lapply(seq_len(nrow(x)), function(z) ep)
        }
    }
    x
}

#' @title Combine Multiple MsBackendMongoDb Backend Objects
#'
#' @description
#'
#' Merges multiple MongoDb backend instances into a single backend,
#' provided they share the same MongoDb connection.
#'
#' @param backends A list of `MsBackendMongoDb` objects.
#'
#' @return A combined `MsBackendMongoDb` instance.
#'
#' @author Ahlam Mentag
#'
#' @noRd
.combine_mongo <- function(backends) {
  if (!all(sapply(backends, inherits, "MsBackendMongoDb")))
    stop("All backends must be MsBackendMongoDb")

  if (length(backends) == 1) return(backends[[1]])

  dbcon_list <- lapply(backends, function(x) x@dbcon)
  if (!all(sapply(dbcon_list, function(x) identical(x, dbcon_list[[1]]))))
    stop("All backends must have the same connection")

  dbcon <- dbcon_list[[1]]

  # Combine character spectraIds
  combined_ids <- unlist(lapply(backends, function(x) x@spectraIds))
  n <- length(combined_ids)

  # Combine spectraVariables
  combined_vars <- unique(unlist(lapply(backends,
                                        function(x) x@spectraVariables),
                                                    use.names = FALSE))

  combined_localData <- data.frame(dummy = rep(NA_integer_, n))

  # Use peak_fun from first backend
  combined_peak_fun <- backends[[1]]@peak_fun

  new("MsBackendMongoDb",
      dbcon = dbcon,
      spectraIds = combined_ids,
      id_map = combined_ids,
      nspectra = n,
      localData = combined_localData,
      .collections = dbcon,
      peak_fun = combined_peak_fun,
      spectraVariables = combined_vars)
}
