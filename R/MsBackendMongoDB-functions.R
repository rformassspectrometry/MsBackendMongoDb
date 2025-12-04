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
#' @param spectraIds `Integer()` vector of spectrum IDs stored in MongoDb.
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
MsBackendMongoDb <- function(dbcon = NULL, spectraIds = integer(),
                             collections = list()) {
    spectraIds <- as.integer(spectraIds)
    nspectra <- length(spectraIds)
    localData <- data.frame(dummy = rep(NA_integer_, nspectra))
    ## Set default spectra variables
    spectraVars <- c(
        "spectrum_id", "msLevel", "polarity", "compound_id", "precursor_mz",
        "instrument", "instrument_type", "acquisitionNum", "precScanNum",
        "collision_energy", "predicted", "splash", "dataOrigin", "original_id",
        "peaks"
    )
    new("MsBackendMongoDb",
        dbcon = dbcon,
        spectraIds = spectraIds,
        .collections = collections,
        peak_fun = .fetch_peaks_data_long_mongo,
        localData = localData,
        nspectra = nspectra,
        spectraVariables = spectraVars
        )
}


#' Create a connection to MongoDb
#'
#' @description
#'
#' Creates connections to required MongoDb collections used by the backend.
#'
#' @param db `character()` scalar, name of the MongoDb database.
#'
#' @param url `character()` a MongoDb server URL.
#'
#' @return A named `list()` of `mongo` connection objects.
#'
#' @author Ahlam Mentag
#'
#' @importFrom mongolite mongo
#'
#' @noRd
.connect_mongodb <- function(db = "spectra_db", url = "mongodb://localhost") {
    list(
        compounds_info_coll = mongo(collection = "compounds_info_coll",
                                    db = db, url = url)
    )
}

#' Validate a MongoDb Connection Object
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
    if (is.null(x) || length(x) == 0)
        return("'dbcon' is NULL or empty")

    if (inherits(x, "mongo")) {
        conns <- list(compounds_info_coll = x)
    } else if (is.list(x) && all(sapply(x, inherits, "mongo"))) {
        conns <- x
    } else {
        return("'dbcon' should be a mongolite::mongo object or a list of mongo objects")
    }

    ## Required collections for the mongodb backend
    required_collections <- c("compounds_info_coll")
    existing_collections <- names(conns)

    missing <- setdiff(required_collections, existing_collections)
    if (length(missing))
        return(paste("Missing required collection:",
                     paste(missing, collapse = ", ")))

    ## Test if connection is alive
    ok <- tryCatch({
        conns[[1]]$run('{"ping": 1}')
        TRUE
    }, error = function(e) FALSE)

    if (!ok)
        return("MongoDb connection not responding")

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

#' Normalize Peaks Into a Standard Format
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

    ## Recursively unwrap nested lists until we find "mz" and "intensity"
    while (is.list(peaks) && !all(c("mz", "intensity") %in% names(peaks))) {
        if (length(peaks) == 0) return(list(mz = numeric(),
                                            intensity = numeric()))
        peaks <- peaks[[1]]
    }

    if (is.list(peaks) && all(c("mz", "intensity") %in% names(peaks))) {
        mz <- unlist(peaks$mz, recursive = TRUE)
        intensity <- unlist(peaks$intensity, recursive = TRUE)
        return(list(mz = as.numeric(mz), intensity = as.numeric(intensity)))
    }

    if (is.matrix(peaks)) {
        return(list(mz = as.numeric(peaks[,1]),
                    intensity = as.numeric(peaks[,2])))
    }

    if (is.data.frame(peaks)) {
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
    if (!all(c("mz", "intensity") %in% names(doc)))
        stop("Document missing mz/intensity")
    list(mz = as.numeric(doc$mz), intensity = as.numeric(doc$intensity))
}

#' Insert Spectra Into MongoDb (dynamic fields)
#'
#' @description
#'
#' Inserts individual spectra into a MongoDb collection.
#' Required fields (`spectrum_id`, `msLevel`, `peaks`) must be present.
#' All other available fields in the `Spectra` object are inserted automatically.
#'
#' @param dbcon A MongoDb connection list.
#'
#' @param sp A `Spectra` object.
#'
#' @param collection Character, name of the MongoDb collection.
#'
#' @return Invisibly `TRUE`.
#'
#' @author Ahlam Mentag
#'
#' @importMethodsFrom Spectra peaksData
#'
#' @importMethodsFrom Spectra spectraVariables
#'
#' @importFrom jsonlite toJSON
#'
#' @noRd
.insert_spectra_mongo <- function(dbcon, sp,
                                  collection = "compounds_info_coll") {

    coll <- dbcon[[collection]]

    ## Required fields from spectra object
    required_fields <- c("spectrum_id", "msLevel")

    docs <- lapply(seq_along(sp), function(i) {
        s <- sp[i]
        ## Extract peaks
        peaks_df <- peaksData(s)[[1]]
        peaks_list <- list(list(
            mz = as.numeric(peaks_df[, 1]),
            intensity = as.numeric(peaks_df[, 2])
        ))

        ## Extract all spectraData columns
        vars <- spectraVariables(s)
        s_list <- lapply(vars, function(var) s[[var]])
        names(s_list) <- vars

        ## Ensure required fields exist
        missing_req <- setdiff(required_fields, names(s_list))
        if (length(missing_req) > 0)
            stop("Missing required fields in spectra: ",
                 paste(missing_req, collapse = ", "))

        ## Add/override required fields
        s_list[["spectrum_id"]] <- if ("spectrum_id" %in% names(s_list))
                                       s_list[["spectrum_id"]] else s$spectrum_id
        s_list[["msLevel"]] <- if ("msLevel" %in% names(s_list)) s_list[["msLevel"]]    else s$msLevel
        s_list[["peaks"]] <- peaks_list

        ##Return the document
        s_list
    })

    ## Insert into MongoDb
    json_docs <- vapply(
        docs,
        function(d) toJSON(d, auto_unbox = TRUE),
        FUN.VALUE = character(1)
    )

    coll$insert(json_docs)
    invisible(TRUE)
}

#' Get columns from the msms_spectrum_peak database table (dropping spectrum_id)
#'
#' @param x `MsBackendMongoDb`
#'
#' @author Ahlam Mentag
#'
#' @noRd
.available_peaks_variables_mongo <- function(object_or_dbcon,
                                             collection="compounds_info_coll") {
    if (inherits(object_or_dbcon, "MsBackendMongoDb")) {
        dbcon <- object_or_dbcon@dbcon[[collection]]
    } else {
        dbcon <- object_or_dbcon[[collection]]
    }

    docs <- dbcon$find('{}', fields = '{"peaks":1,"_id":0}', limit = 1)

    if (nrow(docs) == 0) return(character(0))

    p <- docs$peaks[[1]]

    while (is.list(p) && length(p) == 1 && is.list(p[[1]])) {
        p <- p[[1]]
    }

    if (is.list(p) && !is.null(names(p))) {
      return(names(p))
    }

    character(0)
}




#' Fetch Peak Data From MongoDb for a Backend
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
    dbcon <- object@dbcon[["compounds_info_coll"]]
    out <- vector("list", length(object@spectraIds))
    emat <- matrix(numeric(0), ncol = length(columns),
                   dimnames = list(NULL, columns))

    for (i in seq_along(object@spectraIds)) {
        sid <- object@spectraIds[i]

        df <- dbcon$find(sprintf('{"spectrum_id": %s}', sid),
                         fields = '{"peaks":1,"_id":0}')

        if (nrow(df) && !is.null(df$peaks[[1]])) {
            peaks <- .encode_peaks(df$peaks[[1]])
            mz <- as.numeric(peaks$mz)
            intensity <- as.numeric(peaks$intensity)
        } else {
            mz <- numeric(0)
            intensity <- numeric(0)
        }

        ## Choose what to return
        if (all(c("mz","intensity") %in% columns)) {
            out[[i]] <- if (length(mz)) cbind(mz=mz, intensity=intensity) else emat
        } else if (identical(columns, "mz")) {
            out[[i]] <- mz
        } else if (identical(columns, "intensity")) {
            out[[i]] <- intensity
        } else {
            stop("Unknown columns requested.")
        }
    }

    ## Optionally drop matrix dimensions if drop=TRUE and single column
    if (drop && length(columns)==1) {
        out <- lapply(out, function(x) if (is.matrix(x)) as.numeric(x[,1]) else x)
    }

    out
}

#' Retrieve spectraData and peaks from MongoDb backend
#'
#' @param x An MsBackendMongoDb object
#'
#' @param columns Character vector of requested fields
#'
#' @return A DataFrame with requested scalar fields and list-columns for peaks
#'
#' @author Ahlam Mentag
#'
#' @importFrom S4Vectors DataFrame
#'
#' @importFrom S4Vectors List
#'
#' @importFrom methods as
#'
#' @importFrom methods getMethod
#'
#' @noRd
.spectra_data_mongo <- function(x, columns = spectraVariables(x)) {
    ## Start with cached data if available
    res <- getMethod("spectraData", "MsBackendCached")(x, columns = columns)
    if (is.null(res))
        res <- S4Vectors::make_zero_col_DFrame(length(x))

    ## Determine scalar vs peaks columns
    db_cols <- intersect(columns, x@spectraVariables)
    db_cols <- db_cols[!db_cols %in% c("peaks", colnames(res))]  # scalar only
    peaks_col <- intersect(columns, "peaks")

    ## Fetch scalar fields
    if (length(db_cols)) {
        scalars <- .fetch_spectra_data_mongo(x, columns = db_cols)
        res <- cbind(res, as(scalars, "DataFrame"))
    }

    ## Fetch peaks if requested
    if (length(peaks_col)) {
        peaks_raw <- .fetch_peaks_data_long_mongo(
            x, columns = c("mz","intensity"))

        ## Convert each element to a list with mz/intensity
        peaks_data <- lapply(peaks_raw, function(p) {
            if (is.matrix(p)) {
                list(mz = as.numeric(p[,1]), intensity = as.numeric(p[,2]))
            } else if (is.list(p) && all(c("mz","intensity") %in% names(p))) {
                list(mz = as.numeric(p$mz), intensity = as.numeric(p$intensity))
            } else {
                list(mz = numeric(), intensity = numeric())
            }
        })
        res$peaks <- List(peaks_data)
    }

    ## Add missing columns as NA
    missing_cols <- setdiff(columns, colnames(res))
    for (col in missing_cols) res[[col]] <- NA

    ## Preserve requested order
    res <- res[, columns, drop = FALSE]
    res
}


#' Fetch spectraData (scalar + peaks) from MongoDb
#'
#' @param x An MsBackendMongoDb backend
#'
#' @param columns Character vector of requested fields
#'
#' @return A DataFrame with scalar fields and peaks
#'
#' @author Ahlam Mentag
#'
#' @importFrom S4Vectors DataFrame
#'
#' @noRd
.fetch_spectra_data_mongo <- function(x, columns = spectraVariables(x)) {
    if (!inherits(x, "MsBackendMongoDb")) stop("x must be MsBackendMongoDb")

    scalar_cols <- setdiff(columns, "peaks")
    projection_fields <- unique(c("spectrum_id", scalar_cols))
    projection <- paste0("{", paste(sprintf('"%s":1', projection_fields),
                                    collapse = ","), ",\"_id\":0}")
    ## Fetch scalar fields
    docs <- x@dbcon$compounds_info_coll$find(query = "{}", fields = projection)

    ## Normalize scalar fields
    for (col in projection_fields) {
        if (!col %in% names(docs)) {
            docs[[col]] <- NA
        } else if (is.list(docs[[col]])) {
            docs[[col]] <- sapply(docs[[col]], function(v) {
                if (is.list(v) && length(v) == 1) v <- v[[1]]
                if (is.null(v)) NA else v
            }, simplify = TRUE)
        }
    }

    ## Preserve requested column order
    docs <- docs[, intersect(columns, names(docs)), drop = FALSE]
    docs <- docs[, columns, drop = FALSE]

    DataFrame(docs)
}

#' Combine Multiple MsBackendMongoDb Backend Objects
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
    combined_ids <- as.integer(unlist(lapply(
        backends, function(x) x@spectraIds)))
    n <- length(combined_ids)
    combined_vars <- unique(unlist(lapply(backends, function(x)
        x@spectraVariables), use.names = FALSE))
    combined_localData <- data.frame(dummy = rep(NA_integer_, n))
    combined_peak_fun <- backends[[1]]@peak_fun

    new("MsBackendMongoDb",
        dbcon = dbcon,
        spectraIds = combined_ids,
        nspectra = n,
        localData = combined_localData,
        .collections = list(),
        peak_fun = combined_peak_fun,
        spectraVariables = combined_vars)
}

#' @description
#'
#' Inserts all thde spectraData fields plus the peaks list into MongoDb.
#' Peaks are always stored in the nested list format:
#'   `peaks = list(list(mz = [...], intensity = [...]))`.
#'
#' @param dbcon A list of mongo connections (must include `compounds_info_coll`).
#'
#' @param df A data.frame containing at least `spectrum_id`, `msLevel`, and `peaks`.
#'
#' @return A list with one element:
#'   - `spectrum_id`: the last inserted spectrum identifier.
#'
#' @author Ahlam Mentag
#'
#' @noRd
.insert_backend_mongo <- function(dbcon, df) {

    if (!is.data.frame(df))
        stop("df must be a data.frame")

    if (!"spectrum_id" %in% names(df))
      stop("df must contain a 'spectrum_id' column")

    if (!"peaks" %in% names(df))
        stop("df must contain a 'peaks' column")

    ## Convert each row into a document
    docs <- lapply(seq_len(nrow(df)), function(i) {

        ## Convert the whole row to a list of scalar fields
        d <- as.list(df[i, , drop = FALSE])

        ## Ensure scalar fields are MongoDb friendly
        d <- lapply(d, function(v) if (length(v) == 1) unlist(v) else v)

        ##  Normalize peaks into the nested structure
        d$peaks <- list(list(
            mz = as.numeric(df$peaks[[i]]$mz),
            intensity = as.numeric(df$peaks[[i]]$intensity)
        ))

        d
    })

    ## Convert each row document to a JSON string
    json_docs <- vapply(
        docs,
        function(d) toJSON(d, auto_unbox = TRUE),
        FUN.VALUE = character(1)
    )

    ## Insert the documents
    dbcon$compounds_info_coll$insert(json_docs)

    ## SQL-style return value
    list(spectrum_id = max(df$spectrum_id))
}

#' Convert a Spectra or data.frame to MongoDb format and insert it
#'
#' @description
#'
#' Converts a `Spectra` object or spectra-data data.frame into the storage
#' format expected by `.insert_backend_mongo()` and inserts the result.
#'
#' @param dbcon A `list()` of mongo connections to MongoDb.
#'
#' @param x A `Spectra` object or a data.frame containing spectra data.
#'
#' @return `list()` with last `spectrum_id`.
#'
#' @author Ahlam Mentag
#'
#' @importMethodsFrom Spectra spectraData
#'
#' @importMethodsFrom Spectra mz
#'
#' @importMethodsFrom Spectra intensity
#'
#' @noRd
.createMsBackendMongoDb <- function(dbcon, x = NULL) {

    if (!is.list(dbcon) || !all(sapply(dbcon, inherits, "mongo")))
        stop("'dbcon' must be a list of mongo connection objects.")

    if (is.null(x))
        stop("Input x must be a Spectra object or a data.frame.")

    ## If x is a Spectra object
    if (inherits(x, "Spectra")) {

        df <- spectraData(x)

        ## Create peaks list
        df$peaks <- lapply(seq_len(length(x)), function(i) {
            list(
                mz = as.numeric(mz(x)[[i]]),
                intensity = as.numeric(intensity(x)[[i]])
            )
        })

        x <- df
    }

    ## If x is a data.frame
    if (!is.data.frame(x))
        stop("Input x must be a data.frame or Spectra object.")

    if (!"peaks" %in% names(x))
        stop("Data.frame must contain a 'peaks' column.")

    ## Normalize peaks structure (in case user inserted inconsistent format)
    x$peaks <- lapply(x$peaks, function(p) {
        list(
            mz = as.numeric(p$mz),
            intensity = as.numeric(p$intensity)
        )
    })

    ## Call the  insertion helper function
    .insert_backend_mongo(dbcon, x)
}

#' @description
#'
#' Update the field of an existing spectrum_id in the mongoDB connection
#'
#' @param dbcon A `list()` of mongo connections to MongoDb.
#'
#' @param sp A `Spectra` object containing the new spectraData for an existing
#'        spectrum_id.
#'
#' @param collection A `character()` the collection we want to update.
#'
#' @return Invisibly `TRUE`.
#'
#' @author Ahlam Mentag
#'
#' @noRd
.update_spectra_mongo <- function(dbcon, sp,
                                  collection = "compounds_info_coll") {
    coll <- dbcon[[collection]]

    for (i in seq_along(sp)) {
        s <- sp[i]

        ## Extract peaks
        peaks_df <- peaksData(s)[[1]]
        peaks_list <- list(list(
            mz = as.numeric(peaks_df[,1]),
            intensity = as.numeric(peaks_df[,2])
        ))

        ## Convert all metadata fields to a plain list
        vars <- spectraVariables(s)
        s_list <- lapply(vars, function(var) {
            val <- s[[var]]
            ## Ensure atomic values are unlisted
            if (length(val) == 1) val <- unlist(val)
            val
        })
        names(s_list) <- vars

        ## Always update required fields
        s_list$peaks <- peaks_list
        s_list$msLevel <- as.integer(s$msLevel)
        s_list$spectrum_id <- as.integer(s$spectrum_id)

        ## Update MongoDb document (merge existing fields)
        coll$update(
                 query = jsonlite::toJSON(list(spectrum_id = s$spectrum_id),
                                          auto_unbox = TRUE),
                 update = jsonlite::toJSON(list('$set' = s_list),
                                           auto_unbox = TRUE),
                 upsert = TRUE
             )
    }

    invisible(TRUE)
}

#' Create MongoDb Indices for Efficient Spectra Queries
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
.create_indices_mongo <- function(dbcon, collection = "compounds_info_coll") {
    coll <- dbcon[[collection]]
    message("Creating indices in MongoDB ...")
    coll$index(add = '{"spectrum_id": 1}')
    coll$index(add = '{"precursor_mz": 1}')
    coll$index(add = '{"msLevel": 1}')
    message("Indices created succesfuly")
    TRUE
}
