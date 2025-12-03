## Test the MsBackendMongoDb backend
library(testthat)
library(MsBackendMongoDb)
library(mongolite)
library(Spectra)
library(msdata)

## Can only run tests if a mongodb server is running on the system.

test_check("MsBackendMongoDb")


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
