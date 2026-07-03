## MsBackendMongoDb 0.97

## Changes in version 0.97.4

- Load test data from *MsDataHub*.
- Add functions to start and stop a local MongoDB server (requires the *mongod*
  binary to be available).
- Add examples.

## Changes in version 0.97.3

- `mz()` and `intensity()` to return uncompressed `NumericList`.
- Add a short vignette describing the backend and its properties.
- Ensure core spectra variables are returned with correct data types.

## Changes in version 0.97.2

- Initialize `MsBackendMongoDb` with spectra variables retrieved from the
  database.
- Refactor `spectraData()` to support caching variables.
- Complete and expand unit tests.

## Changes in version 0.97.1

- Refactor mongodb collection: store spectra metadata and peaks data into two
  separate collections.
- Export `connectMsBackendMongoDb()` function.
- Refactor input parameter reformatting for data creation/insertion in MongoDB.

## Changes in version 0.97.0

- Add package related files (DESCRIPTION, NAMESPACE) and render the
  documentation.
- Check the unit tests and identify potential problems.
