# NoSQL-based Mass Spectrometry Data Backend

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check-bioc](https://github.com/RforMassSpectrometry/MsBackendMongoDb/workflows/R-CMD-check-bioc/badge.svg)](https://github.com/RforMassSpectrometry/MsBackendMongoDb/actions?query=workflow%3AR-CMD-check-bioc)
[![codecov](https://codecov.io/gh/rformassspectrometry/MsBackendMongoDb/graph/badge.svg?token=O6Cpv5YOSU)](https://codecov.io/gh/rformassspectrometry/MsBackendMongoDb)
[![:name status badge](https://rformassspectrometry.r-universe.dev/badges/:name)](https://rformassspectrometry.r-universe.dev/)
[![license](https://img.shields.io/badge/license-GPL--3.0-brightgreen.svg)](https://opensource.org/license/gpl-3.0)

## Welcome to *MsBackendMongoDb*!

This repository provides *backends* for
[Spectra](https://github.com/RforMassSpectrometry/Spectra) objects to store and
retrieve mass spectrometry (MS) data to/from [MongoDB](https://www.mongodb.com/)
NoSQL databases.

## Installation

The package can be installed with

```r
install.packages("remotes")
install.packages("BiocManager")
BiocManager::install("RforMassSpectrometry/MsBackendMongoDb")
```

For usage, access to a remote or local MongoDB database is required. See the
official [MongoDB web site](https://www.mongodb.com) for more information.

-------------------------------------------------------------------------------

## 🤝 Contribution

Contributions are highly welcome and should follow the [contribution
guidelines](https://rformassspectrometry.github.io/RforMassSpectrometry/articles/RforMassSpectrometry.html#contributions).
Also, please check the coding style guidelines in the [RforMassSpectrometry
vignette](https://rformassspectrometry.github.io/RforMassSpectrometry/articles/RforMassSpectrometry.html).

### 📜 Code of Conduct

We follow the [**RforMassSpectrometry Code of
Conduct**](https://rformassspectrometry.github.io/RforMassSpectrometry/articles/RforMassSpectrometry.html#code-of-conduct)
to maintain an inclusive and respectful community.

## License

This package is licensed under the **GPL 3.0 License**:
📄 [https://opensource.org/license/gpl-3.0](https://opensource.org/license/gpl-3.0)

Documentation (manuals, vignettes) is licensed under **CC BY-NC-SA 4.0**:
📄 [https://creativecommons.org/licenses/by-nc-sa/4.0/](https://creativecommons.org/licenses/by-nc-sa/4.0/)
