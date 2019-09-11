[![Appveyor build Status](https://ci.appveyor.com/api/projects/status/github/KWB-R/kwb.flusshygiene?branch=master&svg=true)](https://ci.appveyor.com/project/KWB-R/kwb-flusshygiene/branch/master)
[![Travis build Status](https://travis-ci.org/KWB-R/kwb.flusshygiene.svg?branch=master)](https://travis-ci.org/KWB-R/kwb.flusshygiene)
[![codecov](https://codecov.io/github/KWB-R/kwb.flusshygiene/branch/master/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.flusshygiene)
[![Project Status](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.flusshygiene)]()

# kwb.flusshygiene

Easy and transferable functions for creating and managing 
models for hygiene data in rivers. This package is developed within the 
FLUSSHYGIENE project. 
See https://bmbf.nawam-rewam.de/en/projekt/flusshygiene/ for details.

## Installation

For details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# Install KWB package 'kwb.flusshygiene' from GitHub

remotes::install_github("KWB-R/kwb.flusshygiene")
```

## Documentation

Release: [https://kwb-r.github.io/kwb.flusshygiene](https://kwb-r.github.io/kwb.flusshygiene)

Development: [https://kwb-r.github.io/kwb.flusshygiene/dev](https://kwb-r.github.io/kwb.flusshygiene/dev)
