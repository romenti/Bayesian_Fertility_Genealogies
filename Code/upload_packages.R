rm(list = ls())

library_upload = function (package1, ...) {
  packages = c(package1, ...)
  for (package in packages) {
    if (package %in% rownames(installed.packages())) {
      suppressPackageStartupMessages( do.call(library, list(package)) )
      print(paste("library2:",package, "loaded."))
    }
    else {
      tryCatch({
        install.packages(package)
        suppressPackageStartupMessages( do.call(library, list(package)) )
      }, error = function(e) {
      })
    }
  }
}

packages  = c("here", "data.table", "tidyverse", "reshape2","mapdata",
              "purrr","h2o","readxl","cowplot","kableExtra","hrbrthemes",
              "plotly","LearnBayes","assertthat","readr","babynames","viridis",
              "zoo",'corrr','rstan','nimble','nimbleHMC','splines',
              'MCMCvis','parallel','mgcv','patchwork','ggthemes')

library_upload(packages)

