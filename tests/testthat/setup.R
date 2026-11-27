
if (!testthat::is_testing()){
  suppressPackageStartupMessages(library(testthat))
  testthat::source_test_helpers(env = globalenv())
}

# Set temporary directory
tempDir <- file.path(tempdir(), "testthat-module")
if (is_testing()) withr::defer(unlink(tempDir, recursive = TRUE), envir = teardown_env(), priority = "last")

# Source module functions
if (!as.logical(Sys.getenv("CI", FALSE))){

  moduleDir  <- normalizePath(test_path("../.."), winslash = "/")
  moduleName <- basename(moduleDir)
  pkgs <- SpaDES.core::packages(modules = moduleName, paths = dirname(moduleDir))[[1]]
  Require::Require(pkgs, verbose = FALSE)
  lapply(list.files(file.path(moduleDir, "R"), pattern = "\\.R$", full.names = TRUE), source)
  for (e in parse(file.path(moduleDir, paste0(moduleName, ".R")))){
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]])) eval(e)
  }
}

