
if (!testthat::is_testing()){
  suppressPackageStartupMessages(library(testthat))
  testthat::source_test_helpers(env = globalenv())
}

# Set module directory and name
moduleDir  <- normalizePath(test_path("../.."), winslash = "/")
moduleName <- basename(moduleDir)

# Source module functions
lapply(list.files(file.path(moduleDir, "R"), pattern = "\\.R$", full.names = TRUE), source)
for (e in parse(file.path(moduleDir, paste0(moduleName, ".R")))){
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]])) eval(e)
}

# Set temporary output directory
tempDir <- tempfile(moduleName)
if (is_testing()) withr::defer(
  unlink(tempDir, recursive = TRUE),
  envir = teardown_env(), priority = "last")

