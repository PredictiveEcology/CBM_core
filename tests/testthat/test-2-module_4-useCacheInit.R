
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module: .useCache must not include 'init'", {

  ## Set up project ----

  projectName <- "module_useCacheInit"
  times       <- list(start = 2000, end = 2000)

  makeSimInitInput <- function(useCache) SpaDES.project::setupProject(

    modules = "CBM_core",
    times   = times,
    paths   = list(
      projectPath = spadesTestPaths$projectPath,
      modulePath  = spadesTestPaths$modulePath,
      packagePath = spadesTestPaths$packagePath,
      inputPath   = spadesTestPaths$inputPath,
      cachePath   = spadesTestPaths$cachePath,
      outputPath  = file.path(spadesTestPaths$temp$outputs, projectName, make.names(paste(useCache, collapse = "_")))
    ),
    params = list(CBM_core = list(.useCache = useCache))
  )


  ## Test: .useCache = "init" errors before the init event's side effects run ----

  simTestInit <- SpaDES.core::simInit2(makeSimInitInput("init"))
  expect_s4_class(simTestInit, "simList")
  expect_error(SpaDES.core::spades(simTestInit, events = list(CBM_core = c(".inputObjects", "init"))), "init")


  ## Test: .useCache = TRUE also caches the init event, so it errors too ----

  simTestInitAll <- SpaDES.core::simInit2(makeSimInitInput(TRUE))
  expect_s4_class(simTestInitAll, "simList")
  expect_error(SpaDES.core::spades(simTestInitAll, events = list(CBM_core = c(".inputObjects", "init"))), "init")


  ## Test: .useCache = ".inputObjects" does not touch the init event ----

  simTestInitObjs <- SpaDES.core::simInit2(makeSimInitInput(".inputObjects"))
  expect_s4_class(simTestInitObjs, "simList")
  simRunInitObjs <- SpaDES.core::spades(simTestInitObjs, events = list(CBM_core = c(".inputObjects", "init")))
  expect_s4_class(simRunInitObjs, "simList")

})
