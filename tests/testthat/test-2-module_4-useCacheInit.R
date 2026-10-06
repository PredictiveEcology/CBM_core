
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

    params = list(CBM_core = list(.useCache = useCache, .plot = FALSE)),

    standDT = data.table::data.table(
      pixelIndex = 1,
      admin_name = "Saskatchewan",
      eco_id     = 9,
      area       = 900
    ),
    cohortDT = data.table::data.table(
      cohortID   = 1,
      pixelIndex = 1,
      gcID       = 1,
      age        = 10
    ),
    disturbanceMeta = data.table::data.table(
      eventID = 1,
      disturbance_type_id = 1
    ),
    disturbanceEvents = data.table::data.table(
      pixelIndex = integer(0),
      year       = integer(0),
      eventID    = integer(0)
    ),
    gcMeta = data.table::data.table(
      gcID       = 1,
      admin_name = "Saskatchewan",
      eco_id     = 9,
      sw         = TRUE
    ),
    gcIncrements = data.table::data.table(
      gcID        = 1,
      age         = 0:100,
      merch_inc   = c(0, seq(0.01, 1, length.out = 100)),
      foliage_inc = c(0, seq(0.01, 1, length.out = 100)),
      other_inc   = c(0, seq(0.01, 1, length.out = 100))
    )
  )


  ## Test: .useCache = "init" errors before the init event's side effects run ----

  simTestInit <- SpaDES.core::simInit2(makeSimInitInput("init"))
  expect_s4_class(simTestInit, "simList")
  expect_error(SpaDES.core::spades(simTestInit), "init")


  ## Test: .useCache = TRUE also caches the init event, so it errors too ----

  simTestInitAll <- SpaDES.core::simInit2(makeSimInitInput(TRUE))
  expect_s4_class(simTestInitAll, "simList")
  expect_error(SpaDES.core::spades(simTestInitAll), "init")


  ## Test: .useCache = ".inputObjects" does not touch the init event ----

  simTestInitObjs <- SpaDES.core::simInit2(makeSimInitInput(".inputObjects"))
  expect_s4_class(simTestInitObjs, "simList")
  simRunInitObjs <- SpaDES.core::spades(simTestInitObjs)
  expect_s4_class(simRunInitObjs, "simList")
})
