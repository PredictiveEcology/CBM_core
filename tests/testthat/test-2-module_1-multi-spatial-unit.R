
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module: multiple spatial units", {

  # Set up project
  projectName <- "module_multi-spatial_unit"
  times       <- list(start = 2000, end = 2000)

  simInitInput <- SpaDES.project::setupProject(

    modules = "CBM_core",
    times   = times,
    paths   = list(
      projectPath = spadesTestPaths$projectPath,
      modulePath  = spadesTestPaths$modulePath,
      packagePath = spadesTestPaths$packagePath,
      inputPath   = spadesTestPaths$inputPath,
      cachePath   = spadesTestPaths$cachePath,
      outputPath  = file.path(spadesTestPaths$temp$outputs, projectName),
      testdata    = spadesTestPaths$testdata
    ),

    params = list(CBM_core = list(.plots = NA)),

    # 2 cohorts with identical attributes but in 2 different spatial units
    masterRaster = terra::rast(nrows = 2, ncols = 1, res = 1, crs = "local"),
    cohortDT     = data.table::data.table(cohortID = 1:2, pixelIndex = 1:2, age = 10, gcID = 1:2),
    standDT      = data.table::data.table(pixelIndex = 1:2, area = 1, admin_name = "Saskatchewan", eco_id = c(6, 9)),
    gcMeta       = data.table::data.table(gcID = 1:2, admin_name = "Saskatchewan", eco_id = c(6, 9), sw = TRUE),
    gcIncrements = data.table::rbindlist(lapply(1:2, function(gcID) data.table::data.table(
      gcID = gcID, age = 0:10, merch_inc = 1, foliage_inc = 1, other_inc = 1)))
  )

  # Run simInit
  simTestInit <- SpaDES.core::simInit2(simInitInput)
  expect_s4_class(simTestInit, "simList")

  # Run spades
  simTest <- SpaDES.core::spades(simTestInit)
  expect_s4_class(simTest, "simList")

  # Check result
  ## spatial units are set correctly
  ## pools values differ
  expect_equal(nrow(simTest$cbm_vars$state), 2)
  expect_equal(simTest$cbm_vars$state[row_idx == simTest$cbm_vars$key[cohortID == 1, row_idx], spatial_unit_id], 27)
  expect_equal(simTest$cbm_vars$state[row_idx == simTest$cbm_vars$key[cohortID == 2, row_idx], spatial_unit_id], 28)
  expect_equal(nrow(unique(simTest$cbm_vars$pools[, -c("row_idx")])), 2)

})


