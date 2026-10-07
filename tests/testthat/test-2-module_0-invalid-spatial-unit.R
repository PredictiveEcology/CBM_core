
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Expect error: invalid spatial unit", {

  # Set up project
  projectName <- "module_invalid-spatial_unit"
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

    # Northwest Territories x eco_id 3 (Southern Arctic) has no CBM spatial unit.
    # Northwest Territories x eco_id 4 does have a CBM spatial unit.
    cohortDT     = data.table::data.table(cohortID = 1:2, pixelIndex = 1:2, age = 10, gcID = 1:2),
    standDT      = data.table::data.table(pixelIndex = 1:2, area = 1, admin_name = "Northwest Territories", eco_id = 3:4),
    gcMeta       = data.table::data.table(gcID = 1:2, admin_name = "Northwest Territories", eco_id = 3:4, sw = TRUE),
    gcIncrements = data.table::rbindlist(lapply(1:2, function(gcID) data.table::data.table(
      gcID = gcID, age = 0:10, merch_inc = 1, foliage_inc = 1, other_inc = 1)))
  )

  # Run simInit
  simTestInit <- SpaDES.core::simInit2(simInitInput)
  expect_s4_class(simTestInit, "simList")

  # Expect error: no spatial_unit_id found for ecozone 3
  expect_error(
    SpaDES.core::spades(simTestInit),
    "spatial_unit_id|eco_boundary_id")
})


