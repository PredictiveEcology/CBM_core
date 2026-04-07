
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module: with regeneration delay", {

  ## Test: regeneration delay set by cohortDT column ----

  # Set up project
  projectName <- "module_regenDelayCol"
  times       <- list(start = 2000, end = 2002)

  simInitInput <- SpaDES.project::setupProject(

    modules = "CBM_core",
    times   = times,
    paths   = list(
      projectPath = spadesTestPaths$projectPath,
      modulePath  = spadesTestPaths$modulePath,
      packagePath = spadesTestPaths$packagePath,
      inputPath   = spadesTestPaths$inputPath,
      cachePath   = spadesTestPaths$cachePath,
      outputPath  = file.path(spadesTestPaths$temp$outputs, projectName)
    ),

    params = list(CBM_core = list(.plot = FALSE)),

    masterRaster = terra::rast(ncol = 1, nrow = 2, res = 10, crs = "local"),

    standDT = data.table::data.table(
      pixelIndex = c(1, 2),
      admin_name = "Saskatchewan",
      eco_id     = 9,
      area       = 100
    ),
    cohortDT = data.table::data.table(
      cohortID    = c(1, 2),
      pixelIndex  = c(1, 2),
      gcID        = 1L,
      age         = 10,
      delay_regen = c(0, 2)
    ),
    disturbanceMeta = data.table::data.table(
      eventID = 1,
      disturbance_type_id = 1
    ),
    disturbanceEvents = data.table::data.table(
      pixelIndex = c(1, 2),
      year       = 2000,
      eventID    = 1
    ),
    gcMeta = data.table::data.table(
      gcID       = 1L,
      admin_name = "Saskatchewan",
      eco_id     = 9,
      species_id = 1,
      sw_hw      = "sw"
    ),
    gcIncrements = data.table::data.table(
      gcID        = 1L,
      age         = 0:100,
      merch_inc   = c(0, seq(0.01, 1, length.out = 100)),
      foliage_inc = c(0, seq(0.01, 1, length.out = 100)),
      other_inc   = c(0, seq(0.01, 1, length.out = 100))
    )
  )

  # Run simInit
  simTestInit <- SpaDES.core::simInit2(simInitInput)
  expect_s4_class(simTestInit, "simList")

  # Run spades
  simTest <- SpaDES.core::spades(simTestInit)
  expect_s4_class(simTest, "simList")

  # Check result
  simDelay <- CBM4r::cbm4_results_query(
    simTest$CBM4data,
    "SELECT timestep, \"state.age\", \"inventory.delay\", \"state.regeneration_delay\" FROM simulation ORDER BY timestep, index")

  expect_equal(simDelay[seq(1, nrow(simDelay), by = 2),]$state.age,                c(10, 1, 2, 3))
  expect_equal(simDelay[seq(1, nrow(simDelay), by = 2),]$inventory.delay,          c( 0, 0, 0, 0))
  expect_equal(simDelay[seq(1, nrow(simDelay), by = 2),]$state.regeneration_delay, c( 0, 0, 0, 0))

  expect_equal(simDelay[seq(2, nrow(simDelay), by = 2),]$state.age,                c(10, 0, 0, 1))
  expect_equal(simDelay[seq(2, nrow(simDelay), by = 2),]$inventory.delay,          c( 2, 2, 2, 2))
  expect_equal(simDelay[seq(2, nrow(simDelay), by = 2),]$state.regeneration_delay, c( 2, 1, 0, 0))


  ## Test: regeneration delay set by parameter ----

  # Set up project
  simInitInputParam <- simInitInput
  simInitInputParam$params$CBM_core$def_delay_regen <- 2
  simInitInputParam$cohortDT$delay_regen <- NULL

  # Run simInit
  simTestInitParam <- SpaDES.core::simInit2(simInitInputParam)
  expect_s4_class(simTestInitParam, "simList")

  # Run spades
  simTestParam <- SpaDES.core::spades(simTestInitParam)
  expect_s4_class(simTestParam, "simList")

  # Check result
  simDelay <- CBM4r::cbm4_results_query(
    simTest$CBM4data,
    "SELECT timestep, \"state.age\" FROM simulation ORDER BY timestep, index")
  expect_equal(simDelay$state.age, c(10, 0, 0, 1))

})


