
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module: with regeneration delay", {

  ## Test: regeneration delay set by cohortDT column ----

  # Set times
  times <- list(start = 2000, end = 2002)

  # Set project path
  projectPath <- file.path(spadesTestPaths$temp$projects, "module_regenDelayCol")
  dir.create(projectPath)
  withr::local_dir(projectPath)

  # Set up project
  simInitInput <- SpaDEStestMuffleOutput(

    SpaDES.project::setupProject(

      modules = "CBM_core",
      times   = times,
      paths   = list(
        projectPath = projectPath,
        modulePath  = spadesTestPaths$modulePath,
        packagePath = spadesTestPaths$packagePath,
        inputPath   = spadesTestPaths$inputPath,
        cachePath   = spadesTestPaths$cachePath,
        outputPath  = file.path(projectPath, "outputs")
      ),
      params = list(CBM_core = list(.plot = FALSE)),

      standDT = data.table::data.table(
        pixelIndex      = c(1, 2),
        spatial_unit_id = 28,
        area            = 900
      ),
      cohortDT = data.table::data.table(
        cohortID   = c(1, 2),
        pixelIndex = c(1, 2),
        gcids      = 1,
        age        = 10,
        delayRegen = c(0, 2)
      ),
      disturbanceEvents = data.table::data.table(
        pixelIndex = c(1, 2),
        year       = 2000,
        eventID    = 1
      ),
      disturbanceMeta = data.table::data.table(
        eventID = 1,
        disturbance_type_id = 1
      ),
      gcMeta = data.table::data.table(
        gcids      = 1,
        species_id = 1,
        sw_hw      = "sw"
      ),
      growth_increments = data.table::data.table(
        gcids       = 1,
        age         = 0:100,
        merch_inc   = c(0, seq(0.01, 1, length.out = 100)),
        foliage_inc = c(0, seq(0.01, 1, length.out = 100)),
        other_inc   = c(0, seq(0.01, 1, length.out = 100))
      ),
      pooldef   = file.path(spadesTestPaths$testdata, "SK/input", "pooldef.txt")   |> readLines(),
      spinupSQL = file.path(spadesTestPaths$testdata, "SK/input", "spinupSQL.csv") |> data.table::fread()
    )
  )

  # Run simInit
  simTestInit <- SpaDEStestMuffleOutput(
    SpaDES.core::simInit2(simInitInput)
  )
  expect_s4_class(simTestInit, "simList")

  # Run spades
  simTest <- SpaDEStestMuffleOutput(
    SpaDES.core::spades(simTestInit)
  )
  expect_s4_class(simTest, "simList")

  # Check result
  cohortGroupNPP   <- simTest@.envir$.mods$CBM_core$simCohortGroupNPP(simTest)
  cohortGroupPools <- simTest@.envir$.mods$CBM_core$simCohortGroupPools(simTest)

  expect_equal(subset(cohortGroupPools, cohortGroupID == min(cohortGroupID))$age, c(1, 2, 3))
  expect_equal(subset(cohortGroupPools, cohortGroupID == max(cohortGroupID))$age, c(0, 0, 1))
  expect_true(all(
    subset(cohortGroupNPP, cohortGroupID == 2)$NPP < subset(cohortGroupNPP, cohortGroupID == 1)$NPP
  ))


  ## Test: regeneration delay set by parameter ----

  # Set project path
  projectPath <- file.path(spadesTestPaths$temp$projects, "module_regenDelayParam")
  dir.create(projectPath)
  withr::local_dir(projectPath)

  # Set up project
  simInitInputParam <- simInitInput
  simInitInputParam$params$CBM_core$default_delay_regen <- 2
  simInitInputParam$cohortDT$delayRegen <- NULL

  # Run simInit
  simTestInitParam <- SpaDEStestMuffleOutput(
    SpaDES.core::simInit2(simInitInputParam)
  )
  expect_s4_class(simTestInitParam, "simList")

  # Run spades
  simTestParam <- SpaDEStestMuffleOutput(
    SpaDES.core::spades(simTestInitParam)
  )
  expect_s4_class(simTestParam, "simList")

  # Check result
  cohortGroupNPP   <- simTest@.envir$.mods$CBM_core$simCohortGroupNPP(simTest)
  cohortGroupPools <- simTest@.envir$.mods$CBM_core$simCohortGroupPools(simTest)

  expect_equal(cohortGroupPools$age, c(0, 0, 1))
  expect_equal(
    cohortGroupPools[, .SD, .SDcols = !c("cohortGroupID", "N")],
    subset(cohortGroupPools, cohortGroupID == max(cohortGroupID))[, .SD, .SDcols = !c("cohortGroupID", "N")],
    check.attributes = FALSE)
  expect_equal(
    cohortGroupNPP[, .SD, .SDcols = !c("cohortGroupID", "N")],
    subset(cohortGroupNPP, cohortGroupID == max(cohortGroupID))[, .SD, .SDcols = !c("cohortGroupID", "N")],
    check.attributes = FALSE)

})


