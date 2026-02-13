
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module: SK 1985-2011", {

  ## Run simInit and spades ----

  # Set up project
  projectName <- "module_SK_1985-2011"
  times       <- list(start = 1985, end = 2011)

  simInitInput <- SpaDEStestMuffleOutput(

    SpaDES.project::setupProject(

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

      standDT           = file.path(spadesTestPaths$testdata, "SK/input", "standDT.qs2")  |> qs2::qs_read(),
      cohortDT          = file.path(spadesTestPaths$testdata, "SK/input", "cohortDT.qs2") |> qs2::qs_read(),
      disturbanceMeta   = file.path(spadesTestPaths$testdata, "SK/input", "disturbanceMeta.qs2")   |> qs2::qs_read(),
      disturbanceEvents = file.path(spadesTestPaths$testdata, "SK/input", "disturbanceEvents.qs2") |> qs2::qs_read(),
      gcMeta            = file.path(spadesTestPaths$testdata, "SK/input", "gcMeta.qs2")       |> qs2::qs_read(),
      gcIncrements      = file.path(spadesTestPaths$testdata, "SK/input", "gcIncrements.qs2") |> qs2::qs_read()
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


  ## Check outputs ----

  # emissionsProducts
  expect_true(!is.null(simTest$emissionsProducts))
  expect_equal(
    data.table::as.data.table(simTest$emissionsProducts),
    qs2::qs_read(file.path(spadesTestPaths$testdata, "SK/valid", "emissionsProducts.qs2"))[
      , .SD, .SDcols = colnames(simTest$emissionsProducts)],
    check.attributes = FALSE)

  # Cohort data
  ## There should always be the same number of total cohort groups.
  expect_true(!is.null(simTest$cbm_vars$key))
  expect_identical(simTest$cbm_vars$key$cohortID,   simTest$cohortDT$cohortID)
  expect_identical(simTest$cbm_vars$key$pixelIndex, simTest$cohortDT$pixelIndex)
  expect_equal(max(simTest$cbm_vars$key$row_idx),            4401)
  expect_equal(length(unique(simTest$cbm_vars$key$row_idx)), 4354) # Cohort groups eliminated by disturbances
  expect_equal(nrow(simTest$cbm_vars$parameters),            4354)
  expect_equal(nrow(simTest$cbm_vars$state),                 4354)
  expect_equal(nrow(simTest$cbm_vars$flux),                  4354)
  expect_equal(nrow(simTest$cbm_vars$pool),                  4354)

  # Check mean_annual_temperature is correct for each spatial unit
  pixelSPUs <- split(simTest$standDT$pixelIndex, simTest$standDT$spatial_unit_id)
  meanTemps <- merge(
    merge(simTest$cbm_vars$key, simTest$standDT, by = "pixelIndex"),
    simTest$cbm_vars$parameters,
    by = "row_idx")[, .(spatial_unit_id, mean_annual_temperature)] |> unique()
  expect_setequal(meanTemps$spatial_unit_id, c(27, 28))
  expect_true(meanTemps[spatial_unit_id == 27, "mean_annual_temperature"] !=
                meanTemps[spatial_unit_id == 28, "mean_annual_temperature"])

  # Check saved data
  testNPP <- data.table::rbindlist(lapply(times$start:times$end, function(year){
    merge(
      qs2::qd_read(file.path(simTest$spadesCBMdb, "data", paste0(year, "_key.qs2"))),
      qs2::qd_read(file.path(simTest$spadesCBMdb, "data", paste0(year, "_flux.qs2"))),
      by = "row_idx")[, .(
        year = year,
        NPP = sum(DeltaBiomass_AG, DeltaBiomass_BG,
                  TurnoverMerchLitterInput, TurnoverFolLitterInput, TurnoverOthLitterInput,
                  TurnoverCoarseLitterInput, TurnoverFineLitterInput)
      )]
  }))
  expect_equal(
    testNPP,
    qs2::qs_read(file.path(spadesTestPaths$testdata, "SK/valid", "NPP.qs2"))
  )
})


