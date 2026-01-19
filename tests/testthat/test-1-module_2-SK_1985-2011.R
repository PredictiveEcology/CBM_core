
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

      cohortDT          = data.table::fread(file.path(spadesTestPaths$testdata, "SK/input", "cohortDT.csv"))[, ageSpinup := sapply(age, max, 3)],
      standDT           = data.table::fread(file.path(spadesTestPaths$testdata, "SK/input", "standDT.csv"))[, area := 900],
      disturbanceEvents = file.path(spadesTestPaths$testdata, "SK/input", "disturbanceEvents.csv") |> data.table::fread(),
      disturbanceMeta   = file.path(spadesTestPaths$testdata, "SK/input", "disturbanceMeta.csv")   |> data.table::fread(),
      gcMeta            = file.path(spadesTestPaths$testdata, "SK/input", "gcMeta.csv")            |> data.table::fread(),
      growth_increments = file.path(spadesTestPaths$testdata, "SK/input", "growth_increments.csv") |> data.table::fread(),
      spinupSQL         = file.path(spadesTestPaths$testdata, "SK/input", "spinupSQL.csv")         |> data.table::fread()
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
    data.table::fread(file.path(spadesTestPaths$testdata, "SK/valid", "emissionsProducts.csv"))[
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
  expect_in(
    subset(
      simTest$cbm_vars$parameters,
      row_idx %in% subset(simTest$cbm_vars$key, pixelIndex %in% pixelSPUs$`27`)$row_idx
    )$mean_annual_temperature,
    simTest$spinupSQL[id == 27,]$mean_annual_temperature)
  expect_in(
    subset(
      simTest$cbm_vars$parameters,
      row_idx %in% subset(simTest$cbm_vars$key, pixelIndex %in% pixelSPUs$`28`)$row_idx
    )$mean_annual_temperature,
    simTest$spinupSQL[id == 28,]$mean_annual_temperature)

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
    data.table::fread(file.path(spadesTestPaths$testdata, "SK/valid", "NPP.csv"))
  )
})


