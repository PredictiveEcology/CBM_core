
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module: SK-small 1998-2000", {

  ## Run simInit and spades ----

  # Set up project
  projectName <- "module_SK-small_1998-2000"
  times       <- list(start = 1998, end = 2000)

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
      params = list(CBM_core = list(.saveSpinup = TRUE, .saveAll = TRUE, .plot = FALSE)),

      standDT           = file.path(spadesTestPaths$testdata, "SK-small/input", "standDT.qs2")  |> qs2::qs_read(),
      cohortDT          = file.path(spadesTestPaths$testdata, "SK-small/input", "cohortDT.qs2") |> qs2::qs_read(),
      disturbanceMeta   = file.path(spadesTestPaths$testdata, "SK-small/input", "disturbanceMeta.qs2")   |> qs2::qs_read(),
      disturbanceEvents = file.path(spadesTestPaths$testdata, "SK-small/input", "disturbanceEvents.qs2") |> qs2::qs_read(),
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


  ## Check inputs ----

  ## Check that input tables are not altered by module.
  expect_equal(
    simTest$cohortDT,
    qs2::qs_read(file.path(spadesTestPaths$testdata, "SK-small/input", "cohortDT.qs2")))
  expect_equal(
    simTest$standDT,
    qs2::qs_read(file.path(spadesTestPaths$testdata, "SK-small/input", "standDT.qs2")))
  expect_equal(
    simTest$disturbanceEvents,
    qs2::qs_read(file.path(spadesTestPaths$testdata, "SK-small/input", "disturbanceEvents.qs2")))
  expect_equal(
    simTest$disturbanceMeta,
    qs2::qs_read(file.path(spadesTestPaths$testdata, "SK/input", "disturbanceMeta.qs2")))
  expect_equal(
    simTest$gcMeta,
    qs2::qs_read(file.path(spadesTestPaths$testdata, "SK/input", "gcMeta.qs2")))
  expect_equal(
    simTest$gcIncrements,
    qs2::qs_read(file.path(spadesTestPaths$testdata, "SK/input", "gcIncrements.qs2")))


  ## Check outputs ----

  # emissionsProducts
  expect_true(!is.null(simTest$emissionsProducts))
  expect_equal(
    data.table::as.data.table(simTest$emissionsProducts),
    qs2::qs_read(file.path(spadesTestPaths$testdata, "SK-small/valid", "emissionsProducts.qs2"))[
      , .SD, .SDcols = colnames(simTest$emissionsProducts)],
    check.attributes = FALSE)

  # Cohort data
  ## There should always be the same number of total cohort groups.
  expect_true(!is.null(simTest$cbm_vars$key))
  expect_identical(simTest$cbm_vars$key$cohortID,   simTest$cohortDT$cohortID)
  expect_identical(simTest$cbm_vars$key$pixelIndex, simTest$cohortDT$pixelIndex)
  expect_equal(max(simTest$cbm_vars$key$row_idx),            43)
  expect_equal(length(unique(simTest$cbm_vars$key$row_idx)), 43)
  expect_equal(nrow(simTest$cbm_vars$parameters),            43)
  expect_equal(nrow(simTest$cbm_vars$state),                 43)
  expect_equal(nrow(simTest$cbm_vars$flux),                  43)
  expect_equal(nrow(simTest$cbm_vars$pool),                  43)

  # Check sw_hw flag
  cohortSW <- merge(simTest$cohortDT, simTest$gcMeta, by = "gcids") |>
    merge(simTest$cbm_vars$key[, .(cohortID, row_idx)], by = "cohortID") |>
    merge(simTest$cbm_vars$state[, .(row_idx, sw_hw)], by = "row_idx")
  expect_equal(unique(subset(cohortSW, sw_hw.x == "sw")$sw_hw.y), 0)
  expect_equal(unique(subset(cohortSW, sw_hw.x == "hw")$sw_hw.y), 1)

  # Check saved data
  outDataDir   <- file.path(simTest$spadesCBMdb, "data")
  validDataDir <- file.path(spadesTestPaths$testdata, "SK-small/valid/cbm_vars")

  for (year in times$start:times$end){
    expect_equal(
      qs2::qd_read(file.path(outDataDir,   paste0(year, "_key.qs2")))[, .(cohortID, pixelIndex, row_idx)],
      qs2::qd_read(file.path(validDataDir, paste0(year, "_key.qs2")))[, .(cohortID, pixelIndex, row_idx)]
    )
    for (table in c("parameters", "state", "flux", "pools")){
      tblValid <- qs2::qd_read(file.path(outDataDir,   paste0(year, "_", table, ".qs2")))
      expect_equal(
        qs2::qd_read(file.path(outDataDir,   paste0(year, "_", table, ".qs2")))[, .SD, .SDcols = names(tblValid)],
        tblValid
      )
    }
  }
})


