
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

      cohortDT          = data.table::fread(file.path(spadesTestPaths$testdata, "SK-small/input", "cohortDT.csv")),
      standDT           = data.table::fread(file.path(spadesTestPaths$testdata, "SK-small/input", "standDT.csv"))[, area := 900],
      disturbanceEvents = file.path(spadesTestPaths$testdata, "SK-small/input", "disturbanceEvents.csv") |> data.table::fread(),
      disturbanceMeta   = file.path(spadesTestPaths$testdata, "SK/input", "disturbanceMeta.csv")   |> data.table::fread(),
      gcMeta            = file.path(spadesTestPaths$testdata, "SK/input", "gcMeta.csv")            |> data.table::fread(),
      growth_increments = file.path(spadesTestPaths$testdata, "SK/input", "growth_increments.csv") |> data.table::fread()
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
  expect_mapequal(
    simTest$cohortDT,
    data.table::fread(file.path(spadesTestPaths$testdata, "SK-small/input", "cohortDT.csv")))
  expect_mapequal(
    simTest$standDT,
    data.table::fread(file.path(spadesTestPaths$testdata, "SK-small/input", "standDT.csv"))[, area := 900])
  expect_mapequal(
    simTest$disturbanceEvents,
    data.table::fread(file.path(spadesTestPaths$testdata, "SK-small/input", "disturbanceEvents.csv")))
  expect_mapequal(
    simTest$disturbanceMeta,
    data.table::fread(file.path(spadesTestPaths$testdata, "SK/input", "disturbanceMeta.csv")))
  expect_mapequal(
    simTest$gcMeta,
    data.table::fread(file.path(spadesTestPaths$testdata, "SK/input", "gcMeta.csv")))
  expect_mapequal(
    simTest$growth_increments,
    data.table::fread(file.path(spadesTestPaths$testdata, "SK/input", "growth_increments.csv")))


  ## Check outputs ----

  # emissionsProducts
  expect_true(!is.null(simTest$emissionsProducts))
  expect_equal(
    data.table::as.data.table(simTest$emissionsProducts),
    data.table::fread(file.path(spadesTestPaths$testdata, "SK-small/valid", "emissionsProducts.csv"))[
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
      expect_equal(
        qs2::qd_read(file.path(outDataDir,   paste0(year, "_", table, ".qs2"))),
        qs2::qd_read(file.path(validDataDir, paste0(year, "_", table, ".qs2")))
      )
    }
  }
})


