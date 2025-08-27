
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
      params = list(CBM_core = list(.plot = FALSE)),

      outputs = as.data.frame(expand.grid(
        objectName = c("cbmPools", "NPP"),
        saveTime   = sort(c(times$start, times$start + c(1:(times$end - times$start))))
      )),

      cohortDT          = data.table::fread(file.path(spadesTestPaths$testdata, "SK-small/input", "cohortDT.csv")),
      standDT           = data.table::fread(file.path(spadesTestPaths$testdata, "SK-small/input", "standDT.csv"))[, area := 900],
      disturbanceEvents = file.path(spadesTestPaths$testdata, "SK-small/input", "disturbanceEvents.csv") |> data.table::fread(),
      disturbanceMeta   = file.path(spadesTestPaths$testdata, "SK/input", "disturbanceMeta.csv")   |> data.table::fread(),
      gcMeta            = file.path(spadesTestPaths$testdata, "SK/input", "gcMeta.csv")            |> data.table::fread(),
      growth_increments = file.path(spadesTestPaths$testdata, "SK/input", "growth_increments.csv") |> data.table::fread(),
      pooldef           = file.path(spadesTestPaths$testdata, "SK/input", "pooldef.txt")           |> readLines(),
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
      , .(simYear = year, Products, Emissions, CO2, CH4, CO)],
    check.attributes = FALSE)

  # # spinupResult ## TEMPORARY: Not currently being saved.
  # expect_true(!is.null(simTest$spinupResult))
  # expect_equal(
  #   data.table::as.data.table(simTest$spinupResult$output$pools),
  #   data.table::fread(file.path(spadesTestPaths$testdata, "SK-small/valid", "spinupResult.csv")),
  #   check.attributes = FALSE)

  # cbmPools
  expect_true(!is.null(simTest$cbmPools))
  for (year in times$start:times$end){
    expect_equal(
      subset(simTest$cbmPools, simYear == year)[, .(row_idx = cohortGroupID, N)][order(row_idx)],
      qs::qread(file.path(spadesTestPaths$testdata, "SK-small/valid/cbm_vars", paste0(year, "_key.qs")))[, .N, by = row_idx][order(row_idx)],
      check.attributes = FALSE)
    expect_equal(
      subset(simTest$cbmPools, simYear == year)[, -c("simYear", "cohortGroupID", "N", "age")],
      qs::qread(file.path(spadesTestPaths$testdata, "SK-small/valid/cbm_vars", paste0(year, "_pools.qs")))[, -c("row_idx", "Products")],
      check.attributes = FALSE)
  }

  # NPP
  expect_true(!is.null(simTest$NPP))
  for (year in times$start:times$end){
    expect_equal(
      subset(simTest$NPP, simYear == year)[, .(row_idx = cohortGroupID, N)][order(row_idx)],
      qs::qread(file.path(spadesTestPaths$testdata, "SK-small/valid/cbm_vars", paste0(year, "_key.qs")))[, .N, by = row_idx][order(row_idx)],
      check.attributes = FALSE)
    expect_equal(
      subset(simTest$NPP, simYear == year)[, .(row_idx = cohortGroupID, NPP)],
      qs::qread(file.path(spadesTestPaths$testdata, "SK-small/valid/cbm_vars", paste0(year, "_flux.qs")))[, .(
        NPP = sum(DeltaBiomass_AG, DeltaBiomass_BG,
                  TurnoverMerchLitterInput, TurnoverFolLitterInput, TurnoverOthLitterInput,
                  TurnoverCoarseLitterInput, TurnoverFineLitterInput)
      ), by = row_idx],
      check.attributes = FALSE)
  }

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

})


