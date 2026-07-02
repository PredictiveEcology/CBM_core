
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module: SK 1985-2011", {

  ## Run simInit and spades ----

  testthat::skip_on_ci()

  # Set up project
  projectName <- "module_SK_1985-2011"
  times       <- list(start = 1985, end = 2011)

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

    params = list(CBM_core = list(
      .chunk_size   = 100,
      .useCacheCBM4 = FALSE,
      .plot         = FALSE
    )),

    masterRaster = terra::rast(
      crs  = "EPSG:3979",
      ext  = c(xmin = -710000, xmax = -651500, ymin = 690000, ymax = 747000),
      res  = 30,
      vals = 1L
    ),
    standDT           = file.path(paths$testdata, "SK/input", "standDT.qs2")           |> qs2::qs_read() |> data.table::as.data.table(),
    cohortDT          = file.path(paths$testdata, "SK/input", "cohortDT.qs2")          |> qs2::qs_read() |> data.table::as.data.table(),
    disturbanceMeta   = file.path(paths$testdata, "SK/input", "disturbanceMeta.qs2")   |> qs2::qs_read() |> data.table::as.data.table(),
    disturbanceEvents = file.path(paths$testdata, "SK/input", "disturbanceEvents.qs2") |> qs2::qs_read() |> data.table::as.data.table(),
    gcMeta            = file.path(paths$testdata, "SK/input", "gcMeta.qs2")            |> qs2::qs_read() |> data.table::as.data.table(),
    gcIncrements      = file.path(paths$testdata, "SK/input", "gcIncrements.qs2")      |> qs2::qs_read() |> data.table::as.data.table()
  )

  # Run simInit
  simTestInit <- SpaDES.core::simInit2(simInitInput)
  expect_s4_class(simTestInit, "simList")

  # Run spades
  simTest <- SpaDES.core::spades(simTestInit)
  expect_s4_class(simTest, "simList")


  ## Check outputs ----

  testResults <- list(emissionsProducts = simTest$emissionsProducts)
  cbm4_results <- CBM4r::cbm4_results_processor(simTest$CBM4data)
  for (view_name in c("pool_indicators", "flux_indicators", "disturbance_indicators")){
    testResults[[view_name]] <- CBM4r::cbm4_results_totals(cbm4_results, view_name)
  }
  for (table in names(testResults)){
    testValid <- data.table::fread(file.path(spadesTestPaths$testdata, "SK", "valid", paste0(table, ".csv")))
    expect_equal(names(testResults[[table]]), names(testValid))
    expect_equal(testResults[[table]], testValid, scale = 1, tolerance = 0.001, check.attributes = FALSE)
  }
})


