
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module: SK-small 1998-2000", {

  ## Run simInit and spades ----

  # Set up project
  projectName <- "module_SK-small_1998-2000"
  times       <- list(start = 1998, end = 2000)

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

    params = list(CBM_core = list(.plot = FALSE)),

    masterRaster = terra::rast(
      crs  = "EPSG:3979",
      ext  = c(xmin = -687696, xmax = -681036, ymin = 711955, ymax = 716183),
      res  = 30,
      vals = 1L
    ),
    standDT           = file.path(paths$testdata, "SK-small/input", "standDT.qs2")           |> qs2::qs_read() |> data.table::as.data.table(),
    cohortDT          = file.path(paths$testdata, "SK-small/input", "cohortDT.qs2")          |> qs2::qs_read() |> data.table::as.data.table(),
    disturbanceMeta   = file.path(paths$testdata, "SK-small/input", "disturbanceMeta.qs2")   |> qs2::qs_read() |> data.table::as.data.table(),
    disturbanceEvents = file.path(paths$testdata, "SK-small/input", "disturbanceEvents.qs2") |> qs2::qs_read() |> data.table::as.data.table(),
    gcMeta            = file.path(paths$testdata, "SK/input",       "gcMeta.qs2")            |> qs2::qs_read() |> data.table::as.data.table(),
    gcIncrements      = file.path(paths$testdata, "SK/input",       "gcIncrements.qs2")      |> qs2::qs_read() |> data.table::as.data.table()
  )

  # Run simInit
  simTestInit <- SpaDES.core::simInit2(simInitInput)
  expect_s4_class(simTestInit, "simList")

  # Run spades
  simTest <- SpaDES.core::spades(simTestInit)
  expect_s4_class(simTest, "simList")


  ## Check inputs ----

  ## Check that input tables are not altered by module.
  refTables <- list(
    standDT           = qs2::qs_read(file.path(spadesTestPaths$testdata, "SK-small/input", "standDT.qs2")),
    cohortDT          = qs2::qs_read(file.path(spadesTestPaths$testdata, "SK-small/input", "cohortDT.qs2")),
    disturbanceMeta   = qs2::qs_read(file.path(spadesTestPaths$testdata, "SK/input",       "disturbanceMeta.qs2")),
    disturbanceEvents = qs2::qs_read(file.path(spadesTestPaths$testdata, "SK-small/input", "disturbanceEvents.qs2")),
    gcMeta            = qs2::qs_read(file.path(spadesTestPaths$testdata, "SK/input",       "gcMeta.qs2")),
    gcIncrements      = qs2::qs_read(file.path(spadesTestPaths$testdata, "SK/input",       "gcIncrements.qs2"))
  )
  outTables <- lapply(setNames(names(refTables), names(refTables)), function(table) simTest[[table]]) |>
    lapply(data.table::copy) |> lapply(data.table::setindex, NULL)

  expect_equal(outTables$standDT[, .SD, .SDcols = names(refTables$standDT)], refTables$standDT) # Columns are added
  expect_equal(outTables$cohortDT,          refTables$cohortDT)
  expect_equal(outTables$disturbanceMeta,   refTables$disturbanceMeta)
  expect_equal(outTables$disturbanceEvents, refTables$disturbanceEvents)
  expect_equal(outTables$gcMeta,            refTables$gcMeta)
  expect_equal(outTables$gcIncrements,      refTables$gcIncrements)


  ## Check outputs ----

  testResults <- list(
    emissionsProducts = simTest$emissionsProducts,
    pools = CBM4r::cbm4_results_totals(simTest$CBM4data, "pool_indicators"),
    flux  = CBM4r::cbm4_results_totals(simTest$CBM4data, "flux_indicators")
  )
  testValid <- lapply(setNames(names(testResults), names(testResults)), function(table){
    data.table::fread(file.path(spadesTestPaths$testdata, "SK-small", "valid", paste0(table, ".csv")))
  })
  for (table in names(testResults)){
    expect_equal(names(testResults[[table]]), names(testValid[[table]]))
    expect_equal(testResults[[table]], testValid[[table]], scale = 1, tolerance = 0.001, check.attributes = FALSE)
  }


  ## Run with fixedCohorts = FALSE ----

  # Set up project
  simInitInputUnfixed <- SpaDES.project::setupProject(

    modules = "CBM_core",
    times   = times,
    paths   = list(
      projectPath = spadesTestPaths$projectPath,
      modulePath  = spadesTestPaths$modulePath,
      packagePath = spadesTestPaths$packagePath,
      inputPath   = spadesTestPaths$inputPath,
      cachePath   = spadesTestPaths$cachePath,
      outputPath  = file.path(spadesTestPaths$temp$outputs, paste0(projectName, "_unfixed")),
      testdata    = spadesTestPaths$testdata
    ),

    params = list(CBM_core = list(.plot = FALSE, fixedCohorts = FALSE)),

    masterRaster = terra::rast(
      crs  = "EPSG:3979",
      ext  = c(xmin = -687696, xmax = -681036, ymin = 711955, ymax = 716183),
      res  = 30,
      vals = 1L
    ),
    standDT           = file.path(paths$testdata, "SK-small/input", "standDT.qs2")           |> qs2::qs_read() |> data.table::as.data.table(),
    cohortDT          = file.path(paths$testdata, "SK-small/input", "cohortDT.qs2")          |> qs2::qs_read() |> data.table::as.data.table(),
    disturbanceMeta   = file.path(paths$testdata, "SK-small/input", "disturbanceMeta.qs2")   |> qs2::qs_read() |> data.table::as.data.table(),
    disturbanceEvents = file.path(paths$testdata, "SK-small/input", "disturbanceEvents.qs2") |> qs2::qs_read() |> data.table::as.data.table(),
    gcMeta            = file.path(paths$testdata, "SK/input",       "gcMeta.qs2")            |> qs2::qs_read() |> data.table::as.data.table(),
    gcIncrements      = file.path(paths$testdata, "SK/input",       "gcIncrements.qs2")      |> qs2::qs_read() |> data.table::as.data.table()
  )

  # Run simInit
  simTestInitUnfixed <- SpaDES.core::simInit2(simInitInputUnfixed)
  expect_s4_class(simTestInitUnfixed, "simList")

  # Run spades
  simTestUnfixed <- SpaDES.core::spades(simTestInitUnfixed)
  expect_s4_class(simTestUnfixed, "simList")

  # Check outputs
  testResultsUnfixed <- list(
    emissionsProducts = simTestUnfixed$emissionsProducts,
    pools = CBM4r::cbm4_results_totals(simTestUnfixed$CBM4data, "pool_indicators"),
    flux  = CBM4r::cbm4_results_totals(simTestUnfixed$CBM4data, "flux_indicators")
  )
  for (table in names(testResults)) expect_equal(testResults[[table]], testResultsUnfixed[[table]], scale = 1, tolerance = 0.001)

})


