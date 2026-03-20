
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

      masterRaster = terra::rast(
        crs  = "EPSG:3979",
        ext  = c(xmin = -710000, xmax = -651500, ymin = 690000, ymax = 747000),
        res  = 30,
        vals = 1L
      ),
      curveID           = "gcids",
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

  # Check outputs
  ## TEMPORARY: just check that the module runs; more assertions will be added later
  expect_true(!is.null(simTest$emissionsProducts))

})


