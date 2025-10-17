
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Multi module: SK-small 1998-2000", {

  ## Run simInit and spades ----

  # Set up project
  projectName <- "integration_SK-small_1998-2000"
  times       <- list(start = 1998, end = 2000)

  simInitInput <- SpaDEStestMuffleOutput(

    SpaDES.project::setupProject(

      modules = c(
        paste0("PredictiveEcology/CBM_defaults@",       Sys.getenv("BRANCH_NAME", "development")),
        paste0("PredictiveEcology/CBM_dataPrep_SK@",    Sys.getenv("BRANCH_NAME", "development")),
        paste0("PredictiveEcology/CBM_dataPrep@",       Sys.getenv("BRANCH_NAME", "development")),
        paste0("PredictiveEcology/CBM_vol2biomass_SK@", Sys.getenv("BRANCH_NAME", "development")),
        "CBM_core"
      ),

      times   = times,
      paths   = list(
        projectPath = spadesTestPaths$projectPath,
        modulePath  = spadesTestPaths$temp$modules,
        packagePath = spadesTestPaths$packagePath,
        inputPath   = spadesTestPaths$inputPath,
        cachePath   = spadesTestPaths$cachePath,
        outputPath  = file.path(spadesTestPaths$temp$outputs, projectName)
      ),
      params = list(CBM_core = list(.plot = TRUE)),

      require = "terra",

      masterRaster = terra::rast(
        ext  = c(xmin = -687696, xmax = -681036, ymin = 711955, ymax = 716183),
        res  = 30,
        vals = 0L,
        crs  = "EPSG:3979"
      )
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

  expect_true(!is.null(simTest$emissionsProducts))

})


