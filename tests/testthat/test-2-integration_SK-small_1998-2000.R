
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Multi module: SK-small 1998-2000", {

  ## Run simInit and spades ----

  # Set times
  times <- list(start = 1998, end = 2000)

  # Set project path
  projectPath <- file.path(spadesTestPaths$temp$projects, "integration_SK-small_1998-2000")
  dir.create(projectPath)
  withr::local_dir(projectPath)

  # Set Github repo branch
  if (!nzchar(Sys.getenv("BRANCH_NAME"))) withr::local_envvar(BRANCH_NAME = "development")

  # Set up project
  simInitInput <- SpaDEStestMuffleOutput(

    SpaDES.project::setupProject(

      modules = c(
        paste0("PredictiveEcology/CBM_defaults@",       Sys.getenv("BRANCH_NAME")),
        paste0("PredictiveEcology/CBM_dataPrep_SK@",    Sys.getenv("BRANCH_NAME")),
        paste0("PredictiveEcology/CBM_dataPrep@",       Sys.getenv("BRANCH_NAME")),
        paste0("PredictiveEcology/CBM_vol2biomass_SK@", Sys.getenv("BRANCH_NAME")),
        "CBM_core"
      ),

      times   = times,
      paths   = list(
        projectPath = projectPath,
        modulePath  = spadesTestPaths$temp$modules,
        packagePath = spadesTestPaths$packagePath,
        inputPath   = spadesTestPaths$inputPath,
        cachePath   = spadesTestPaths$cachePath,
        outputPath  = file.path(projectPath, "outputs")
      ),
      params = list(CBM_core = list(.plot = FALSE)),

      require = "terra",

      masterRaster = terra::rast(
        ext  = c(xmin = -687696, xmax = -681036, ymin = 711955, ymax = 716183),
        res  = 30,
        vals = 0L,
        crs  = "EPSG:3979"
      ),

      outputs = as.data.frame(expand.grid(
        objectName = c("cbmPools", "NPP"),
        saveTime   = sort(c(times$start, times$start + c(1:(times$end - times$start))))
      ))
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

  # # spinupResult ## TEMPORARY: Not currently being saved.
  # expect_true(!is.null(simTest$spinupResult))

  expect_true(!is.null(simTest$cbmPools))

  expect_true(!is.null(simTest$NPP))

  expect_true(!is.null(simTest$emissionsProducts))

})


