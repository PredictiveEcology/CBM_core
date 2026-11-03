
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Multi module: SK-small 1985-1986", {

  ## Run simInit and spades ----

  skip_if(Sys.getenv("BRANCH_NAME") != "main")

  # Set up project
  simInitInput <- setupProjectTest(
    projectName = "integration_SK-small_1985-1986",
    times       = list(start = 1985, end = 1986),
    modules     = c(
      paste0("PredictiveEcology/CBM_defaults@",    Sys.getenv("BRANCH_NAME", "development")),
      paste0("PredictiveEcology/CBM_dataPrep_SK@", Sys.getenv("BRANCH_NAME", "development")),
      paste0("PredictiveEcology/CBM_dataPrep@",    Sys.getenv("BRANCH_NAME", "development")),
      paste0("PredictiveEcology/CBM_vol2biomass@", Sys.getenv("BRANCH_NAME", "development")),
      "CBM_core"
    ),

    params = list(.globals = list(.plots = NA)),

    masterRaster = terra::rast(
      ext  = c(xmin = -687696, xmax = -681036, ymin = 711955, ymax = 716183),
      res  = 30,
      vals = 0L,
      crs  = "EPSG:3979"
    )
  )

  # Run simInit
  simTestInit <- suppressWarnings(SpaDES.core::simInit2(simInitInput))
  expect_s4_class(simTestInit, "simList")

  # Run spades
  simTest <- suppressWarnings(SpaDES.core::spades(simTestInit))
  expect_s4_class(simTest, "simList")

  # Check outputs
  expect_true(!is.null(simTest$emissionsProducts))

})


