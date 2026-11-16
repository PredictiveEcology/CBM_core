
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Multi module: RIA-small with LandR 2000-2002", {

  ## Run simInit and spades ----

  testthat::skip_on_ci()

  # Set up project
  projectName <- "integration_LandRCBM_RIA-small_2000-2002"
  times       <- list(start = 2000, end = 2002)

  simInitInput <- SpaDES.project::setupProject(

    modules = c(
      paste0("PredictiveEcology/Biomass_core@", Sys.getenv("BRANCH_NAME", "development")),
      "PredictiveEcology/LandRCBM_split3pools@main",
      "CBM_core"
    ),

    times   = times,
    paths   = list(
      projectPath = spadesTestPaths$projectPath,
      modulePath  = spadesTestPaths$temp$modules,
      packagePath = spadesTestPaths$packagePath,
      inputPath   = spadesTestPaths$inputPath,
      cachePath   = spadesTestPaths$cachePath,
      outputPath  = file.path(spadesTestPaths$temp$outputs, projectName),
      testdata    = spadesTestPaths$testdata
    ),

    require = c("sf", "terra", "data.table"),

    # Prepare input objects
    masterRaster = terra::rast(
      crs  = "EPSG:3005",
      ext  = c(xmin = 1018000, xmax = 1020000, ymin = 1200750, ymax = 1202000),
      res  = 250,
      vals = 1L
    ),
    standDT = data.table::data.table(
      pixelIndex   = 1:terra::ncell(masterRaster),
      admin_name   = "British Columbia",
      admin_abbrev = "BC",
      eco_id       = 14,
      area         = prod(terra::res(masterRaster))
    ),
    rasterToMatch         = masterRaster,
    studyArea             = sf::st_sf(sf::st_as_sfc(sf::st_bbox(rasterToMatch))),
    biomassMap            = file.path(paths$testdata, "LandRCBM-RIA-small/input", "biomassMap.tif")    |> terra::rast(),
    cohortData            = file.path(paths$testdata, "LandRCBM-RIA-small/input", "cohortData.rds")    |> readRDS(),
    pixelGroupMap         = file.path(paths$testdata, "LandRCBM-RIA-small/input", "pixelGroupMap.tif") |> terra::rast(),
    speciesLayers         = file.path(paths$testdata, "LandRCBM-RIA-small/input", "speciesLayers.tif") |> terra::rast(),
    ecoregionMap          = file.path(paths$testdata, "LandRCBM-RIA-small/input", "ecoregionMap.tif")  |> terra::rast(),
    minRelativeB          = file.path(paths$testdata, "LandRCBM-RIA-small/input", "minRelativeB.rds")  |> readRDS(),
    ecoregion             = file.path(paths$testdata, "LandRCBM-RIA-small/input", "ecoregion.rds")     |> readRDS(),
    species               = file.path(paths$testdata, "LandRCBM-RIA-small/input", "species.rds")       |> readRDS(),
    speciesEcoregion      = file.path(paths$testdata, "LandRCBM-RIA-small/input", "speciesEcoregion.rds") |> readRDS(),
    yieldTablesCumulative = file.path(paths$testdata, "LandRCBM-RIA-small/input", "yieldTablesCumulative.rds") |> readRDS(),
    yieldTablesId         = file.path(paths$testdata, "LandRCBM-RIA-small/input", "yieldTablesId.rds") |> readRDS(),
    sppEquiv              = file.path(paths$testdata, "LandRCBM-RIA-small/input", "sppEquiv.rds") |> readRDS(),

    # Parameters
    params = list(
      .globals = list(
        .plots = NA,
        sppEquivCol = "LandR"
      ),
      CBM_core = list(
        skipPrepareCBMvars = TRUE
      ))
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


