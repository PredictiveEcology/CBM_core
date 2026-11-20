
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Multi module: RIA-small with LandR 2000-2002", {

  ## Run simInit and spades ----

  skip_if(as.logical(Sys.getenv("CI", FALSE)) & !"main" %in% Sys.getenv(c("GITHUB_BASE_REF", "GITHUB_REF_NAME")))
  skip_on_ci()

  # Set up project
  simInitInput <- setupProjectTest(
    projectName = "integration_LandRCBM_RIA-small_2000-2002",
    times       = list(start = 2000, end = 2002),
    modules = c(
      paste0("PredictiveEcology/Biomass_core@",         ifelse("main" %in% Sys.getenv(c("GITHUB_BASE_REF", "GITHUB_REF_NAME")), "main", "development")),
      paste0("PredictiveEcology/LandRCBM_split3pools@", ifelse("main" %in% Sys.getenv(c("GITHUB_BASE_REF", "GITHUB_REF_NAME")), "main", "development")),
      "CBM_core"
    ),

    masterRaster = terra::rast(
      crs  = "EPSG:3005",
      ext  = c(xmin = 1018000, xmax = 1020000, ymin = 1200750, ymax = 1202000),
      res  = 250,
      vals = 1L
    ),
    standDT = data.table::data.table(
      admin_name   = "British Columbia",
      admin_abbrev = "BC",
      eco_id       = 14
    ),
    biomassMap            = file.path(test_path("testdata"), "LandRCBM-RIA-small/input", "biomassMap.tif")    |> terra::rast(),
    cohortData            = file.path(test_path("testdata"), "LandRCBM-RIA-small/input", "cohortData.rds")    |> readRDS(),
    pixelGroupMap         = file.path(test_path("testdata"), "LandRCBM-RIA-small/input", "pixelGroupMap.tif") |> terra::rast(),
    speciesLayers         = file.path(test_path("testdata"), "LandRCBM-RIA-small/input", "speciesLayers.tif") |> terra::rast(),
    ecoregionMap          = file.path(test_path("testdata"), "LandRCBM-RIA-small/input", "ecoregionMap.tif")  |> terra::rast(),
    minRelativeB          = file.path(test_path("testdata"), "LandRCBM-RIA-small/input", "minRelativeB.rds")  |> readRDS(),
    ecoregion             = file.path(test_path("testdata"), "LandRCBM-RIA-small/input", "ecoregion.rds")     |> readRDS(),
    species               = file.path(test_path("testdata"), "LandRCBM-RIA-small/input", "species.rds")       |> readRDS(),
    speciesEcoregion      = file.path(test_path("testdata"), "LandRCBM-RIA-small/input", "speciesEcoregion.rds") |> readRDS(),
    yieldTablesCumulative = file.path(test_path("testdata"), "LandRCBM-RIA-small/input", "yieldTablesCumulative.rds") |> readRDS(),
    yieldTablesId         = file.path(test_path("testdata"), "LandRCBM-RIA-small/input", "yieldTablesId.rds") |> readRDS(),
    sppEquiv              = file.path(test_path("testdata"), "LandRCBM-RIA-small/input", "sppEquiv.rds") |> readRDS(),

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

  simInitInput$standDT <- cbind(
    data.table::data.table(pixelIndex = 1:terra::ncell(simInitInput$masterRaster)),
    simInitInput$standDT,
    area = prod(terra::res(simInitInput$masterRaster))
  )
  simInitInput$rasterToMatch <- simInitInput$masterRaster
  simInitInput$studyArea     <- sf::st_sf(sf::st_as_sfc(sf::st_bbox(simInitInput$masterRaster)))

  # Run simInit
  simTestInit <- suppressWarnings(SpaDES.core::simInit2(simInitInput))
  expect_s4_class(simTestInit, "simList")

  # Run spades
  simTest <- suppressWarnings(SpaDES.core::spades(simTestInit))
  expect_s4_class(simTest, "simList")

  # Check outputs
  expect_true(!is.null(simTest$emissionsProducts))

})


