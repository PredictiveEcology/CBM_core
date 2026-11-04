
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Multi module: RIA-small with LandR 2000-2002", {

  ## Run simInit and spades ----

  skip_if(isTRUE(Sys.getenv("CI")) & Sys.getenv("BRANCH_NAME") != "main")
  skip_on_ci()

  # Set up project
  simInitInput <- setupProjectTest(
    projectName = "integration_LandRCBM_RIA-small_2000-2002",
    times       = list(start = 2000, end = 2002),
    modules = c(
      paste0("PredictiveEcology/Biomass_core@",         Sys.getenv("BRANCH_NAME", "development")),
      paste0("PredictiveEcology/LandRCBM_split3pools@", Sys.getenv("BRANCH_NAME", "development")),
      "CBM_core"
    ),

    params = list(
      .globals = list(
        .plots = NA,
        sppEquivCol = "LandR"
      ),
      CBM_core = list(
        skipPrepareCBMvars = TRUE
      )
    ),

    studyArea             = file.path(test_path("testdata"), "LandRCBM-RIA-small/input", "studyArea.shp")     |> sf::st_read(quiet = TRUE),
    rasterToMatch         = file.path(test_path("testdata"), "LandRCBM-RIA-small/input", "rasterToMatch.tif") |> terra::rast(),
    standDT               = file.path(test_path("testdata"), "LandRCBM-RIA-small/input", "standDT.qs2")       |> qs2::qs_read() |> data.table::as.data.table(),
    biomassMap            = file.path(test_path("testdata"), "LandRCBM-RIA-small/input", "biomassMap.tif")    |> terra::rast(),
    cohortData            = file.path(test_path("testdata"), "LandRCBM-RIA-small/input", "cohortData.qs2")    |> qs2::qs_read() |> data.table::as.data.table(),
    pixelGroupMap         = file.path(test_path("testdata"), "LandRCBM-RIA-small/input", "pixelGroupMap.tif") |> terra::rast(),
    speciesLayers         = file.path(test_path("testdata"), "LandRCBM-RIA-small/input", "speciesLayers.tif") |> terra::rast(),
    ecoregionMap          = file.path(test_path("testdata"), "LandRCBM-RIA-small/input", "ecoregionMap.tif")  |> terra::rast(),
    minRelativeB          = file.path(test_path("testdata"), "LandRCBM-RIA-small/input", "minRelativeB.qs2")  |> qs2::qs_read() |> data.table::as.data.table(),
    ecoregion             = file.path(test_path("testdata"), "LandRCBM-RIA-small/input", "ecoregion.qs2")     |> qs2::qs_read() |> data.table::as.data.table(),
    species               = file.path(test_path("testdata"), "LandRCBM-RIA-small/input", "species.qs2")       |> qs2::qs_read() |> data.table::as.data.table(),
    speciesEcoregion      = file.path(test_path("testdata"), "LandRCBM-RIA-small/input", "speciesEcoregion.qs2") |> qs2::qs_read() |> data.table::as.data.table(),
    yieldTablesCumulative = file.path(test_path("testdata"), "LandRCBM-RIA-small/input", "yieldTablesCumulative.qs2") |> qs2::qs_read() |> data.table::as.data.table(),
    yieldTablesId         = file.path(test_path("testdata"), "LandRCBM-RIA-small/input", "yieldTablesId.qs2") |> qs2::qs_read() |> data.table::as.data.table(),
    sppEquiv              = file.path(test_path("testdata"), "LandRCBM-RIA-small/input", "sppEquiv.qs2") |> qs2::qs_read() |> data.table::as.data.table()
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


