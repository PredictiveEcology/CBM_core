
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module: RIA-small", {

  for (disturbances in c(FALSE, TRUE)){

    # Set up project
    projectName <- paste0("module_RIA-small_dist", disturbances)
    times       <- list(start = 2000, end = 2001)

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

      params = list(CBM_core = list(.plots = NA)),

      masterRaster = terra::rast(
        crs  = "EPSG:3005",
        ext  = c(xmin = 1018000, xmax = 1020000, ymin = 1200750, ymax = 1202000),
        res  = 250,
        vals = 1L
      ),
      standDT = data.table::data.table(
        pixelIndex = 1:terra::ncell(masterRaster),
        admin_name = "British Columbia",
        eco_id     = 14,
        area       = prod(terra::res(masterRaster))
      ),
      cohortDT     = file.path(paths$testdata, "RIA-small/input", "cohortDT.rds")     |> readRDS(),
      gcMeta       = file.path(paths$testdata, "RIA-small/input", "gcMeta.rds")       |> readRDS(),
      gcIncrements = file.path(paths$testdata, "RIA-small/input", "gcIncrements.rds") |> readRDS()
    )

    if (disturbances){
      simInitInput$disturbanceMeta   <- data.table::data.table(eventID = 1, disturbance_type_id = 1)
      simInitInput$disturbanceEvents <- data.table::data.table(pixelIndex = 1, year = 2001, eventID = 1)
    }

    # Run simInit
    simTestInit <- SpaDES.core::simInit2(simInitInput)
    expect_s4_class(simTestInit, "simList")

    # Run spades
    simTest <- SpaDES.core::spades(simTestInit)
    expect_s4_class(simTest, "simList")

    # Check results
    emissionsProductsValid <- data.table::fread(
      file.path(spadesTestPaths$testdata, "RIA-small/valid", paste0("dist", disturbances), "emissionsProducts.csv"))
    expect_equal(simTest$emissionsProducts, emissionsProductsValid,
                 scale = 1, tolerance = 0.001, check.attributes = FALSE)
  }
})

