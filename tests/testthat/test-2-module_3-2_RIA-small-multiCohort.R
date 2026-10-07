
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

      standDT      = {
        standDT <- file.path(paths$testdata, "RIA-small/input", "standDT.qs2") |> qs2::qs_read() |> data.table::as.data.table()
        standDT[, admin_name := "British Columbia"]
        standDT[, area := 250 * 250]
        standDT
      },
      cohortDT     = file.path(paths$testdata, "RIA-small/input", "cohortDT.qs2") |> qs2::qs_read() |> data.table::as.data.table(),
      gcMeta       = {
        gcMeta <- file.path(paths$testdata, "RIA-small/input", "gcMeta.qs2") |> qs2::qs_read() |> data.table::as.data.table()
        gcMeta <- cbind(gcMeta, unique(standDT[, .(admin_name, eco_id)]))
        gcMeta
      },
      gcIncrements = file.path(paths$testdata, "RIA-small/input", "gcIncrements.qs2") |> qs2::qs_read() |> data.table::as.data.table()
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
      file.path(spadesTestPaths$testdata, "RIA-small", "valid", paste0("emissionsProducts_dist", disturbances, ".csv")))
    expect_equal(simTest$emissionsProducts, emissionsProductsValid,
                 scale = 1, tolerance = 0.001, check.attributes = FALSE)
  }
})

