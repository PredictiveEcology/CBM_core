
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module: RIA-small", {

  for (cohort_fixed in c(TRUE, FALSE)) for (disturbances in c(FALSE, TRUE)){

    ## Run simInit and spades ----

    # Set up project
    projectName <- paste0("module_RIA-small_dist", disturbances, "_fixedCohorts", cohort_fixed)
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

      params = list(CBM_core = list(.plots = NA, cohort_fixed = cohort_fixed)),

      masterRaster = terra::rast(
        crs  = "EPSG:3005",
        ext  = c(xmin = 1018000, xmax = 1020000, ymin = 1200000, ymax = 1202000),
        res  = 250,
        vals = 1L
      ),
      standDT      = file.path(paths$testdata, "RIA-small/input", "standDT.qs2")      |> qs2::qs_read() |> data.table::as.data.table(),
      cohortDT     = file.path(paths$testdata, "RIA-small/input", "cohortDT.qs2")     |> qs2::qs_read() |> data.table::as.data.table(),
      gcMeta       = file.path(paths$testdata, "RIA-small/input", "gcMeta.qs2")       |> qs2::qs_read() |> data.table::as.data.table(),
      gcIncrements = file.path(paths$testdata, "RIA-small/input", "gcIncrements.qs2") |> qs2::qs_read() |> data.table::as.data.table()
    )

    if (disturbances){
      simInitInput$disturbanceMeta   <- data.table::data.table(eventID = 1, disturbance_type_name = "Wildfire")
      simInitInput$disturbanceEvents <- data.table::data.table(pixelIndex = 1, year = 2001, eventID = 1)
    }

    # Run simInit
    simTestInit <- SpaDES.core::simInit2(simInitInput)
    expect_s4_class(simTestInit, "simList")

    # Run spades
    simTest <- SpaDES.core::spades(simTestInit)
    expect_s4_class(simTest, "simList")


    ## Check outputs ----

    emissionsProductsValid <- data.table::fread(
      file.path(spadesTestPaths$testdata, "RIA-small", "valid", paste0("emissionsProducts_dist", disturbances, ".csv")))
    expect_equal(simTest$emissionsProducts, emissionsProductsValid,
                 scale = 1, tolerance = 0.001, check.attributes = FALSE)

    cbm4_results <- CBM4r::SQLResultsProcessor(simTest$CBM4data)
    simResults <- CBM4r::cbm4_results_cohort(cbm4_results, "spatial_pool_indicators")

    ## Check there are results for all input cohorts through all simulation stages
    cohortDTin <- file.path(spadesTestPaths$testdata, "RIA-small/input", "cohortDT.qs2") |>
      qs2::qs_read() |> data.table::as.data.table()
    for (t in 0:2){

      expect_equal(sum(simResults$timestep == t), nrow(cohortDTin))

      cohortIn  <- cohortDTin[, .(pixelIndex, age = age + t, gcID)]
      cohortOut <- simResults[timestep == t][, .(pixelIndex = pixel_index, age, gcID = gc_id)]

      if (disturbances & t == 2){

        # Check that disturbance reset ages
        expect_equal(cohortOut[pixelIndex == 1, age], rep(1, 2))

        expect_equal(cohortOut[pixelIndex != 1], cohortIn[pixelIndex != 1], check.attributes = FALSE)

      }else{
        expect_equal(cohortOut, cohortIn, check.attributes = FALSE)
      }
    }

    ## Check that cohorts have expected total C
    simResults[, C :=
                 SoftwoodMerch + SoftwoodFoliage + SoftwoodOther +
                 HardwoodMerch + HardwoodFoliage + HardwoodOther]
    for (t in 1:2){
      C_i <- simResults[timestep == t - 1]
      C_f <- simResults[timestep == t][, age := age - 1]
      C_diff <- merge(C_i[, .SD, .SDcols = c("pixel_index", "gc_id", "age", "C")],
                      C_f[, .SD, .SDcols = c("pixel_index", "gc_id", "age", "C")],
                      by = c("pixel_index", "gc_id", "age"))
      C_diff[, C_diff := C.y - C.x]
      C_diff <- merge(C_diff, simTest$gcIncrements, by.x = c("gc_id", "age"), by.y = c("gcID", "age"))
      if (disturbances & t == 2) C_diff <- C_diff[pixel_index != 1]
      expect_equal(
        C_diff$C_diff,
        C_diff$merch_inc + C_diff$foliage_inc + C_diff$other_inc
      )
    }
  }
})

