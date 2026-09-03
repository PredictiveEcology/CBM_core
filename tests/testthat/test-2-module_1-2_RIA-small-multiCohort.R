
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module: RIA-small 1998-2000", {

  cohortDTin <- file.path(spadesTestPaths$testdata, "RIA-small/input", "cohortDT.qs2") |>
    qs2::qs_read() |> data.table::as.data.table()

  for (fixedCohorts in c(TRUE, FALSE)){

    # Set up project
    projectName <- paste0("module_RIA-small_1998-2000_fixedCohorts", fixedCohorts)
    times       <- list(start = 1998, end = 2000)

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

      params = list(
        CBM_core = list(
          fixedCohorts = fixedCohorts
        )
      ),

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

    # Run simInit
    simTestInit <- SpaDES.core::simInit2(simInitInput)
    expect_s4_class(simTestInit, "simList")

    # Run spades
    simTest <- SpaDES.core::spades(simTestInit)
    expect_s4_class(simTest, "simList")

    # Check results
    cbm4_results <- CBM4r::cbm4_results_processor(simTest$CBM4data)

    expect_equal(simTest$emissionsProducts,
                 data.table::fread(file.path(spadesTestPaths$testdata, "RIA-small", "valid", "emissionsProducts.csv")),
                 scale = 1, tolerance = 0.001, check.attributes = FALSE)

    ## Check there are results for all input cohorts through all simulation stages
    simResults <- CBM4r::cbm4_results_query(cbm4_results, c(
      "SELECT a.raster_index, b.* FROM raster_index a LEFT JOIN simulation b",
      "ON a.timestep = b.timestep AND a.index = b.index AND a.cohort_index = b.cohort_index AND a.chunk_index = b.chunk_index"
    ))
    for (t in 0:2){

      expect_equal(sum(simResults$timestep == t), nrow(cohortDTin))

      expect_equal(
        simResults[timestep == t][, .(pixelIndex = raster_index + 1, age = state.age - t, gcID = classifiers.gc_id)][order(pixelIndex, gcID)],
        cohortDTin[, .(pixelIndex, age, gcID)],
        check.attributes = FALSE)
    }

    ## Check that cohorts have expected total C
    simResults[, C :=
                 pools.SoftwoodMerch + pools.SoftwoodFoliage + pools.SoftwoodOther +
                 pools.HardwoodMerch + pools.HardwoodFoliage + pools.HardwoodOther]
    for (t in 1:2){
      C_i <- simResults[timestep == t - 1]
      C_f <- simResults[timestep == t]
      C_diff <- merge(C_i, C_f[, .SD, .SDcols = c("raster_index", "classifiers.gc_id", "cohort_index", "chunk_index", "C")],
                      by = c("raster_index", "classifiers.gc_id", "cohort_index", "chunk_index"))
      C_diff[, C_diff := C.y - C.x]
      C_diff <- merge(C_diff, simTest$gcIncrements, by.x = c("classifiers.gc_id", "state.age"), by.y = c("gcID", "age"))
      expect_equal(
        C_diff$C_diff,
        C_diff$merch_inc + C_diff$foliage_inc + C_diff$other_inc
      )
    }
  }
})

