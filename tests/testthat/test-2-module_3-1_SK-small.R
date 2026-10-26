
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module: SK-small", {

  for (cohort_fixed in c(TRUE, FALSE)) for (disturbances in c(FALSE, TRUE)){

    ## Run simInit and spades ----

    # Set up project
    projectName <- paste0("module_SK-small_dist", disturbances, "_fixedCohorts", cohort_fixed)
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

      params = list(CBM_core = list(cohort_fixed = cohort_fixed)),

      masterRaster = terra::rast(
        crs  = "EPSG:3979",
        ext  = c(xmin = -687696, xmax = -681036, ymin = 711955, ymax = 716183),
        res  = 30,
        vals = 1L
      ),
      standDT      = file.path(paths$testdata, "SK-small/input", "standDT.qs2")      |> qs2::qs_read() |> data.table::as.data.table(),
      cohortDT     = file.path(paths$testdata, "SK-small/input", "cohortDT.qs2")     |> qs2::qs_read() |> data.table::as.data.table(),
      gcMeta       = file.path(paths$testdata, "SK-small/input", "gcMeta.qs2")       |> qs2::qs_read() |> data.table::as.data.table(),
      gcIncrements = file.path(paths$testdata, "SK-small/input", "gcIncrements.qs2") |> qs2::qs_read() |> data.table::as.data.table()
    )

    ## In year 2000 there are no disturbances
    ## In year 1998 pixels 1324-1327 have both a wildfire and "Generic 20% mortality" event;
    ## wildfire takes priority. `disturbanceEvents[pixelIndex %in% 1324:1327 & year == 1998]`
    if (disturbances){
      simInitInput$disturbanceMeta   <- file.path(spadesTestPaths$testdata, "SK-small/input", "disturbanceMeta.qs2")   |> qs2::qs_read() |> data.table::as.data.table()
      simInitInput$disturbanceEvents <- file.path(spadesTestPaths$testdata, "SK-small/input", "disturbanceEvents.qs2") |> qs2::qs_read() |> data.table::as.data.table()
    }

    # Run simInit
    simTestInit <- SpaDES.core::simInit2(simInitInput)
    expect_s4_class(simTestInit, "simList")

    # Run spades
    simTest <- SpaDES.core::spades(simTestInit)
    expect_s4_class(simTest, "simList")


    ## Check inputs ----

    ## Check that input tables are not altered by module.
    refTables <- list(
      standDT           = qs2::qs_read(file.path(spadesTestPaths$testdata, "SK-small/input", "standDT.qs2")),
      cohortDT          = qs2::qs_read(file.path(spadesTestPaths$testdata, "SK-small/input", "cohortDT.qs2")),
      gcMeta            = qs2::qs_read(file.path(spadesTestPaths$testdata, "SK-small/input", "gcMeta.qs2")),
      gcIncrements      = qs2::qs_read(file.path(spadesTestPaths$testdata, "SK-small/input", "gcIncrements.qs2")),
      disturbanceMeta   = qs2::qs_read(file.path(spadesTestPaths$testdata, "SK-small/input", "disturbanceMeta.qs2")),
      disturbanceEvents = qs2::qs_read(file.path(spadesTestPaths$testdata, "SK-small/input", "disturbanceEvents.qs2"))
    )
    outTables <- lapply(setNames(names(refTables), names(refTables)), function(table) simTest[[table]]) |>
      lapply(data.table::copy) |> lapply(data.table::setindex, NULL)

    expect_equal(outTables$standDT[, .SD, .SDcols = names(refTables$standDT)], refTables$standDT) # Columns are added
    if (cohort_fixed) expect_equal(outTables$cohortDT, refTables$cohortDT)
    expect_equal(outTables$gcMeta,       refTables$gcMeta)
    expect_equal(outTables$gcIncrements, refTables$gcIncrements)
    if (disturbances){
      expect_equal(outTables$disturbanceMeta,   refTables$disturbanceMeta)
      expect_equal(outTables$disturbanceEvents, refTables$disturbanceEvents)
    }


    ## Check outputs ----

    # emissionsProducts, pools, and flux
    testResults <- list(emissionsProducts = simTest$emissionsProducts)
    cbm4_results <- CBM4r::SQLResultsProcessor(simTest$CBM4data)
    for (view_name in c("pool_indicators", "flux_indicators", "disturbance_indicators"[disturbances])){
      testResults[[view_name]] <- CBM4r::cbm4_results_timestep(cbm4_results, view_name)
    }
    for (table in names(testResults)){
      testValid <- data.table::fread(file.path(spadesTestPaths$testdata, "SK-small/valid", paste0("dist", disturbances), paste0(table, ".csv")))
      expect_equal(names(testResults[[table]]), names(testValid))
      expect_equal(testResults[[table]], testValid, scale = 1, tolerance = 0.001, check.attributes = FALSE)
    }

    # Check cohort group count
    expect_equal(length(CBM4r::cbm4_results_view(cbm4_results, "simulation", timestep = 3)$index), ifelse(disturbances, 43, 41))

    # Check disturbance event priority
    if (disturbances){
      distPriority <- CBM4r::cbm4_results_cohort(cbm4_results, "spatial_pool_indicators", timestep = 1)[pixel_index %in% 1324:1327]
      expect_all_equal(distPriority$age,                         1)
      expect_all_equal(distPriority$time_since_last_disturbance, 1)
      expect_all_equal(distPriority$last_disturbance_type,       1)
    }

    # Check that plots are created
    expect_equal(list.files(file.path(outputPath(simTest), "CBM_core_figures")), c(
      "emissionsProducts.png",
      "NPP-1998.png", "NPP-2000.png",
      "poolProportions.png",
      "totalCarbon-1998.png", "totalCarbon-2000.png"
    ))
  }
})


