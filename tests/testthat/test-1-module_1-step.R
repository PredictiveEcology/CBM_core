
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module: step without spinup", {

  incAges <- list(
    age10 = 10, # Increments apply to matching age
    age1  =  1, # Increments apply when cohort age exceeds increment maximum age
    ageQ  = "?" # Increments apply to all ages
  )

  # Set up project
  for (testName in names(incAges)){

    projectName <- paste0("module_step_", testName)
    times       <- list(start = 2000, end = 2000)

    simInitInput <- SpaDES.project::setupProject(
      modules = "CBM_core",
      times   = times,
      paths   = list(
        projectPath = spadesTestPaths$projectPath,
        modulePath  = spadesTestPaths$modulePath,
        packagePath = spadesTestPaths$packagePath,
        inputPath   = spadesTestPaths$inputPath,
        cachePath   = spadesTestPaths$cachePath,
        outputPath  = file.path(spadesTestPaths$temp$outputs, projectName)
      ),
      params = list(
        CBM_core = list(
          .useCacheCBM4 = FALSE,
          .plot         = FALSE,
          spinup        = FALSE,
          fixedCohorts  = FALSE
        )
      ),
      masterRaster = terra::rast(
        crs  = "EPSG:3979",
        ext  = c(xmin = -687696, xmax = -687696 + 1, ymin = 711955, ymax = 711955 + 1),
        res  = 1,
        vals = 1L
      ),
      standDT           = data.table::data.table(pixelIndex = 1, admin_abbrev = "SK", eco_id = 9),
      cohortDT          = data.table::data.table(
        pixelIndex = 1, gcID = 1, age = 10,
        SoftwoodMerch           = 0,
        SoftwoodFoliage         = 0,
        SoftwoodOther           = 0,
        SoftwoodCoarseRoots     = 0,
        SoftwoodFineRoots       = 0,
        SoftwoodStemSnag        = 0,
        SoftwoodBranchSnag      = 0,
        HardwoodMerch           = 0,
        HardwoodFoliage         = 0,
        HardwoodOther           = 0,
        HardwoodCoarseRoots     = 0,
        HardwoodFineRoots       = 0,
        HardwoodStemSnag        = 0,
        HardwoodBranchSnag      = 0,
        AboveGroundVeryFastSoil = 0,
        BelowGroundVeryFastSoil = 0,
        AboveGroundFastSoil     = 0,
        BelowGroundFastSoil     = 0,
        MediumSoil              = 0,
        AboveGroundSlowSoil     = 0,
        BelowGroundSlowSoil     = 0
      ),
      gcMeta            = data.table::data.table(gcID = 1, sw = TRUE),
      gcIncrements      = data.table::data.table(gcID = 1, age = NA, merch_inc = 1, foliage_inc = 1, other_inc = 1)
    )
    simInitInput$gcIncrements$age <- incAges[[testName]]

    # Run simInit
    simTestInit <- SpaDES.core::simInit2(simInitInput)
    expect_s4_class(simTestInit, "simList")

    # Run spades
    simTest <- SpaDES.core::spades(simTestInit)
    expect_s4_class(simTest, "simList")

    # Check outputs
    pools <- simTest$cohortDT[, .SD, .SDcols = names(simTest$cohortDT)[grepl("pools\\.", names(simTest$cohortDT))]]

    poolsValid <- data.table::data.table(
      pools.SoftwoodMerch       = 1,
      pools.SoftwoodFoliage     = 1,
      pools.SoftwoodOther       = 1,
      pools.SoftwoodCoarseRoots = 0.4004544,
      pools.SoftwoodFineRoots   = 0.2655456
    )
    for (pool in setdiff(names(pools), names(poolsValid))) poolsValid[[pool]] <- 0

    expect_equal(pools[, .SD, .SDcols = names(poolsValid)], poolsValid, tolerance = 0.000001, scale = 1)

    expect_equal(simTest$emissionsProducts, data.table::data.table(
      year = 2000, timestep = 1, Products = 0, Emissions = 0, CO2 = 0, CH4 = 0, CO = 0, key = "year"))
  }
})

test_that("Module: step without spinup: with disturbance", {

  # Set up project
  projectName <- "module_step-disturbance"
  times       <- list(start = 2000, end = 2000)

  simInitInput <- SpaDES.project::setupProject(
    modules = "CBM_core",
    times   = times,
    paths   = list(
      projectPath = spadesTestPaths$projectPath,
      modulePath  = spadesTestPaths$modulePath,
      packagePath = spadesTestPaths$packagePath,
      inputPath   = spadesTestPaths$inputPath,
      cachePath   = spadesTestPaths$cachePath,
      outputPath  = file.path(spadesTestPaths$temp$outputs, projectName)
    ),
    params = list(
      CBM_core = list(
        .useCacheCBM4 = FALSE,
        .plot         = FALSE,
        spinup        = FALSE,
        fixedCohorts  = FALSE
      )
    ),
    masterRaster = terra::rast(
      crs  = "EPSG:3979",
      ext  = c(xmin = -687696, xmax = -687696 + 1, ymin = 711955, ymax = 711955 + 1),
      res  = 1,
      vals = 1L
    ),
    standDT           = data.table::data.table(pixelIndex = 1, admin_abbrev = "SK", eco_id = 9),
    cohortDT          = data.table::data.table(
      pixelIndex = 1, gcID = 1, age = 10,
      SoftwoodMerch           = 1,
      SoftwoodFoliage         = 1,
      SoftwoodOther           = 1,
      SoftwoodCoarseRoots     = 1,
      SoftwoodFineRoots       = 1,
      SoftwoodStemSnag        = 1,
      SoftwoodBranchSnag      = 1,
      HardwoodMerch           = 1,
      HardwoodFoliage         = 1,
      HardwoodOther           = 1,
      HardwoodCoarseRoots     = 1,
      HardwoodFineRoots       = 1,
      HardwoodStemSnag        = 1,
      HardwoodBranchSnag      = 1,
      AboveGroundVeryFastSoil = 1,
      BelowGroundVeryFastSoil = 1,
      AboveGroundFastSoil     = 1,
      BelowGroundFastSoil     = 1,
      MediumSoil              = 1,
      AboveGroundSlowSoil     = 1,
      BelowGroundSlowSoil     = 1
    ),

    gcMeta            = data.table::data.table(gcID = 1, sw = TRUE),
    gcIncrements      = data.table::data.table(gcID = 1, age = "?", merch_inc = 0, foliage_inc = 0, other_inc = 0),

    disturbanceMeta   = data.table::data.table(eventID = 1, disturbance_type_name = "Wildfire"),
    disturbanceEvents = data.table::data.table(pixelIndex = 1, year = 2000, eventID = 1)
  )

  # Run simInit
  simTestInit <- SpaDES.core::simInit2(simInitInput)
  expect_s4_class(simTestInit, "simList")

  # Run spades
  simTest <- SpaDES.core::spades(simTestInit)
  expect_s4_class(simTest, "simList")

  # Check outputs
  pools <- simTest$cohortDT[, .SD, .SDcols = names(simTest$cohortDT)[grepl("pools\\.", names(simTest$cohortDT))]]

  poolsValid <- data.table::data.table(
    pools.SoftwoodMerch       = 0,
    pools.SoftwoodFoliage     = 0,
    pools.SoftwoodOther       = 0,
    pools.SoftwoodCoarseRoots = 0,
    pools.SoftwoodFineRoots   = 0,
    pools.HardwoodMerch       = 0,
    pools.HardwoodFoliage     = 0,
    pools.HardwoodOther       = 0,
    pools.HardwoodCoarseRoots = 0,
    pools.HardwoodFineRoots   = 0,
    pools.CO2                 = 5.747076,
    pools.CH4                 = 0.053862,
    pools.CO                  = 0.484750,
    pools.NO2                 = 0,
    pools.Products            = 0
  )

  expect_equal(pools[, .SD, .SDcols = names(poolsValid)], poolsValid, tolerance = 0.000001, scale = 1)

  expect_equal(simTest$emissionsProducts, data.table::data.table(
    year = 2000, timestep = 1,
    Products  = 1/10^4 * poolsValid$pools.Products,
    Emissions = 1/10^4 * (poolsValid$pools.CO2 + poolsValid$pools.CH4 + poolsValid$pools.CO),
    CO2       = 1/10^4 * poolsValid$pools.CO2,
    CH4       = 1/10^4 * poolsValid$pools.CH4,
    CO        = 1/10^4 * poolsValid$pools.CO,
    key       = "year"),
    tolerance = 0.000001, scale = 1)
})



