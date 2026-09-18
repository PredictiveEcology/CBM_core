
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module: step without spinup", {

  incAges <- list(
    age10 = 10, # Increments apply to matching age
    age1  =  1, # Increments apply when cohort age exceeds increment maximum age
    ageQ  = "?" # Increments apply to all ages
  )

  for (testName in names(incAges)){

    # Set up project
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

  distTests <- list(

    # Wildfire in a single pixel
    fire = list(
      disturbanceMeta   = data.table::data.table(eventID = 1, disturbance_type_name = "Wildfire"),
      disturbanceEvents = data.table::data.table(eventID = 1, year = 2000, pixelIndex = 1)
    ),

    # 2 disturbances in the same pixel in the same year, with wildfire taking priority
    priority = list(
      disturbanceMeta   = rbind(
        data.table::data.table(eventID = 1, disturbance_type_name = "Wildfire", priority = 1),
        data.table::data.table(eventID = 2, disturbance_type_name = "Clearcut harvesting without salvage", priority = 2)
      ),
      disturbanceEvents = rbind(
        data.table::data.table(eventID = 1, year = 2000, pixelIndex = 1),
        data.table::data.table(eventID = 2, year = 2000, pixelIndex = 1)
      )
    ),

    # 2 disturbances in the same pixel with disturbance_order
    priority_order = list(
      disturbanceMeta   = rbind(
        data.table::data.table(eventID = 1, disturbance_type_name = "Wildfire", disturbance_order = 2),
        data.table::data.table(eventID = 2, disturbance_type_name = "Clearcut harvesting without salvage", disturbance_order = 1)
      ),
      disturbanceEvents = rbind(
        data.table::data.table(eventID = 1, year = 2000, pixelIndex = 1),
        data.table::data.table(eventID = 2, year = 2000, pixelIndex = 1)
      )
    ),

    # 2 disturbances in the same pixel in the same year without order or priority
    priority_missing = list(
      disturbanceMeta   = rbind(
        data.table::data.table(eventID = 1, disturbance_type_name = "Wildfire"),
        data.table::data.table(eventID = 2, disturbance_type_name = "Clearcut harvesting without salvage")
      ),
      disturbanceEvents = rbind(
        data.table::data.table(eventID = 1, year = 2000, pixelIndex = 1),
        data.table::data.table(eventID = 2, year = 2000, pixelIndex = 1)
      )
    )
  )

  testOut <- list()
  for (testName in names(distTests)){

    # Set up project
    projectName <- paste0("module_step-disturbance_", testName)
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
      gcIncrements      = data.table::data.table(gcID = 1, age = "?", merch_inc = 0, foliage_inc = 0, other_inc = 0)
    )

    simInitInput$disturbanceMeta   <- distTests[[testName]]$disturbanceMeta
    simInitInput$disturbanceEvents <- distTests[[testName]]$disturbanceEvents

    # Run simInit
    simTestInit <- SpaDES.core::simInit2(simInitInput)
    expect_s4_class(simTestInit, "simList")

    if (testName == "priority_missing"){

      # Expect error: multiple disturbances without order or priority
      expect_error(SpaDES.core::spades(simTestInit), "priority")

    }else{

      # Run spades
      simTest <- SpaDES.core::spades(simTestInit)
      expect_s4_class(simTest, "simList")

      testOut[[testName]] <- list(
        cohortDT          = data.table::copy(simTest$cohortDT),
        emissionsProducts = data.table::copy(simTest$emissionsProducts)
      )
    }
  }

  # Check outputs
  for (testName in c("fire", "priority")){

    cohortDT          <- testOut[[testName]]$cohortDT
    emissionsProducts <- testOut[[testName]]$emissionsProducts

    expect_equal(cohortDT$age, 1)

    pools <- cohortDT[, .SD, .SDcols = names(cohortDT)[grepl("pools\\.", names(cohortDT))]]

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

    expect_equal(emissionsProducts, data.table::data.table(
      year = 2000, timestep = 1,
      Products  = 1/10^4 * poolsValid$pools.Products,
      Emissions = 1/10^4 * (poolsValid$pools.CO2 + poolsValid$pools.CH4 + poolsValid$pools.CO),
      CO2       = 1/10^4 * poolsValid$pools.CO2,
      CH4       = 1/10^4 * poolsValid$pools.CH4,
      CO        = 1/10^4 * poolsValid$pools.CO,
      key       = "year"),
      tolerance = 0.000001, scale = 1)
  }

  for (testName in "priority_order"){

    cohortDT          <- testOut[[testName]]$cohortDT
    emissionsProducts <- testOut[[testName]]$emissionsProducts

    # Check that 2 disturbances occured. Pools should be different than if just wildfire occurs
    expect_equal(cohortDT$age, 1)

    expect_false(isTRUE(all.equal(
      cohortDT[, .SD, .SDcols = names(cohortDT)[grepl("pools\\.", names(cohortDT))]],
      testOut[["fire"]]$cohortDT[, .SD, .SDcols = names(cohortDT)[grepl("pools\\.", names(cohortDT))]],
      check.attributes = FALSE
    )))

    expect_false(isTRUE(all.equal(
      emissionsProducts,
      testOut[["fire"]]$emissionsProducts,
      check.attributes = FALSE
    )))
  }
})

test_that("Module: step without spinup: with partial disturbance", {

  # Set up project
  projectName <- "module_step-disturbance-partial"
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
      pixelIndex              = 1,
      species                 = c("A", "B"), # 2 cohorts
      age                     = 10,
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

    gcMeta            = data.table::data.table(gcID = 1:2, species = c("A", "B"), sw = TRUE),
    gcIncrements      = data.table::data.table(gcID = 1:2, age = "?", merch_inc = 0, foliage_inc = 0, other_inc = 0),

    disturbanceMeta   = data.table::data.table(eventID = 1, species = "A", disturbance_type_name = "Wildfire"),
    disturbanceEvents = data.table::data.table(eventID = 1, year = 2000, pixelIndex = 1)
  )

  # Run simInit
  simTestInit <- SpaDES.core::simInit2(simInitInput)
  expect_s4_class(simTestInit, "simList")

  # Run spades
  simTest <- SpaDES.core::spades(simTestInit)
  expect_s4_class(simTest, "simList")

  # Check outputs
  expect_equal(simTest$cohortDT[, .(species, age)], data.table::data.table(species = c("A", "B"), age = c(1, 11)))

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

  expect_equal(pools[1, .SD, .SDcols = names(poolsValid)], poolsValid, tolerance = 0.000001, scale = 1)

})

