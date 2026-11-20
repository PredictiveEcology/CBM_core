
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module: SK-small", {

  for (disturbances in c(FALSE, TRUE)){

    ## Run simInit and spades ----

    # Set up project
    simInitInput <- setupProjectTest(
      projectName = "module_SK-small",
      times       = list(start = 1998, end = 2000),

      params = list(CBM_core = list(.saveSpinup = TRUE, .saveAll = TRUE)),

      masterRaster = terra::rast(
        crs  = "EPSG:3979",
        ext  = c(xmin = -687696, xmax = -681036, ymin = 714685, ymax = 716185),
        res  = 30,
        vals = 1L
      ),
      standDT = data.table::data.table(
        pixelIndex = 1:terra::ncell(masterRaster),
        admin_name = "Saskatchewan",
        eco_id     = 9,
        area       = prod(terra::res(masterRaster))
      ),
      cohortDT     = file.path(test_path("testdata"), "SK-small/input", "cohortDT.rds")     |> readRDS(),
      gcMeta       = file.path(test_path("testdata"), "SK-small/input", "gcMeta.rds")       |> readRDS(),
      gcIncrements = file.path(test_path("testdata"), "SK-small/input", "gcIncrements.rds") |> readRDS()
    )

    ## In year 1998 pixels 1324-1327 have both a wildfire and "Generic 20% mortality" event;
    ## wildfire takes priority. `disturbanceEvents[pixelIndex %in% 1324:1327 & year == 1998]`
    ## In year 1999 disturbances only occur in unforested pixels
    ## No disturbances in year 2000
    ## This leads to just 1 wildfire event in 1998
    if (disturbances){
      simInitInput$disturbanceMeta   <- file.path(test_path("testdata"), "SK-small/input", "disturbanceMeta.rds")   |> readRDS()
      simInitInput$disturbanceEvents <- file.path(test_path("testdata"), "SK-small/input", "disturbanceEvents.rds") |> readRDS()
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
      cohortDT          = readRDS(file.path(test_path("testdata"), "SK-small/input", "cohortDT.rds")),
      gcMeta            = readRDS(file.path(test_path("testdata"), "SK-small/input", "gcMeta.rds")),
      gcIncrements      = readRDS(file.path(test_path("testdata"), "SK-small/input", "gcIncrements.rds")),
      disturbanceMeta   = readRDS(file.path(test_path("testdata"), "SK-small/input", "disturbanceMeta.rds")),
      disturbanceEvents = readRDS(file.path(test_path("testdata"), "SK-small/input", "disturbanceEvents.rds"))
    )
    outTables <- lapply(setNames(names(refTables), names(refTables)), function(table) simTest[[table]]) |>
      lapply(data.table::copy) |> lapply(data.table::setindex, NULL)

    expect_equal(outTables$cohortDT,     refTables$cohortDT)
    expect_equal(outTables$gcMeta,       refTables$gcMeta)
    expect_equal(outTables$gcIncrements, refTables$gcIncrements)
    if (disturbances){
      expect_equal(outTables$disturbanceMeta,   refTables$disturbanceMeta)
      expect_equal(outTables$disturbanceEvents, refTables$disturbanceEvents)
    }


    ## Check outputs ----

    # emissionsProducts
    expect_true(!is.null(simTest$emissionsProducts))
    expect_equal(
      data.table::as.data.table(simTest$emissionsProducts),
      data.table::fread(file.path(test_path("testdata"), "SK-small/valid", paste0("dist", disturbances), "emissionsProducts.csv"))[
        , .SD, .SDcols = colnames(simTest$emissionsProducts)]
    )

    # Cohort data
    ## There should always be the same number of total cohort groups.
    expect_true(!is.null(simTest$cbm_vars$key))
    expect_identical(simTest$cbm_vars$key$cohortID,   simTest$cohortDT$cohortID)
    expect_identical(simTest$cbm_vars$key$pixelIndex, simTest$cohortDT$pixelIndex)

    nGroupsInit <- 24
    expect_equal(max(simTest$cbm_vars$key$row_idx),            ifelse(!disturbances, nGroupsInit, nGroupsInit + 1))
    expect_equal(length(unique(simTest$cbm_vars$key$row_idx)), ifelse(!disturbances, nGroupsInit, nGroupsInit + 1))
    expect_equal(nrow(simTest$cbm_vars$parameters),            ifelse(!disturbances, nGroupsInit, nGroupsInit + 1))
    expect_equal(nrow(simTest$cbm_vars$state),                 ifelse(!disturbances, nGroupsInit, nGroupsInit + 1))
    expect_equal(nrow(simTest$cbm_vars$flux),                  ifelse(!disturbances, nGroupsInit, nGroupsInit + 1))
    expect_equal(nrow(simTest$cbm_vars$pool),                  ifelse(!disturbances, nGroupsInit, nGroupsInit + 1))

    # Check sw_hw flag
    expect_equal(unique(simTest$cbm_vars$state[species_name == "Black spruce", sw_hw]), 0)
    expect_equal(unique(simTest$cbm_vars$state[species_name == "Jack pine",    sw_hw]), 0)
    expect_equal(unique(simTest$cbm_vars$state[species_name == "White birch",  sw_hw]), 1)

    # Check disturbance event priority
    if (disturbances){
      distPriority <- simTest$cbm_vars$state[row_idx %in% simTest$cbm_vars$key[pixelIndex %in% 1324:1327, row_idx]]
      expect_equal(distPriority$age,                         3)
      expect_equal(distPriority$time_since_last_disturbance, 3)
      expect_equal(distPriority$last_disturbance_type,       1)
    }

    # Check saved data
    if (disturbances){

      outDataDir   <- file.path(simTest$spadesCBMdb, "data")
      validDataDir <- file.path(test_path("testdata"), "SK-small/valid", paste0("dist", disturbances), "cbm_vars")

      for (year in times$start:times$end){
        expect_equal(
          qs2::qd_read(file.path(outDataDir,   paste0(year, "_key.qs2")))[, .(cohortID, pixelIndex, row_idx)],
          qs2::qd_read(file.path(validDataDir, paste0(year, "_key.qs2")))[, .(cohortID, pixelIndex, row_idx)]
        )
        for (table in c("parameters", "state", "flux", "pools")){
          tblValid <- qs2::qd_read(file.path(outDataDir,   paste0(year, "_", table, ".qs2")))
          expect_equal(
            qs2::qd_read(file.path(outDataDir,   paste0(year, "_", table, ".qs2")))[, .SD, .SDcols = names(tblValid)],
            tblValid
          )
        }
      }
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


