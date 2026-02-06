defineModule(sim, list(
  name = "CBM_core",
  description = "Modules that simulated the annual events as described in the CBM-CFS model", # "insert module description here",
  keywords = c("carbon", "CBM-CFS"),
  authors = c(
    person("Céline",  "Boisvenue", email = "celine.boisvenue@nrcan-rncan.gc.ca", role = c("aut", "cre")),
    person("Camille", "Giuliano",  email = "camsgiu@gmail.com",                  role = c("ctb")),
    person("Susan",   "Murray",    email = "murray.e.susan@gmail.com",           role = c("ctb"))
  ),
  childModules = character(0),
  version = list(CBM_core = "0.0.2"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "CBM_core.Rmd"),
  reqdPkgs = list(
    "data.table", "reticulate", "RSQLite", "arrow", "dplyr", "terra",
    "PredictiveEcology/CBMutils@development (>=2.5)"
  ),
  parameters = rbind(
    defineParameter("virtualenv", "character", "r-spadesCBM4", NA, NA, "Python virtual environment"),
    defineParameter("partitions",  "integer", 1L, NA, NA, "Number of chunk partitions"),
    defineParameter("max_workers", "integer", NA, NA, NA, "Number of parallel processes"),
    defineParameter("def_delay_spinup", "integer", 0L, 0L, NA, "Default regeneration delay used in the spinup"),
    defineParameter("def_delay_regen",  "integer", 0L, 0L, NA, "Default regeneration delay post disturbance"),
    defineParameter("def_historic_disturbance_type",  "character", "Wildfire", NA, NA, "Default historic disturbance type."),
    defineParameter("def_last_pass_disturbance_type", "character", "Wildfire", NA, NA, "Default last pass disturbance type."),
    defineParameter(".plot",      "logical", TRUE,  NA, NA, "Plot simulation results"),
    defineParameter(".saveAll",   "logical", FALSE, NA, NA, "Save all available data"),
    defineParameter(".useCache",  "logical", FALSE, NA, NA, "Cache module events")
  ),
  inputObjects = bindrows(
    expectsInput(
      objectName = "masterRaster", objectClass = "SpatRaster",
      desc = "Raster grid defining the study area."),
    expectsInput(
      objectName = "standDT", objectClass = "data.table",
      desc = "Table of stand attributes. Stands can have 1 or more cohorts.",
      columns = c(
        pixelIndex      = "Stand ID",
        area            = "Stand area in meters",
        admin_boundary  = "Canada province or territory name",
        eco_boundary_id = "Canada ecozone ID",
        spatial_unit_id = "Optional. CBM spatial unit ID.",
        historic_disturbance_type  = "Optional. Historic disturbance type. Defaults to parameter 'def_historic_disturbance_type'",
        last_pass_disturbance_type = "Optional. Last pass disturbance type. Defaults to parameter 'def_last_pass_disturbance_type'"
      )),
    expectsInput(
      objectName = "classifiers", objectClass = "character",
      desc = "Column names of attributes defining cohorts in `cohortDT` and `gcMeta`."),
    expectsInput(
      objectName = "cohortDT", objectClass = "data.table",
      desc = "Table of cohort attributes. All `classifiers` columns must be present.",
      columns = c(
        cohortID     = "Cohort ID",
        pixelIndex   = "Stand ID",
        age          = "Cohort age at simulation start",
        age_spinup   = "Optional. Alternative cohort age to use in the spinup",
        delay_spinup = "Optional. Regeneration delay used in the spinup. Defaults to parameter 'def_delay_spinup'",
        delay_regen  = "Optional. Regeneration delay post disturbance in years. Defaults to parameter 'def_delay_regen'"
      )),
    expectsInput(
      objectName = "gcMeta", objectClass = "data.table",
      desc = "Growth curve metadata. Some `classifiers` columns must be present.",
      columns = c(
        gcID  = "Growth curve ID",
        sw_hw = "'sw' or 'hw'"
      )),
    expectsInput(
      objectName = "gcIncrements", objectClass = "data.table",
      desc = "Growth curve increments",
      columns = c(
        gcID        = "Growth curve ID",
        age         = "Cohort age",
        merch_inc   = "merch_inc",   #TODO: define
        foliage_inc = "foliage_inc", #TODO: define
        other_inc   = "other_inc"    #TODO: define
      )),
    expectsInput(
      objectName = "distMeta", objectClass = "data.table",
      desc = "Table defining the disturbance event types. This can include 'classifiers' columns.",
      columns = c(
        eventID             = "Event type ID",
        disturbance_type    = "Disturbance type name",
        disturbance_type_id = "Optional. CBM disturbance type ID. Can use this or 'disturbance_type'.",
        priority            = "Optional. Priority of event assignment to a pixel if more than one event occurs.",
        description         = "Optional. Disturbance description",
        wholeStand          = "Optional. Specifies if the whole stand is disturbed (1 = TRUE; 0 = FALSE)"
      )),
    expectsInput(
      objectName = "distEvents", objectClass = "data.table",
      desc = paste(
        "Table with disturbance events for each simulation year.",
        "The module is indifferent to whether all events are provided as a single initial input",
        "or if they are created by another module during the simulation."),
      columns = c(
        pixelIndex = "Stand ID",
        year       = "Year of disturbance",
        eventID    = "Event type ID. This associates events to metadata in the 'distMeta' table."
      )),
    expectsInput(
      objectName = "cbm_defaults_db", objectClass = "character",
      desc = "Path to CBM defaults SQLite database",
      sourceURL = "https://raw.githubusercontent.com/cat-cfs/libcbm_py/main/libcbm/resources/cbm_defaults_db/cbm_defaults_v1.2.9300.391.db"
    )
  ),
  outputObjects = bindrows(
    createsOutput(
      objectName = "CBM4data", objectClass = "character",
      desc = "Path to CBM4 spatial dataset directory containing simulation data in Parquet format."),
    createsOutput(
      objectName = "emissionsProducts", objectClass = "data.table",
      desc = paste(
        "Emissions and product totals for each simulation year.",
        "Choose which columns to return with the 'emissionsProductsCols' parameter."))
  )
))

doEvent.CBM_core <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {

      # Initiate module
      sim <- Init(sim)

      # Schedule setting partitions
      sim <- scheduleEvent(sim, start(sim), "CBM_core", "partition", eventPriority = 5)

      # Schedule spinup
      sim <- scheduleEvent(sim, start(sim), "CBM_core", "spinup", eventPriority = 5)

      # Schedule annual event
      sim <- scheduleEvent(sim, start(sim), "CBM_core", "annual_disturbances", eventPriority = 8)
      sim <- scheduleEvent(sim, start(sim), "CBM_core", "annual_step",         eventPriority = 9)

      # Schedule plotting
      if (P(sim)$.plot) sim <- scheduleEvent(sim, end(sim), "CBM_core", "plot", eventPriority = 10)
    },

    partition = {

      sim <- partition(sim)
    },

    spinup = {
      sim <- spinup(sim)
    },

    annual_disturbances = {

      sim <- annual_disturbances(sim)

      sim <- scheduleEvent(sim, time(sim) + 1, "CBM_core", "annual_disturbances", eventPriority = 8)
    },

    annual_step = {

      sim <- annual_step(sim)

      sim <- scheduleEvent(sim, time(sim) + 1, "CBM_core", "annual_step", eventPriority = 9)
    },

    plot = {
      sim <- plot(sim)
    },

    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

Init <- function(sim){

  # Set CBM4 data directory
  sim$CBM4data <- file.path(outputPath(sim), "CBM4data")
  unlink(sim$CBM4data, recursive = TRUE)
  if (file.exists(sim$CBM4data))stop(
    "Failed to remove existing CBM4 spatial dataset directory: ", sim$CBM4data)
  dir.create(sim$CBM4data, recursive = TRUE)

  # Set up Python virtual environment
  if (!reticulate::virtualenv_exists(P(sim)$virtualenv)) stop(
    shQuote(P(sim)$virtualenv), " does not exist.",
    "CBM_core cannot yet set up the CBM4 virtual environment.")

  # Use Python virtual environment
  reticulate::use_virtualenv(P(sim)$virtualenv)

  # Return simList
  return(invisible(sim))

}

partition <- function(sim){

  if (P(sim)$partitions != 1) stop(">1 partition chunks not yet supported")

  if (P(sim)$partitions == 1){
    sim$standDT[, chunk_index  := 0]
    sim$standDT[, raster_index := pixelIndex - 1]
  }

  # Return simList
  return(invisible(sim))
}

spinup <- function(sim) {

  message("Writing CBM4 dataset: inventory")
  cohortKey <- cbm4_write_inventory(
    sim$CBM4data,
    cbm_defaults_db = sim$cbm_defaults_db,
    masterRaster    = sim$masterRaster,
    classifiers     = sim$classifiers,
    cohortDT        = sim$cohortDT,
    standDT         = sim$standDT,
    def_delay       = P(sim)$def_delay_spinup,
    def_historic_disturbance_type  = P(sim)$def_historic_disturbance_type,
    def_last_pass_disturbance_type = P(sim)$def_last_pass_disturbance_type
  )

  # Save cohort key
  cohortKey[, year     := time(sim)]
  cohortKey[, timestep := 0]
  arrow::write_dataset(dplyr::group_by(cohortKey, year), file.path(sim$CBM4data, "spades", "cohort_key"))

  message("Writing CBM4 dataset: spinup_parameters")
  cbm4_write_spinup_parameters(
    sim$CBM4data,
    cbm_defaults_db = sim$cbm_defaults_db,
    classifiers     = sim$classifiers,
    gcMeta          = sim$gcMeta,
    gcIncr          = sim$gcIncrements
  )

  message("Running CBM4 spinup")
  cbm4_spinup(
    sim$CBM4data,
    cbm_defaults_db = sim$cbm_defaults_db,
    max_workers     = P(sim)$max_workers
  )

  # Set regeneration delay
  simDataPath <- file.path(sim$CBM4data, "simulation", "simulation")

  if ("delay_regen" %in% names(sim$cohortDT)){

    cohortKey[, delay_regen := sim$cohortDT$delay_regen[match(cohortID, sim$cohortDT$cohortID)]]

    simData <- arrow::open_dataset(simDataPath) |> dplyr::collect() |> data.table::as.data.table()
    simData[, inventory.delay := NULL]
    simData <- merge(
      simData, unique(cohortKey[, .(cohort_index, chunk_index, inventory.delay = delay_regen)]),
      by = c("cohort_index", "chunk_index"))

    simData <- dplyr::group_by(simData, timestep, cohort_index, chunk_index)
    arrow::write_dataset(simData, simDataPath, existing_data_behavior = "delete_matching")

  }else{

    arrow::open_dataset(simDataPath) |>
      dplyr::mutate(inventory.delay = P(sim)$def_delay_regen) |>
      dplyr::group_by(timestep, cohort_index, chunk_index) |>
      dplyr::collect() |>
      arrow::write_dataset(simDataPath, existing_data_behavior = "delete_matching")
  }

  # Remove inventory and spinup_parameters datasets
  if (!P(sim)$.saveAll){
    unlink(file.path(sim$CBM4data, "inventory"),         recursive = TRUE)
    unlink(file.path(sim$CBM4data, "spinup_parameters"), recursive = TRUE)
  }

  # Return simList
  return(invisible(sim))
}

annual_disturbances <- function(sim){

  message("Writing CBM4 dataset: disturbances")

  if (!is.null(sim$distEvents)){
    distEvents <- sim$distEvents[year == time(sim),]
    distEvents[, timestep := time(sim) - start(sim) + 1]
  }else distEvents <- NULL

  cbm4_write_disturbance(
    sim$CBM4data,
    cbm_defaults_db = sim$cbm_defaults_db,
    standDT         = sim$standDT,
    distMeta        = sim$distMeta,
    distEvents      = distEvents
  )

  # Return simList
  return(invisible(sim))
}

annual_step <- function(sim) {

  message("Writing CBM4 dataset: step_parameters")
  cbm4_write_step_parameters(
    sim$CBM4data,
    cbm_defaults_db = sim$cbm_defaults_db,
    classifiers     = sim$classifiers,
    gcMeta          = sim$gcMeta,
    gcIncr          = sim$gcIncrements
  )

  message("Running CBM4 annual step")
  cbm4_step(
    sim$CBM4data,
    cbm_defaults_db = sim$cbm_defaults_db,
    timestep        = time(sim) - start(sim) + 1,
    max_workers     = P(sim)$max_workers
  )

  message("Summarizing yearly emissions and products")

  stepPools <- file.path(sim$CBM4data, "simulation/simulation") |>
    arrow::open_dataset() |>
    dplyr::filter(timestep == time(sim) - start(sim) + 1) |>
    dplyr::select(inventory.area, cohort_proportion, pools.Products) |>
    dplyr::collect() |>
    dplyr::summarise(Products = sum(pools.Products * (inventory.area * cohort_proportion)))

  stepFlux <- file.path(sim$CBM4data, "simulation/simulation-table-annual_process_flux") |>
    arrow::open_dataset() |>
    dplyr::filter(timestep == time(sim) - start(sim) + 1) |>
    dplyr::select(inventory.area, cohort_proportion, DecayDOMCO2Emission) |>
    dplyr::collect() |>
    dplyr::summarise(CO2 = sum(DecayDOMCO2Emission * (inventory.area * cohort_proportion)))

  if (file.exists(file.path(sim$CBM4data, "simulation/simulation-table-disturbance_flux"))){

    distFlux <- file.path(sim$CBM4data, "simulation/simulation-table-disturbance_flux") |>
      arrow::open_dataset() |>
      dplyr::filter(timestep == time(sim) - start(sim) + 1) |>
      dplyr::select(area, cohort_proportion,
                    DisturbanceBioCO2Emission, DisturbanceDOMCO2Emission,
                    DisturbanceBioCH4Emission, DisturbanceDOMCH4Emission,
                    DisturbanceBioCOEmission,  DisturbanceDOMCOEmission) |>
      dplyr::collect() |>
      dplyr::summarise(
        CO2 = sum(DisturbanceBioCO2Emission * (area * cohort_proportion),
                  DisturbanceDOMCO2Emission * (area * cohort_proportion)),
        CH4 = sum(DisturbanceBioCH4Emission * (area * cohort_proportion),
                  DisturbanceDOMCH4Emission * (area * cohort_proportion)),
        CO  = sum(DisturbanceBioCOEmission  * (area * cohort_proportion),
                  DisturbanceDOMCOEmission  * (area * cohort_proportion))
      )

  }else distFlux <- data.table::data.table(CO2 = 0, CH4 = 0, CO = 0)

  sim$emissionsProducts <- rbind(sim$emissionsProducts, data.table::data.table(
    year      = time(sim),
    Products  = stepPools$Products,
    Emissions = sum(stepFlux, distFlux),
    CO2       = sum(stepFlux$CO2, distFlux$CO2),
    CH4       = distFlux$CH4,
    CO        = distFlux$CO
  ))

  # Return simList
  return(invisible(sim))

}

plot <- function(sim){

  stop("Plotting not yet possible for CBM4")

  figPath <- file.path(outputPath(sim), "CBM_core_figures")

  cPlot <- CBMutils::simPlotEmissionsProducts(sim)
  SpaDES.core::Plots(cPlot,
                     filename = "emissionsProducts",
                     path = figPath,
                     ggsaveArgs = list(width = 14, height = 5, units = "in", dpi = 300),
                     types = "png")
  rm(cPlot)
  gc(full = FALSE, verbose = FALSE)

  if (is.null(P(sim)$.saveInitial)) return(invisible())

  saveYears <- seq(from = as.numeric(P(sim)$.saveInitial),
                   to   = as.numeric(time(sim)),
                   by   = as.numeric(P(sim)$.saveInterval))

  bPlot <- CBMutils::simPlotPoolProportions(
    sim, years = c(0[P(sim)$.saveSpinup], saveYears), useCache = FALSE)

  SpaDES.core::Plots(bPlot,
                     filename = "poolProportions",
                     path = figPath,
                     ggsaveArgs = list(width = 7, height = 5, units = "in", dpi = 300),
                     types = "png")
  rm(bPlot)
  gc(full = FALSE, verbose = FALSE)

  if (!is.null(sim$masterRaster)){

    nPlotStart <- CBMutils::simMapNPP(
      sim, year = saveYears[[1]], useCache = FALSE)
    SpaDES.core::Plots(nPlotStart,
                       filename = paste0("NPP-", saveYears[[1]]),
                       path = figPath,
                       ggsaveArgs = list(width = 7, height = 5, units = "in", dpi = 300),
                       types = "png")
    rm(nPlotStart)
    gc(full = FALSE, verbose = FALSE)

    nPlotEnd <- CBMutils::simMapNPP(
      sim, year = saveYears[[length(saveYears)]], useCache = FALSE)
    SpaDES.core::Plots(nPlotEnd,
                       filename = paste0("NPP-", saveYears[[length(saveYears)]]),
                       path = figPath,
                       ggsaveArgs = list(width = 7, height = 5, units = "in", dpi = 300),
                       types = "png")
    rm(nPlotEnd)
    gc(full = FALSE, verbose = FALSE)
  }

  if (!is.null(sim$masterRaster)){

    sPlotStart <- CBMutils::simMapTotalCarbon(
      sim, year = saveYears[[1]], useCache = FALSE)
    SpaDES.core::Plots(sPlotStart,
                       filename = paste0("totalCarbon-", saveYears[[1]]),
                       path = figPath,
                       ggsaveArgs = list(width = 7, height = 5, units = "in", dpi = 300),
                       types = "png")
    rm(sPlotStart)
    gc(full = FALSE, verbose = FALSE)

    sPlotEnd <- CBMutils::simMapTotalCarbon(
      sim, year = saveYears[[length(saveYears)]], useCache = FALSE)
    SpaDES.core::Plots(sPlotEnd,
                       filename = paste0("totalCarbon-", saveYears[[length(saveYears)]]),
                       path = figPath,
                       ggsaveArgs = list(width = 7, height = 5, units = "in", dpi = 300),
                       types = "png")
    rm(sPlotEnd)
    gc(full = FALSE, verbose = FALSE)
  }

  # Return simList
  return(invisible(sim))
}

.inputObjects <- function(sim){

  # CBM-CFS3 defaults SQLite database
  if (!suppliedElsewhere("cbm_defaults_db", sim)){

    sim$cbm_defaults_db <- file.path(inputPath(sim), basename(extractURL("cbm_defaults_db")))

    if (!file.exists(sim$cbm_defaults_db)) prepInputs(
      destinationPath = inputPath(sim),
      url         = extractURL("cbm_defaults_db"),
      targetFile  = basename(sim$cbm_defaults_db),
      dlFun       = download.file(extractURL("cbm_defaults_db"), sim$cbm_defaults_db, mode = "wb", quiet = TRUE),
      fun         = NA
    )
  }

  # Return simList
  return(invisible(sim))
}


