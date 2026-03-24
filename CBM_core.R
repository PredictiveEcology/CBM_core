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
    "data.table", "arrow", "dplyr",
    "PredictiveEcology/CBM4r@development",
    "PredictiveEcology/CBMutils@development (>=2.5)"
  ),
  parameters = rbind(
    defineParameter("fixedCohorts", "logical", TRUE, NA, NA, "Stand cohorts are fixed for simulation duration"),
    defineParameter("fixedGrowth",  "logical", TRUE, NA, NA, "Growth curves are fixed for simulation duration"),
    defineParameter("chunks",      "integer", 1L, NA, NA, "Number of partition chunks"),
    defineParameter("max_workers", "integer", NA, NA, NA, "Number of parallel processes"),
    defineParameter("def_delay_spinup", "integer", 0L, 0L, NA, "Default regeneration delay used in the spinup"),
    defineParameter("def_delay_regen",  "integer", 0L, 0L, NA, "Default regeneration delay post disturbance"),
    defineParameter("def_historic_disturbance_type",  "character", "Wildfire", NA, NA, "Default historic disturbance type."),
    defineParameter("def_last_pass_disturbance_type", "character", "Wildfire", NA, NA, "Default last pass disturbance type."),
    defineParameter(".plot",      "logical", TRUE,  NA, NA, "Plot simulation results"),
    defineParameter(".saveAll",   "logical", FALSE, NA, NA, "Save all available data"),
    defineParameter(".useCache",  "logical", FALSE, NA, NA, "Cache module events"),
    defineParameter(".virtualenv", "character", NA, NA, NA, "Python virtual environment"),
    defineParameter(".cbm4vers", "character", "2.17.9", NA, NA, "CBM4 version")
  ),
  inputObjects = bindrows(
    expectsInput(
      objectName = "masterRaster", objectClass = "SpatRaster",
      desc = "Raster grid defining the study area."),
    expectsInput(
      objectName = "standDT", objectClass = "data.table",
      desc = "Table of stand attributes. Stands can have 1 or more cohorts.",
      columns = c(
        pixelIndex  = "Stand ID",
        area        = "Stand area in meters",
        admin_name  = "Canada province or territory name",
        eco_id      = "Canada ecozone ID",
        historic_disturbance_type  = "Optional. Historic disturbance type. Defaults to parameter 'def_historic_disturbance_type'",
        last_pass_disturbance_type = "Optional. Last pass disturbance type. Defaults to parameter 'def_last_pass_disturbance_type'"
      )),
    expectsInput(
      objectName = "cohortDT", objectClass = "data.table",
      #desc = "Table of cohort attributes. All `curveID` columns must be present.",
      desc = "Table of cohort attributes. Must contain one or more additional classifier columns.",
      columns = c(
        cohortID     = "Cohort ID",
        pixelIndex   = "Stand ID",
        age          = "Cohort age at simulation start",
        ageSpinup    = "Optional. Alternative cohort age to use in the spinup",
        delay_spinup = "Optional. Regeneration delay used in the spinup. Defaults to parameter 'def_delay_spinup'",
        delay_regen  = "Optional. Regeneration delay post disturbance in years. Defaults to parameter 'def_delay_regen'"
      )),
    expectsInput(
      objectName = "gcMeta", objectClass = "data.table",
      desc = "Growth curve metadata. One or more `cohortDT` classifiers must be present.",
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
        merch_inc   = "Change in carbon (MgC/ha/year) in merchantable pools",
        foliage_inc = "Change in carbon (MgC/ha/year) in foliage pools",
        other_inc   = "Change in carbon (MgC/ha/year) in other pools"
      )),
    expectsInput(
      objectName = "disturbanceMeta", objectClass = "data.table",
      desc = "Disturbance event types. `cohortDT` classifiers can be present.",
      columns = c(
        eventID               = "Event type ID",
        disturbance_type_name = "Disturbance type name",
        disturbance_type_id   = "Optional. CBM disturbance type ID. Can use this or 'disturbance_type_name'.",
        priority              = "Optional. Priority of event assignment to a pixel if more than one event occurs.",
        description           = "Optional. Disturbance description",
        wholeStand            = "Optional. Specifies if the whole stand is disturbed (1 = TRUE; 0 = FALSE)"
      )),
    expectsInput(
      objectName = "disturbanceEvents", objectClass = "data.table",
      desc = paste(
        "Disturbance events for each simulation year.",
        "The module is indifferent to whether all events are provided as a single initial input",
        "or if they are created by another module during the simulation."),
      columns = c(
        pixelIndex = "Stand ID",
        year       = "Year of disturbance",
        eventID    = "Event type ID. This associates events to metadata in the 'disturbanceMeta' table."
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

      # Initiate CBM4 data directory
      sim <- scheduleEvent(sim, start(sim), "CBM_core", "initCBM4data", eventPriority = 1)

      # Schedule spinup
      sim <- scheduleEvent(sim, start(sim), "CBM_core", "spinup", eventPriority = 5)

      # Schedule annual event
      sim <- scheduleEvent(sim, start(sim), "CBM_core", "annual_disturbances", eventPriority = 8)
      sim <- scheduleEvent(sim, start(sim), "CBM_core", "annual_step",         eventPriority = 10)

      # Schedule plotting
      if (P(sim)$.plot) sim <- scheduleEvent(sim, end(sim), "CBM_core", "plot", eventPriority = 10)
    },

    initCBM4data = {

      sim <- initCBM4data(sim)
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

  # Set up Python virtual environment
  if (is.na(P(sim)$.virtualenv)) P(sim)$.virtualenv <- paste0("r-CBM4-", P(sim)$.cbm4vers)

  message("Setting up CBM4 Python virtual environment: ", P(sim)$.virtualenv)
  CBM4r::cbm4_virtualenv_create(
    P(sim)$.virtualenv,
    version = P(sim)$.cbm4vers,
    python  = CBMutils::ReticulateFindPython(
      version        = ">=3.12,<3.13",
      versionInstall = "3.12:latest",
      pyenvOnly      = TRUE),
    quiet   = Sys.getenv("TESTTHAT") == "true",
    upgrade = FALSE
  )

  # Use Python virtual environment
  reticulate::use_virtualenv(P(sim)$.virtualenv)

  # Return simList
  return(invisible(sim))

}

initCBM4data <- function(sim){

  # Set CBM4 data directory
  sim$CBM4data <- file.path(outputPath(sim), "CBM4data")
  unlink(sim$CBM4data, recursive = TRUE)
  if (file.exists(sim$CBM4data))stop(
    "Failed to remove existing CBM4 spatial dataset directory: ", sim$CBM4data)

  # Write study area grid to simulation dataset
  CBM4r::cbm4_write_geo(
    sim$CBM4data,
    dataset_name = "simulation",
    grid_rast    = sim$masterRaster,
    grid_chunks  = P(sim)$chunks
  )

  # Return simList
  return(invisible(sim))
}

spinup <- function(sim) {

  message("Writing CBM4 dataset: inventory")

  invClassifiers <- setdiff(names(sim$cohortDT), c("cohortID", "pixelIndex", "age", "ageSpinup", "delay_spinup", "delay_regen"))

  invDT <- merge(
    sim$standDT[, .SD, .SDcols = intersect(names(sim$standDT), c(
      "pixelIndex", "area", "admin_name", "eco_id", "eco_name", "spatial_unit_id"
    ))],
    sim$cohortDT, by = "pixelIndex")
  invDT[, cohortID := NULL]

  # Set spinup age
  ## Need to keep original "age" to prevent merging ungroupable cohorts
  if ("ageSpinup" %in% names(invDT)){
    invDT[, ageIn := age]
    invDT[, age   := ageSpinup]
  }

  # Set delay
  if ("delay_spinup" %in% names(invDT)) invDT[, delay := delay_spinup]

  CBM4r::cbm4_write_inventory(
    sim$CBM4data,
    template_name   = "simulation",
    cbm_defaults_db = sim$cbm_defaults_db,
    inventoryDT     = invDT,
    classifiers     = invClassifiers,
    def_delay       = P(sim)$def_delay_spinup,
    def_historic_disturbance_type  = P(sim)$def_historic_disturbance_type,
    def_last_pass_disturbance_type = P(sim)$def_last_pass_disturbance_type
  )
  rm(invDT)

  message("Writing CBM4 dataset: spinup_parameters")
  CBM4r::cbm4_write_spinup_parameters(
    sim$CBM4data,
    template_name   = "simulation",
    cbm_defaults_db = sim$cbm_defaults_db,
    classifiers     = intersect(invClassifiers, names(sim$gcMeta)),
    gcMeta          = sim$gcMeta,
    gcIncr          = sim$gcIncrements
  )

  message("Running CBM4 spinup")
  CBM4r::cbm4_spinup(
    sim$CBM4data,
    cbm_defaults_db = sim$cbm_defaults_db,
    max_workers     = P(sim)$max_workers
  )

  # Alter simulation data to set ages & regeneration delay
  simulation_data <- arrow::open_dataset(file.path(sim$CBM4data, "simulation/simulation"))

  if ("delay_regen" %in% names(sim$cohortDT)){
    simulation_data <- simulation_data |>
      dplyr::mutate(inventory.delay = as.integer(dplyr::if_else(
        !is.na(inventory.delay_regen), inventory.delay_regen, P(sim)$def_delay_regen)))
  }else{
    simulation_data <- simulation_data |>
      dplyr::mutate(inventory.delay = as.integer(P(sim)$def_delay_regen))
  }

  if ("ageSpinup" %in% names(sim$cohortDT)){
    simulation_data <- simulation_data |>
      dplyr::mutate(inventory.age = as.integer(inventory.ageIn))
  }

  simulation_data_pq <- list.files(
    file.path(sim$CBM4data, "simulation/simulation", "timestep=0"),
    recursive = TRUE, full.names = TRUE)

  simulation_data |>
    arrow::write_dataset(
      file.path(sim$CBM4data, "simulation/simulation"),
      partitioning = c("timestep", "cohort_index", "chunk_index"))

  unlink(simulation_data_pq)

  # Read cohort data
  if (!P(sim)$fixedCohorts){

    message("Reading CBM4 dataset: simulation: inventory")
    sim$cohortDT <- CBM4r::cbm4_read_simulation_inventory(sim$CBM4data, timestep = 0)
    sim$cohortDT[, data.table::key(sim$cohortDT) := NULL]
    sim$cohortDT[, cohortID := 1:.N]
    data.table::setkey(sim$cohortDT, cohortID)
    data.table::setnames(sim$cohortDT, "pixel_index", "pixelIndex")
    data.table::setcolorder(sim$cohortDT, c("cohortID", "pixelIndex"))
  }

  # Remove interim datasets
  if (!P(sim)$.saveAll){
    unlink(file.path(sim$CBM4data, c("inventory", "spinup_parameters")), recursive = TRUE)
  }

  # Return simList
  return(invisible(sim))
}

annual_disturbances <- function(sim) {

  message("Writing CBM4 dataset: disturbances")

  invClassifiers <- setdiff(names(sim$cohortDT), c("cohortID", "pixelIndex", "age", "ageSpinup", "delay_spinup", "delay_regen"))

  if (!is.null(sim$disturbanceEvents)){
    distEvents <- sim$disturbanceEvents[year == time(sim),]
    distEvents[, timestep := time(sim) - start(sim) + 1]
    distEvents[, year := NULL]
  }else distEvents <- NULL

  CBM4r::cbm4_write_disturbance(
    sim$CBM4data,
    template_name   = "simulation",
    cbm_defaults_db = sim$cbm_defaults_db,
    classifiers     = intersect(invClassifiers, names(sim$disturbanceMeta)),
    distMeta        = sim$disturbanceMeta,
    distEvents      = distEvents
  )

  # Return simList
  return(invisible(sim))
}

annual_step <- function(sim) {

  # Set timestep
  timestep <- time(sim) - start(sim) + 1

  # Set paths
  if (P(sim)$fixedGrowth){
    parameters_dataset <- file.path(sim$CBM4data, "step_parameters")
  }else{
    parameters_dataset <- file.path(sim$CBM4data, "step_parameters", paste0("timestep=", timestep))
  }

  if (!file.exists(parameters_dataset)){

    message("Writing CBM4 dataset: step_parameters")

    invClassifiers <- setdiff(names(sim$cohortDT), c("cohortID", "pixelIndex", "age", "ageSpinup", "delay_spinup", "delay_regen"))

    CBM4r::cbm4_write_step_parameters(
      sim$CBM4data,
      dataset_path    = parameters_dataset,
      template_name   = "simulation",
      cbm_defaults_db = sim$cbm_defaults_db,
      classifiers     = intersect(invClassifiers, names(sim$gcMeta)),
      gcMeta          = sim$gcMeta,
      gcIncr          = sim$gcIncrements
    )
  }

  if (P(sim)$fixedCohorts){

    simulation_dataset <- file.path(sim$CBM4data, "simulation")

  }else{

    message("Writing CBM4 dataset: simulation_init")

    simulation_dataset <- file.path(sim$CBM4data, "simulation_init")

    if (!file.exists(simulation_dataset)){
      CBM4r::cbm4_copy_dataset(
        sim$CBM4data,
        dataset_name = "simulation",
        dataset_path = simulation_dataset
      )
    }

    CBM4r::cbm4_write_simulation_inventory(
      sim$CBM4data,
      inventoryDT = sim$cohortDT[, cohortID := NULL],
      timestep    = timestep
    )
  }

  message("Running CBM4 annual step")
  CBM4r::cbm4_step(
    sim$CBM4data,
    timestep                = timestep,
    simulation_dataset      = simulation_dataset,
    step_parameters_dataset = parameters_dataset,
    cbm_defaults_db         = sim$cbm_defaults_db,
    max_workers             = P(sim)$max_workers,
    write_parameters        = FALSE
  )

  if (!P(sim)$fixedCohorts){

    # Copy new simulation data to final destination
    newDataDirs <- file.path(c(
      "simulation",
      "simulation-raster_index",
      "simulation-table-annual_process_flux",
      "simulation-table-disturbance_flux",
      "simulation-table-disturbance_raster_index"
    ), paste0("timestep=", timestep))

    for (dir in newDataDirs){
      dir.create(file.path(sim$CBM4data, "simulation", dirname(dir)), showWarnings = FALSE)
      if (file.exists(file.path(simulation_dataset, dir))){
        unlink(file.path(sim$CBM4data, "simulation", dir), recursive = TRUE)
        file.copy(file.path(simulation_dataset, dir),
                  file.path(sim$CBM4data, "simulation", dirname(dir)),
                  recursive = TRUE)
      }
    }

    message("Reading CBM4 dataset: simulation: inventory")
    sim$cohortDT <- CBM4r::cbm4_read_simulation_inventory(sim$CBM4data, timestep = timestep)
    sim$cohortDT[, data.table::key(sim$cohortDT) := NULL]
    sim$cohortDT[, cohortID := 1:.N]
    data.table::setkey(sim$cohortDT, cohortID)
    data.table::setnames(sim$cohortDT, "pixel_index", "pixelIndex")
    data.table::setcolorder(sim$cohortDT, c("cohortID", "pixelIndex"))
  }

  # Remove interim datasets
  if (!P(sim)$.saveAll){

    if (time(sim) == end(sim)){
      interimDirs <- c("step_parameters","disturbance", "simulation_init")
    }else{
      interimDirs <- file.path(c(
        "step_parameters",
        "disturbance/disturbance",
        "disturbance/disturbance-raster_index",
        "simulation_init/simulation",
        "simulation_init/simulation-raster_index",
        "simulation_init/simulation-table-annual_process_flux",
        "simulation_init/simulation-table-disturbance_flux",
        "simulation_init/simulation-table-disturbance_raster_index",
        "simulation_init/simulation-table-step_parameters"
      ), paste0("timestep=", timestep - 1))
    }

    unlink(file.path(sim$CBM4data, interimDirs), recursive = TRUE)
  }

  message("Summarizing yearly emissions and products")
  emissionsProducts <- cbind(
    CBM4r::cbm4_results_products_by_timestep(sim$CBM4data, timestep = timestep),
    CBM4r::cbm4_results_emissions_by_timestep(sim$CBM4data, timestep = timestep)[, timestep := NULL]
  )[, year := time(sim)]

  sim$emissionsProducts <- rbind(sim$emissionsProducts, emissionsProducts)
  data.table::setkey(sim$emissionsProducts, year)
  data.table::setcolorder(sim$emissionsProducts)

  # Return simList
  return(invisible(sim))

}

plot <- function(sim){

  stop("CBM4 results cannot yet be plotted")

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


