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
  version = list(CBM_core = "1.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "CBM_core.Rmd"),
  reqdPkgs = list(
    "data.table", "arrow", "dplyr", "zip", "cli",
    "PredictiveEcology/CBM4r@main", "gert",
    "PredictiveEcology/CBMutils@development (>=2.5.4)"
  ),
  parameters = rbind(
    defineParameter("fixedCohorts", "logical", TRUE, NA, NA, "Stand cohorts are fixed for simulation duration"),
    defineParameter("def_delay_spinup", "integer", 0L, 0L, NA, "Default regeneration delay used in the spinup"),
    defineParameter("def_delay_regen",  "integer", 0L, 0L, NA, "Default regeneration delay post disturbance"),
    defineParameter("def_historic_disturbance_type",  "character", "Wildfire", NA, NA, "Default historic disturbance type."),
    defineParameter("def_last_pass_disturbance_type", "character", "Wildfire", NA, NA, "Default last pass disturbance type."),
    defineParameter(".virtualenv",  "character", "r-CBM4", NA, NA, "Python virtual environment"),
    defineParameter(".cbm4vers",    "character", NA,       NA, NA, "CBM4 version"),
    defineParameter(".useCache",    "logical",   FALSE,    NA, NA, "Cache module events"),
    defineParameter(".useCacheCBM4","logical",   TRUE,     NA, NA, "Cache CBM4 processes"),
    defineParameter(".chunks",      "integer", 1L, NA, NA, "Number of partition chunks"),
    defineParameter(".max_workers", "integer", NA, NA, NA, "Number of parallel processes"),
    defineParameter(".saveAll",     "logical",   FALSE,    NA, NA, "Save all available data"),
    defineParameter(".plot",        "logical",   TRUE,     NA, NA, "Plot simulation results")
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
      desc = "Table of cohort attributes. Must contain one or more additional classifier columns.",
      columns = c(
        pixelIndex   = "Stand ID",
        age          = "Cohort age at simulation start",
        ageSpinup    = "Optional. Alternative cohort age to use in the spinup",
        delay_spinup = "Optional. Regeneration delay used in the spinup. Defaults to parameter 'def_delay_spinup'",
        delay_regen  = "Optional. Regeneration delay post disturbance in years. Defaults to parameter 'def_delay_regen'"
      )),
    expectsInput(
      objectName = "cohortClassifiers", objectClass = "character",
      desc = "Optional. Name(s) of cohort classifier columns. Defaults to all additional `cohortDT` columns.",
    ),
    expectsInput(
      objectName = "gcMeta", objectClass = "data.table",
      desc = "Growth curve metadata. One or more `cohortDT` classifiers must be present.",
      columns = c(
        gcID  = "Growth curve ID",
        sw    = "TRUE (softwood) or FALSE (hardwood)"
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
      desc = "Optional. Path to CBM defaults SQLite database"
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
        "Choose which columns to return with the '.emissions' parameter."))
  )
))

doEvent.CBM_core <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {

      # Initiate module
      sim <- Init(sim)

      # Schedule set stand metadata
      sim <- scheduleEvent(sim, start(sim), "CBM_core", "setStands", eventPriority = 5)

      # Schedule spinup
      sim <- scheduleEvent(sim, start(sim), "CBM_core", "spinup", eventPriority = 5)

      # Schedule annual event
      sim <- scheduleEvent(sim, start(sim), "CBM_core", "annual_disturbances", eventPriority = 8)
      sim <- scheduleEvent(sim, start(sim), "CBM_core", "annual_step",         eventPriority = 10)

      # Schedule plotting
      if (P(sim)$.plot) sim <- scheduleEvent(sim, end(sim), "CBM_core", "plot", eventPriority = 10)
    },

    setStands = {
      sim <- setStands(sim)
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

      sim <- annual_totals(sim)

      sim <- scheduleEvent(sim, time(sim) + 1, "CBM_core", "annual_step", eventPriority = 9)

      # Remove interim data
      if (!P(sim)$.saveAll){
        rmDirs <- "spinup_parameters"
        if (time(sim) == end(sim)){
          rmDirs <- c(rmDirs, "disturbance", "step_parameters", "simulation_init")
        }else{
          timestep <- time(sim) - start(sim) + 1
          rmDirs <- c(rmDirs, do.call(c, lapply(c("disturbance", "simulation_init"), function(dataset){
            dirs <- list.dirs(file.path(sim$CBM4data, dataset))
            dirs[basename(dirs) == paste0("timestep=", timestep - 1)]
          })))
        }
        rmDirs <- rmDirs[file.exists(file.path(sim$CBM4data, rmDirs))]
        for (rmDir in rmDirs){
          message(cli::col_blue("Removing interim data: ", file.path(basename(sim$CBM4data), rmDir)))
          unlink(file.path(sim$CBM4data, rmDir), recursive = TRUE)
        }
      }
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

  # Set CBM defaults SQLite database path
  if (!is.null(sim$cbm_defaults_db)) options("CBM4r.db.path" = sim$cbm_defaults_db)

  # Set virtual environment
  if (Sys.getenv("VIRTUAL_ENV") == ""){

    message("Setting up CBM4 Python virtual environment: ", P(sim)$.virtualenv)

    CBM4r::cbm4_virtualenv_create(
      P(sim)$.virtualenv,
      version = if (!is.na(P(sim)$.cbm4vers)) P(sim)$.cbm4vers,
      python  = CBMutils::ReticulateFindPython(
        version        = ">=3.12,<3.13",
        versionInstall = "3.12:latest",
        pyenvOnly      = TRUE),
      quiet   = Sys.getenv("TESTTHAT") == "true",
      upgrade = FALSE
    )

    # Use Python virtual environment
    reticulate::use_virtualenv(P(sim)$.virtualenv)
  }

  # Return simList
  return(invisible(sim))

}

setStands <- function(sim){

  message("Setting stand metadata")

  # Convert to data.table
  if (!data.table::is.data.table(sim$standDT)) sim$standDT <- data.table::as.data.table(sim$standDT)

  # Rename table columns for duration of module event
  cbm4_table_setnames(sim)
  on.exit(cbm4_table_setnames_revert(sim))

  # Set stand metadata
  CBM4r::cbm4_set_grid_meta(sim$standDT, grid_rast = sim$masterRaster)

  message("Writing stand metadata to CBM4 data")
  dir.create(sim$CBM4data)
  arrow::write_parquet(sim$standDT, file.path(sim$CBM4data, "grid_meta.parquet"))

  # Return simList
  return(invisible(sim))

}

spinup <- function(sim) {

  # Get classifiers
  if (is.null(sim$cohortClassifiers)){
    sim$cohortClassifiers <- setdiff(names(sim$cohortDT), c(
      "cohortID", "pixelIndex", "age", "ageSpinup", "delay_spinup", "delay_regen"))
  }

  # Convert to data.table
  for (table in c("cohortDT", "gcMeta", "gcIncrements")){
    if (!data.table::is.data.table(sim[[table]])) sim[[table]] <- data.table::as.data.table(sim[[table]])
  }

  # Set default delays
  for (delay in c("spinup", "regen")) if (paste0("delay_", delay) %in% names(sim$cohortDT)){
    data.table::setnafill(sim$cohortDT, fill = P(sim)[[paste0("def_delay_", delay)]], cols = paste0("delay_", delay))
  }

  # Rename table columns for duration of module event
  cohortRename <- c("delay" = "delay_spinup")
  if ("ageSpinup" %in% names(sim$cohortDT)){
    cohortRename <- c(cohortRename, c("ageSpinup" = "age", "age" = "ageIn"))
  }
  cbm4_table_setnames(sim, cohortRename)
  on.exit(cbm4_table_setnames_revert(sim, cohortRename))

  message("Writing CBM4 dataset: inventory")
  CBM4r::cbm4_write_inventory(
    cbm4_data   = sim$CBM4data,
    grid_meta   = sim$standDT,
    grid_rast   = sim$masterRaster,
    cohorts     = sim$cohortDT,
    classifiers = sim$cohortClassifiers,
    col_ignore  = "cohortID",
    def_delay                      = P(sim)$def_delay_spinup,
    def_historic_disturbance_type  = P(sim)$def_historic_disturbance_type,
    def_last_pass_disturbance_type = P(sim)$def_last_pass_disturbance_type
  ) |>
    reproducible::Cache(
      omitArgs    = "cbm4_data",
      .cacheExtra = digestFile(sim$cbm_defaults_db),
      useCache    = P(sim)$.useCacheCBM4,
      verbose     = P(sim)$.useCacheCBM4) |>
    CacheCBM4dataset(sim$CBM4data, "inventory")

  message("Writing CBM4 dataset: spinup_parameters")
  CBM4r::cbm4_write_spinup_parameters(
    cbm4_data   = sim$CBM4data,
    gc_meta     = sim$gcMeta,
    gc_incr     = sim$gcIncrements,
    classifiers = intersect(sim$cohortClassifiers, names(sim$gcMeta))
  ) |>
    reproducible::Cache(
      omitArgs    = "cbm4_data",
      .cacheExtra = digestFile(sim$cbm_defaults_db),
      useCache    = P(sim)$.useCacheCBM4,
      verbose     = P(sim)$.useCacheCBM4) |>
    CacheCBM4dataset(sim$CBM4data, "spinup_parameters")

  message("Running CBM4 spinup")
  if (P(sim)$.useCacheCBM4) cbm4_data_digest <- digestDir(sim$CBM4data)
  CBM4r::cbm4_spinup(
    cbm4_data   = sim$CBM4data,
    max_workers = P(sim)$.max_workers
  ) |>
    reproducible::Cache(
      omitArgs    = c("cbm4_data", "max_workers"),
      .cacheExtra = c(digestFile(sim$cbm_defaults_db), cbm4_data_digest),
      useCache    = P(sim)$.useCacheCBM4,
      verbose     = P(sim)$.useCacheCBM4) |>
    CacheCBM4dataset(sim$CBM4data, "simulation")

  # Alter simulation data to set true ages & regeneration delay
  simulation_data <- arrow::open_dataset(file.path(sim$CBM4data, "simulation/simulation"))

  if ("delay_regen" %in% names(sim$cohortDT)){
    simulation_data <- dplyr::mutate(simulation_data, inventory.delay = inventory.delay_regen)
  }else{
    simulation_data <- dplyr::mutate(simulation_data, inventory.delay = as.integer(P(sim)$def_delay_regen))
  }
  simulation_data <- dplyr::mutate(simulation_data, state.regeneration_delay = inventory.delay)

  if ("ageSpinup" %in% names(sim$cohortDT)){
    simulation_data <- dplyr::mutate(simulation_data, inventory.age = as.integer(inventory.ageIn))
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
    sim$cohortDT <- CBM4r::cbm4_read_simulation_inventory(
      sim$CBM4data,
      grid_meta = sim$standDT,
      timestep  = 0
    )
  }

  # Return simList
  return(invisible(sim))
}

annual_disturbances <- function(sim) {

  # Rename table columns for duration of module event
  cbm4_table_setnames(sim)
  on.exit(cbm4_table_setnames_revert(sim))

  message("Writing CBM4 dataset: disturbances")

  if (!is.null(sim$disturbanceEvents) && nrow(sim$disturbanceEvents) > 0){

    # Convert to data.table
    for (table in c("disturbanceMeta", "disturbanceEvents")){
      if (!data.table::is.data.table(sim[[table]])) sim[[table]] <- data.table::as.data.table(sim[[table]])
    }

    distEvents <- sim$disturbanceEvents[year == time(sim)]
    distEvents[, timestep := time(sim) - start(sim) + 1]
    distEvents[, year := NULL]

    # Choose disturbance events by priority
    multiEvents <- distEvents[, .(N = .N, disturbance_id = list(disturbance_id)), by = c("pixel_index", "timestep")][N > 1,]
    if (nrow(multiEvents) > 0){

      if (!"priority" %in% names(sim$disturbanceMeta)) stop(
        "Multiple disturbance events found in one or more pixels. ",
        "Use the disturbanceMeta \"priority\" column to set event precendence.")

      multiEvents <- multiEvents[, .(disturbance_id = unlist(disturbance_id)), by = c("pixel_index", "timestep")]
      multiEvents <- merge(multiEvents, sim$disturbanceMeta, by = "disturbance_id", all.x = TRUE)

      multiEvents[, pri_highest := priority %in% min(priority), by = c("pixel_index", "timestep")]
      multiEvents <- multiEvents[pri_highest == TRUE, .(N = .N, disturbance_id = first(disturbance_id)), by = c("pixel_index", "timestep")]

      if (any(multiEvents$N > 1)) stop(
        "Multiple disturbance events found in one or more pixels ",
        "and disturbanceMeta \"priority\" indicates events have the same priority.")

      distEvents <- rbind(
        distEvents[!multiEvents, on = c("pixel_index", "timestep")],
        distEvents[multiEvents,  on = c("pixel_index", "timestep", "disturbance_id")][, .SD, .SDcols = names(distEvents)]
      )
    }

  }else distEvents <- NULL

  CBM4r::cbm4_write_disturbance(
    cbm4_data   = sim$CBM4data,
    grid_meta   = sim$standDT,
    dist_meta   = sim$disturbanceMeta,
    dist_events = distEvents,
    classifiers = intersect(sim$cohortClassifiers, names(sim$disturbanceMeta)),
  ) |>
    reproducible::Cache(
      omitArgs    = "cbm4_data",
      .cacheExtra = digestFile(sim$cbm_defaults_db),
      useCache    = P(sim)$.useCacheCBM4,
      verbose     = P(sim)$.useCacheCBM4) |>
    CacheCBM4dataset(sim$CBM4data, "disturbance")

  # Return simList
  return(invisible(sim))
}

annual_step <- function(sim) {

  # Rename table columns for duration of module event
  cbm4_table_setnames(sim)
  on.exit(cbm4_table_setnames_revert(sim))

  # Set timestep
  timestep <- time(sim) - start(sim) + 1

  # Write inventory
  if (P(sim)$fixedCohorts){

    simulation_dataset <- file.path(sim$CBM4data, "simulation")

  }else{

    message("Writing CBM4 dataset: simulation_init")

    simulation_dataset <- file.path(sim$CBM4data, "simulation_init")

    if (!file.exists(simulation_dataset)){
      CBM4r::cbm4_copy_dataset(
        cbm4_data     = sim$CBM4data,
        dataset_name  = "simulation",
        dataset_path  = simulation_dataset
      )
    }

    CBM4r::cbm4_write_simulation_inventory(
      cbm4_data    = sim$CBM4data,
      grid_meta    = sim$standDT,
      dataset_path = simulation_dataset,
      timestep     = timestep - 1,
      cohorts      = sim$cohortDT,
      classifiers  = sim$cohortClassifiers,
      col_ignore   = "cohortID",
      def_state.regeneration_delay = P(sim)$def_delay_regen
    )
  }

  # Write parameters
  message("Writing CBM4 dataset: step_parameters")
  CBM4r::cbm4_write_step_parameters(
    cbm4_data   = sim$CBM4data,
    gc_meta     = sim$gcMeta,
    gc_incr     = sim$gcIncrements,
    classifiers = intersect(sim$cohortClassifiers, names(sim$gcMeta))
  ) |>
    reproducible::Cache(
      omitArgs    = "cbm4_data",
      .cacheExtra = digestFile(sim$cbm_defaults_db),
      useCache    = P(sim)$.useCacheCBM4,
      verbose     = P(sim)$.useCacheCBM4) |>
    CacheCBM4dataset(sim$CBM4data, "step_parameters")

  message("Running CBM4 annual step")
  CBM4r::cbm4_step(
    cbm4_data          = sim$CBM4data,
    timestep           = timestep,
    simulation_dataset = simulation_dataset,
    max_workers        = P(sim)$.max_workers
  )

  if (!P(sim)$fixedCohorts){

    # Copy new simulation data to final destination
    newDataDirs <- list.dirs(simulation_dataset, full.names = FALSE)
    newDataDirs <- newDataDirs[basename(newDataDirs) == paste0("timestep=", timestep)]
    for (dir in newDataDirs){
      dir.create(file.path(sim$CBM4data, "simulation", dirname(dir)), showWarnings = FALSE)
      file.copy(file.path(simulation_dataset, dir),
                file.path(sim$CBM4data, "simulation", dirname(dir)),
                recursive = TRUE)
    }

    message("Reading CBM4 dataset: simulation: inventory")
    sim$cohortDT <- CBM4r::cbm4_read_simulation_inventory(
      sim$CBM4data,
      grid_meta = sim$standDT,
      timestep  = timestep
    )
  }

  # Return simList
  return(invisible(sim))

}

annual_totals <- function(sim) {

  message("Summarizing yearly emissions and products")

  # Set timestep
  timestep <- time(sim) - start(sim) + 1

  # Read results
  cbm4_results <- CBM4r::cbm4_results_processor(sim$CBM4data, max_workers = P(sim)$.max_workers)

  emissionsProducts <- merge(
    CBM4r::cbm4_results_totals(
      cbm4_results,
      timestep     = timestep,
      view_name    = "composite_flux_indicators",
      view_columns = c(
        "CH4" = "Emissions - Emissions By Gas - Total CH4",
        "CO"  = "Emissions - Emissions By Gas - Total CO",
        "CO2" = "Emissions - Emissions By Gas - Total CO2"
      )),
    CBM4r::cbm4_results_totals(
      cbm4_results,
      timestep     = timestep,
      view_name    = "composite_disturbance_indicators",
      view_columns = c(
        "Products" = "Ecosystem Transfers - Ecosystem to Forest Products - Total Harvest (Biomass + Snags)"
      )),
    all = TRUE)[, .(
      year      = as.integer(time(sim)),
      timestep  = timestep,
      Products  = data.table::fcoalesce(Products, 0),
      Emissions = CO2 + CH4 + CO,
      CO2       = CO2,
      CH4       = CH4,
      CO        = CO
    )]

  sim$emissionsProducts <- rbind(sim$emissionsProducts, emissionsProducts)
  data.table::setkey(sim$emissionsProducts, year)
  data.table::setcolorder(sim$emissionsProducts)

  # Return simList
  return(invisible(sim))

}

plot <- function(sim){

  figPath <- file.path(outputPath(sim), "CBM_core_figures")

  cbm4_results <- CBM4r::cbm4_results_processor(sim$CBM4data, max_workers = P(sim)$.max_workers)

  # Emissions and products
  SpaDES.core::Plots(
    CBMutils::simPlotEmissionsProducts(sim, cbm4_results = cbm4_results),
    filename = "emissionsProducts",
    path = figPath,
    ggsaveArgs = list(width = 14, height = 5, units = "in", dpi = 300),
    types = "png")

  # Pool proportions
  SpaDES.core::Plots(
    CBMutils::simPlotPoolProportions(sim, cbm4_results = cbm4_results),
    filename = "poolProportions",
    path = figPath,
    ggsaveArgs = list(width = 7, height = 5, units = "in", dpi = 300),
    types = "png")

  # NPP
  for (year in c(start(sim), end(sim))){

    SpaDES.core::Plots(
      CBMutils::simMapNPP(sim, year = year, cbm4_results = cbm4_results),
      filename = paste0("NPP-", year),
      path = figPath,
      ggsaveArgs = list(width = 7, height = 5, units = "in", dpi = 300),
      types = "png")
  }

  # Total carbon
  for (year in c(start(sim), end(sim))){

    SpaDES.core::Plots(
      CBMutils::simMapTotalCarbon(sim, year = year, cbm4_results = cbm4_results),
      filename = paste0("totalCarbon-", year),
      path = figPath,
      ggsaveArgs = list(width = 7, height = 5, units = "in", dpi = 300),
      types = "png")
  }

  # Return simList
  return(invisible(sim))
}

.inputObjects <- function(sim){

  if (isTRUE(P(sim)$.useCache)) stop(
    "CBM_core module does not support event caching. Set parameter .useCache = FALSE and .useCacheCBM4 = TRUE")
  P(sim)$.useCacheCBM4 <- getOption("reproducible.useCache", TRUE) & P(sim)$.useCacheCBM4

  # Return simList
  return(invisible(sim))
}


