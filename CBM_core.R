defineModule(sim, list(
  name = "CBM_core",
  description = "Modules that simulated the annual events as described in the CBM-CFS model", # "insert module description here",
  keywords = c("carbon", "CBM-CFS"),
  authors = c(
    person("Céline",  "Boisvenue", email = "celine.boisvenue@nrcan-rncan.gc.ca", role = c("aut", "cre")),
    person("Susan",   "Murray",    email = "murray.e.susan@gmail.com",           role = c("ctb")),
    person("Camille", "Giuliano",  email = "camsgiu@gmail.com",                  role = c("ctb"))
  ),
  childModules = character(0),
  version = list(CBM_core = "2.0.0.9000"),
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
    defineParameter("spinup", "logical", TRUE, NA, NA, "Run CBM spinup"),
    defineParameter("fixedCohorts", "logical", TRUE, NA, NA, "Stand cohorts are fixed for simulation duration"),
    defineParameter("def_delay_spinup", "integer", 0L, 0L, NA, "Default regeneration delay used in the spinup"),
    defineParameter("def_delay_regen",  "integer", 0L, 0L, NA, "Default regeneration delay post disturbance"),
    defineParameter("def_historic_disturbance_type",  "character", "Wildfire", NA, NA, "Default historic disturbance type."),
    defineParameter("def_last_pass_disturbance_type", "character", "Wildfire", NA, NA, "Default last pass disturbance type."),
    defineParameter(".virtualenv",  "character", "r-CBM4", NA, NA, "Python virtual environment"),
    defineParameter(".cbm4vers",    "character", NA,       NA, NA, "CBM4 version"),
    defineParameter(".useCache",    "logical",   FALSE,    NA, NA, "Cache module events"),
    defineParameter(".useCacheCBM4","logical",   TRUE,     NA, NA, "Cache CBM4 processes"),
    defineParameter(".chunk_size",  "integer", NA, NA, NA, "Number of cohort groups per processing chunk"),
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
        pixelIndex   = "Stand ID",
        admin_name   = "Canada province or territory name",
        admin_abbrev = "Optional. Canada province or territory 2-character abbreviation. 'admin_name' or 'admin_abbrev' required.",
        eco_id       = "Canada ecozone ID",
        historic_disturbance_type  = "Optional. Historic disturbance type. Defaults to parameter `def_historic_disturbance_type`",
        last_pass_disturbance_type = "Optional. Last pass disturbance type. Defaults to parameter `def_last_pass_disturbance_type`"
      )),
    expectsInput(
      objectName = "cohortDT", objectClass = "data.table",
      desc = paste("Table of cohort attributes. Must contain one or more additional classifier columns.",
                   "If parameter `spinup` == FALSE, a column must be present with carbon (t/ha) for every aboveground and belowground pool."),
      columns = c(
        pixelIndex   = "Stand ID",
        age          = "Cohort age at simulation start",
        delay_spinup = "Optional. Regeneration delay used in the spinup. Defaults to parameter `def_delay_spinup`",
        delay        = "Optional. Regeneration delay post disturbance in years. Defaults to parameter `def_delay_regen`"
      )),
    expectsInput(
      objectName = "cohortClassifiers", objectClass = "character",
      desc = "Optional. Name(s) of cohort classifier columns. Defaults to all additional `cohortDT` columns.",
    ),
    expectsInput(
      objectName = "gcMeta", objectClass = "data.table",
      desc = paste("Growth curve metadata. One or more `cohortClassifiers` columns must be present.",
                   "Columns `admin_name`, `admin_abbrev`, and/or `eco_id` may be present to associate curves with specific regions."),
      columns = c(
        gcID  = "Growth curve ID",
        sw    = "TRUE (softwood) or FALSE (hardwood)"
      )),
    expectsInput(
      objectName = "gcIncrements", objectClass = "data.table",
      desc = "Growth curve increments.",
      columns = c(
        gcID        = "Growth curve ID",
        age         = "Cohort age",
        merch_inc   = "Change in carbon (MgC/ha/year) in merchantable pools",
        foliage_inc = "Change in carbon (MgC/ha/year) in foliage pools",
        other_inc   = "Change in carbon (MgC/ha/year) in other pools"
      )),
    expectsInput(
      objectName = "disturbanceMeta", objectClass = "data.table",
      desc = "Disturbance event types. `cohortClassifiers` columns can be present.",
      columns = c(
        eventID               = "Event type ID",
        disturbance_type_name = "Disturbance type name",
        disturbance_type_id   = "Optional. CBM disturbance type ID. Can use this or 'disturbance_type_name'.",
        priority              = "Optional. Priority of event assignment to a pixel if more than one event occurs.",
        description           = "Optional. Disturbance description"
      )),
    expectsInput(
      objectName = "disturbanceEvents", objectClass = "data.table",
      desc = "Disturbance events.",
      columns = c(
        pixelIndex = "Stand ID",
        year       = "Year of disturbance",
        eventID    = "Event type ID. This associates events to metadata in the 'disturbanceMeta' table."
      )),
    expectsInput(
      objectName = "cbm_defaults_db", objectClass = "character",
      desc = paste("Optional. Path to an SQLite database of CBM parameters",
                   "Defaults to the most latest version of the CBM defaults database.")
    )
  ),
  outputObjects = bindrows(
    createsOutput(
      objectName = "CBM4data", objectClass = "character",
      desc = "Path to CBM4 spatial dataset directory containing simulation data in Parquet format."),
    createsOutput(
      objectName = "emissionsProducts", objectClass = "data.table",
      desc = "Emissions and product totals for each simulation year.")
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
      sim <- scheduleEvent(sim, start(sim), "CBM_core", "step", eventPriority = 9)

      # Schedule summaries
      sim <- scheduleEvent(sim, end(sim), "CBM_core", "summarize", eventPriority = 10)
      if (P(sim)$.plot) sim <- scheduleEvent(sim, end(sim), "CBM_core", "plot", eventPriority = 10)
    },

    setStands = {
      sim <- setStands(sim)
    },

    spinup = {

      sim <- spinup(sim)

      if (!P(sim)$fixedCohorts) sim <- readCohorts(sim, timestep = 0)
    },

    step = {

      sim <- step(sim)

      sim <- scheduleEvent(sim, time(sim) + 1, "CBM_core", "step", eventPriority = 9)

      if (!P(sim)$fixedCohorts) sim <- readCohorts(sim)

      # Remove interim data
      if (!P(sim)$.saveAll){
        rmDirs <- file.path(sim$CBM4data, "spinup_parameters")
        if (time(sim) == end(sim)){
          rmDirs <- c(rmDirs, file.path(sim$CBM4data, c("disturbance", "step_parameters")))
        }else{
          rmDirs <- c(rmDirs, file.path(sim$CBM4data, "disturbance", c("disturbance", "disturbance-raster_index")))
        }
        rmDirs <- rmDirs[file.exists(rmDirs)]
        for (rmDir in rmDirs){
          message(cli::col_blue("Removing interim data: ", rmDir))
          unlink(rmDir, recursive = TRUE)
        }
      }
    },

    summarize = {
      sim <- summarize(sim)
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
  message("CBM4 data directory set to: ", sim$CBM4data)

  unlink(sim$CBM4data, recursive = TRUE)
  if (file.exists(sim$CBM4data)) stop(
    "Failed to remove existing CBM4 data directory: ", sim$CBM4data)

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

    # Try importing pyarrow to check for library loading issue
    ## See Github issue: https://github.com/apache/arrow/issues/40073
    tryCatch(
      reticulate::import("pyarrow"),
      error = function(e){
        if (grepl("DLL load failed while importing lib", e$message)){
          stop("Importing Python package pyarrow failed:\n", e$message, "\n",
               "Loading the R arrow package before pyarrow may cause issues due to library incompatibilities. ",
               "This is a known issue: https://github.com/apache/arrow/issues/40073", "\n",
               "Refreshing your R session may resolve the issue. ",
               "If it persists, import pyarrow in your session before running SpaDES using the following code: \n",
               cli::col_green("reticulate::use_virtualenv(\"", P(sim)$.virtualenv, "\")", "\n",
                              "reticulate::import(\"pyarrow\")"),
               call. = FALSE)
        }else stop(e)
    })
  }

  # Return simList
  return(invisible(sim))

}

setStands <- function(sim){

  message("Setting stand metadata")

  if (is.null(sim$masterRaster)) stop("masterRaster not found")

  # Convert to data.table
  if (!data.table::is.data.table(sim$standDT)) sim$standDT <- data.table::as.data.table(sim$standDT)

  # Rename table columns for duration of module event
  cbm4_table_setnames(sim)
  on.exit(cbm4_table_setnames_revert(sim))

  # Set stand metadata
  CBM4r::cbm4_set_grid_meta(
    cbm_defaults_db = sim$cbm_defaults_db,
    grid_meta       = sim$standDT,
    grid_rast       = sim$masterRaster,
    chunk_size      = P(sim)$.chunk_size,
    chunk_meta      = if (!is.na(P(sim)$.chunk_size)){
      if ("cohortID" %in% names(sim$cohortDT)){
        sim$cohortDT[, .SD, .SDcols = setdiff(names(sim$cohortDT), "cohortID")]
      }else sim$cohortDT
    },
    def_historic_disturbance_type  = P(sim)$def_historic_disturbance_type,
    def_last_pass_disturbance_type = P(sim)$def_last_pass_disturbance_type
  )

  if (!is.na(P(sim)$.chunk_size)) message(cli::col_blue(
    "Partitions: ", sum(!is.na(unique(sim$standDT$chunk_index))), " chunk(s) of ", P(sim)$.chunk_size))

  if (P(sim)$.saveAll){
    message("Writing stand metadata to CBM4 data")
    dir.create(sim$CBM4data)
    arrow::write_parquet(sim$standDT, file.path(sim$CBM4data, "grid_meta.parquet"))
  }

  # Return simList
  return(invisible(sim))

}

spinup <- function(sim) {

  # Convert to data.table
  for (table in c("cohortDT", "gcMeta", "gcIncrements")){
    if (!data.table::is.data.table(sim[[table]])) sim[[table]] <- data.table::as.data.table(sim[[table]])
  }

  # Set default delays
  if ("delay_spinup" %in% names(sim$cohortDT)){
    data.table::setnafill(sim$cohortDT, fill = P(sim)$def_delay_spinup, cols = "delay_spinup")
  }
  if ("delay" %in% names(sim$cohortDT)){
    data.table::setnafill(sim$cohortDT, fill = P(sim)$def_delay_regen, cols = "delay")
  }

  # Rename table columns for duration of module event
  cohortRename <- if (P(sim)$spinup) c("delay_spinup" = "delay", "delay" = "delay_regen")
  cbm4_table_setnames(sim, cohortRename)
  on.exit(cbm4_table_setnames_revert(sim, cohortRename))

  if (P(sim)$spinup){

    message("Writing CBM4 dataset: inventory")
    CBM4r::cbm4_write_inventory(
      cbm4_data       = sim$CBM4data,
      cbm_defaults_db = sim$cbm_defaults_db,
      grid_meta       = sim$standDT,
      grid_rast       = sim$masterRaster,
      cohorts         = sim$cohortDT,
      classifiers     = cohortClassifiers(sim),
      col_ignore      = "cohortID",
      def_delay       = P(sim)$def_delay_spinup
    ) |>
      reproducible::Cache(
        omitArgs    = c("cbm4_data", "cbm_defaults_db"),
        .cacheExtra = digestFile(sim$cbm_defaults_db),
        useCache    = P(sim)$.useCacheCBM4,
        verbose     = P(sim)$.useCacheCBM4) |>
      CacheCBM4dataset(sim$CBM4data, "inventory")

    message("Writing CBM4 dataset: spinup_parameters")
    CBM4r::cbm4_write_spinup_parameters(
      cbm4_data       = sim$CBM4data,
      cbm_defaults_db = sim$cbm_defaults_db,
      gc_meta         = sim$gcMeta,
      gc_incr         = sim$gcIncrements
    ) |>
      reproducible::Cache(
        omitArgs    = c("cbm4_data", "cbm_defaults_db"),
        .cacheExtra = digestFile(sim$cbm_defaults_db),
        useCache    = P(sim)$.useCacheCBM4,
        verbose     = P(sim)$.useCacheCBM4) |>
      CacheCBM4dataset(sim$CBM4data, "spinup_parameters")

    message("Running CBM4 spinup")
    if (P(sim)$.useCacheCBM4) cbm4_data_digest <- digestDir(sim$CBM4data)
    CBM4r::cbm4_spinup(
      cbm4_data       = sim$CBM4data,
      cbm_defaults_db = sim$cbm_defaults_db,
      max_workers     = P(sim)$.max_workers
    ) |>
      reproducible::Cache(
        omitArgs    = c("cbm4_data", "cbm_defaults_db", "max_workers"),
        .cacheExtra = cbm4_data_digest,
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

    simulation_data_pq <- list.files(
      file.path(sim$CBM4data, "simulation/simulation", "timestep=0"),
      recursive = TRUE, full.names = TRUE)

    simulation_data |>
      arrow::write_dataset(
        file.path(sim$CBM4data, "simulation/simulation"),
        partitioning = c("timestep", "cohort_index", "chunk_index"))

    unlink(simulation_data_pq)

  }else{

    message("Initiating CBM4 dataset: inventory")
    CBM4r::cbm4_write_inventory(
      cbm4_data       = sim$CBM4data,
      cbm_defaults_db = sim$cbm_defaults_db,
      grid_rast       = sim$masterRaster,
      grid_meta       = sim$standDT,
      classifiers     = cohortClassifiers(sim)
    ) |>
      reproducible::Cache(
        omitArgs    = c("cbm4_data", "cbm_defaults_db"),
        .cacheExtra = digestFile(sim$cbm_defaults_db),
        useCache    = P(sim)$.useCacheCBM4,
        verbose     = P(sim)$.useCacheCBM4) |>
      CacheCBM4dataset(sim$CBM4data, "inventory")

    message("Writing CBM4 dataset: simulation: timestep = 0")
    CBM4r::cbm4_write_simulation(
      cbm4_data       = sim$CBM4data,
      cbm_defaults_db = sim$cbm_defaults_db,
      grid_meta       = sim$standDT,
      cohorts         = sim$cohortDT,
      timestep        = 0,
      def_regeneration_delay = P(sim)$def_delay_regen
    )
  }

  # Return simList
  return(invisible(sim))
}

step <- function(sim) {

  # Rename table columns for duration of module event
  cbm4_table_setnames(sim)
  on.exit(cbm4_table_setnames_revert(sim))

  # Set timestep
  timestep <- time(sim) - start(sim) + 1

  # Write disturbances
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
    cbm4_data       = sim$CBM4data,
    cbm_defaults_db = sim$cbm_defaults_db,
    grid_meta       = sim$standDT,
    dist_meta       = sim$disturbanceMeta,
    dist_events     = distEvents
  ) |>
    reproducible::Cache(
      omitArgs    = c("cbm4_data", "cbm_defaults_db"),
      .cacheExtra = digestFile(sim$cbm_defaults_db),
      useCache    = P(sim)$.useCacheCBM4,
      verbose     = P(sim)$.useCacheCBM4) |>
    CacheCBM4dataset(sim$CBM4data, "disturbance")

  # Write parameters
  message("Writing CBM4 dataset: step_parameters")
  CBM4r::cbm4_write_step_parameters(
    cbm4_data       = sim$CBM4data,
    cbm_defaults_db = sim$cbm_defaults_db,
    gc_meta         = sim$gcMeta,
    gc_incr         = sim$gcIncrements
  ) |>
    reproducible::Cache(
      omitArgs    = c("cbm4_data", "cbm_defaults_db"),
      .cacheExtra = digestFile(sim$cbm_defaults_db),
      useCache    = P(sim)$.useCacheCBM4,
      verbose     = P(sim)$.useCacheCBM4) |>
    CacheCBM4dataset(sim$CBM4data, "step_parameters")

  message("Running CBM4 annual step")
  if (P(sim)$fixedCohorts | timestep == 1){
    CBM4r::cbm4_step(
      cbm4_data       = sim$CBM4data,
      cbm_defaults_db = sim$cbm_defaults_db,
      timestep        = timestep,
      max_workers     = P(sim)$.max_workers
    )

  }else{
    CBM4r::cbm4_step_with_cohorts(
      cbm4_data       = sim$CBM4data,
      cbm_defaults_db = sim$cbm_defaults_db,
      timestep        = timestep,
      max_workers     = P(sim)$.max_workers,
      cohorts         = sim$cohortDT,
      grid_meta       = sim$standDT,
      def_regeneration_delay = P(sim)$def_delay_regen
    )
  }

  # Return simList
  return(invisible(sim))
}

readCohorts <- function(sim, timestep = NULL){

  message("Reading CBM4 dataset: simulation: cohorts")

  # Rename table columns for duration of module event
  cbm4_table_setnames(sim)
  on.exit(cbm4_table_setnames_revert(sim))

  # Set timestep
  if (is.null(timestep)) timestep <- time(sim) - start(sim) + 1

  # Read inventory
  sim$cohortDT <- CBM4r::cbm4_read_cohorts(
    sim$CBM4data,
    grid_meta = sim$standDT,
    timestep  = timestep
  )

  sim$cohortDT[, chunk_index  := NULL]
  sim$cohortDT[, raster_index := NULL]
  sim$cohortDT[, cohort_index := NULL]

  data.table::setnames(sim$cohortDT, "state.age", "age")

  sim$cohortDT[, pools.Input  := NULL]

  # Return simList
  return(invisible(sim))

}

summarize <- function(sim) {

  message("Reading yearly totals for emissions and products")

  cbm4_results <- CBM4r::cbm4_results_processor(sim$CBM4data, max_workers = P(sim)$.max_workers)

  sim$emissionsProducts <- merge(
    CBM4r::cbm4_results_totals(
      cbm4_results,
      view_name    = "composite_flux_indicators",
      view_columns = c(
        "CH4" = "Emissions - Emissions By Gas - Total CH4",
        "CO"  = "Emissions - Emissions By Gas - Total CO",
        "CO2" = "Emissions - Emissions By Gas - Total CO2"
      )),
    CBM4r::cbm4_results_totals(
      cbm4_results,
      view_name    = "composite_disturbance_indicators",
      view_columns = c(
        "Products" = "Ecosystem Transfers - Ecosystem to Forest Products - Total Harvest (Biomass + Snags)"
      )),
    by = "timestep", all = TRUE)[, .(
      year      = as.numeric(start(sim)) + timestep - 1,
      timestep  = timestep,
      Products  = data.table::fcoalesce(Products, 0),
      Emissions = CO2 + CH4 + CO,
      CO2       = CO2,
      CH4       = CH4,
      CO        = CO
    )]
  data.table::setkey(sim$emissionsProducts, year)

  # Return simList
  return(invisible(sim))

}

plot <- function(sim){

  figPath <- file.path(outputPath(sim), "CBM_core_figures")

  cbm4_results <- CBM4r::cbm4_results_processor(sim$CBM4data, max_workers = P(sim)$.max_workers)

  # Emissions and products
  SpaDES.core::Plots(
    CBMutils::cbm4PlotEmissionsProducts(cbm4_results, yearStart = start(sim)),
    filename = "emissionsProducts",
    path = figPath,
    ggsaveArgs = list(width = 14, height = 5, units = "in", dpi = 300),
    types = "png")

  # Pool proportions
  SpaDES.core::Plots(
    CBMutils::cbm4PlotPoolProportions(cbm4_results, yearStart = start(sim)),
    filename = "poolProportions",
    path = figPath,
    ggsaveArgs = list(width = 7, height = 5, units = "in", dpi = 300),
    types = "png")

  # Net Primary Productivity (NPP)
  plotsNPP <- CBMutils::cbm4MapNPP(cbm4_results, yearStart = start(sim), years = c(start(sim), end(sim)))
  for (year in names(plotsNPP)){
    SpaDES.core::Plots(
      plotsNPP[[year]],
      filename = paste0("NPP-", year),
      path = figPath,
      ggsaveArgs = list(width = 7, height = 5, units = "in", dpi = 300),
      types = "png")
  }
  rm(plotsNPP)

  # Total carbon
  plotsTC <- CBMutils::cbm4MapTotalCarbon(cbm4_results, yearStart = start(sim), years = c(start(sim), end(sim)))
  for (year in names(plotsTC)){
    SpaDES.core::Plots(
      plotsTC[[year]],
      filename = paste0("totalCarbon-", year),
      path = figPath,
      ggsaveArgs = list(width = 7, height = 5, units = "in", dpi = 300),
      types = "png")
  }
  rm(plotsTC)

  # Return simList
  return(invisible(sim))
}

.inputObjects <- function(sim){

  if (isTRUE(P(sim)$.useCache)) stop(
    "CBM_core module does not support event caching. Set parameter .useCache = FALSE and .useCacheCBM4 = TRUE")
  P(sim)$.useCacheCBM4 <- getOption("reproducible.useCache", TRUE) & P(sim)$.useCacheCBM4

  if (!suppliedElsewhere("cbm_defaults_db", sim)){
    sim$cbm_defaults_db <- getOption("CBM4r.db.path")
  }

  # Return simList
  return(invisible(sim))
}


