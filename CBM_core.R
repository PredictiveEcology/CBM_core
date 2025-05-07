defineModule(sim, list(
  name = "CBM_core",
  description = "Modules that simulated the annual events as described in the CBM-CFS model", # "insert module description here",
  keywords = c("carbon", "CBM-CFS"),
  authors = person("Celine", "Boisvenue", email = "celine.boisvenue@nrcan-rncan.gc.ca", role = c("aut", "cre")),
  childModules = character(0),
  version = list(CBM_core = "0.0.2"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "CBM_core.Rmd"),
  reqdPkgs = list(
    "data.table", "reticulate",
    "PredictiveEcology/CBMutils@development",
    "PredictiveEcology/LandR@development (>= 1.1.1)",
    "PredictiveEcology/libcbmr"
  ),
  parameters = rbind(
    defineParameter(
      "default_delay", "integer", default = 0L, min = 0L, max = NA_integer_, desc = paste(
        "The default regeneration delay post disturbance.",
        "This can instead be set for each cohort with the spatialDT 'delay' column."
      )),
    defineParameter(
      "default_historical_disturbance_type", "integer", default = 1L, NA_integer_, NA_integer_, desc = paste(
        "The default historical disturbance type ID. Examples: 1 = wildfire; 2 = clearcut.",
        "This can instead be set for each stand with the spatialDT 'historical_disturbance_type' column."
      )),
    defineParameter(
      "default_last_pass_disturbance_type", "numeric", default = 1L, NA_integer_, NA_integer_, desc = paste(
        "The default last pass disturbance type ID. Examples: 1 = wildfire; 2 = clearcut.",
        "This can instead be set for each stand with the spatialDT 'last_pass_disturbance_type' column."
      )),
    defineParameter(
      "emissionsProductsCols", "character", c("CO2", "CH4", "CO", "Emissions"), NA_character_, NA_character_,
      desc = "A vector of columns to return for emissions and products"),
    defineParameter(
      "poolsToPlot", "character", default = "totalCarbon", NA, NA,
      desc = "which carbon pools to plot, if any. Defaults to total carbon"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA, "Simulation time when the first plot event should occur"),
    defineParameter(".plotInterval",    "numeric", 1L,         NA, NA, "Time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA,         NA, NA, "Simulation time when the first save event should occur"),
    defineParameter(".saveInterval",    "numeric", NA,         NA, NA, "Time interval between save events"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Use module caching")
  ),
  inputObjects = bindrows(
    expectsInput(
      objectName = "standDT", objectClass = "data.table", sourceURL = NA,
      desc = "Table of stand attributes. Stands can have 1 or more cohorts.",
      columns = c(
        pixelIndex      = "Stand ID",
        area            = "Stand area in meters",
        spatial_unit_id = "CBM-CFS3 spatial unit ID",
        historical_disturbance_type = "Historic CBM-CFS3 disturbance type ID. Defaults to the 'historical_disturbance_type' parameter",
        last_pass_disturbance_type  = "Last pass CBM-CFS3 disturbance type ID. Defaults to the 'last_pass_disturbance_type' parameter"
      )),
    expectsInput(
      objectName = "cohortDT", objectClass = "data.table", sourceURL = NA,
      desc = "Table of cohort attributes",
      columns = c(
        cohortID   = "Cohort ID",
        pixelIndex = "Stand ID",
        gcids      = "Growth curve ID",
        ages       = "Cohort age at simulation start",
        ageSpinup  = "Optional. Alternative cohort age at the simulation start year to use in the spinup",
        delay      = "Optional. Regeneration delay post disturbance in years. Defaults to the 'default_delay' parameter"
      )),
    expectsInput(
      objectName = "gcMeta", objectClass = "data.table", sourceURL = NA,
      desc = "Growth curve metadata",
      columns = c(
        gcids      = "Growth curve ID",
        species_id = "CBM-CFS3 species ID",
        sw_hw      = "'sw' or 'hw'"
      )),
    expectsInput(
      objectName = "growth_increments", objectClass = "data.table", sourceURL = NA,
      desc = "Growth curve increments",
      columns = c(
        gcids       = "Growth curve ID",
        age         = "Cohort age",
        merch_inc   = "merch_inc",   #TODO: define
        foliage_inc = "foliage_inc", #TODO: define
        other_inc   = "other_inc"    #TODO: define
      )),
    expectsInput(
      objectName = "pooldef", objectClass = "character",
      desc = "Vector of names (characters) for each of the carbon pools, with `Input` being the first one",
      sourceURL = NA),
    expectsInput(
      objectName = "spinupSQL", objectClass = "dataset",
      desc = "Table containing many necesary spinup parameters used in CBM_core",
      sourceURL = NA),
    expectsInput(
      objectName = "disturbanceEvents", objectClass = "data.table",
      desc = paste(
        "Table with disturbance events for each simulation year.",
        "Events types are defined in the 'disturbanceMeta' table.",
        "The module is indifferent to whether all events are provided as a single initial input",
        "or if they are created by another module during the simulation."),
      columns = c(
        pixelIndex = "Stand ID",
        year       = "Year of disturbance occurance",
        eventID    = "Event type ID. This associates events to metadata in the 'disturbanceMeta' table."
      )),
    expectsInput(
      objectName = "disturbanceMeta", objectClass = "data.table",
      desc = paste(
        "Table defining the disturbance event types.",
        "This associates CBM-CFS3 disturbances with the event IDs in the 'disturbanceEvents' table."),
      columns = c(
        eventID               = "Event type ID",
        wholeStand            = "Specifies if the whole stand is disturbed (1 = TRUE; 0 = FALSE)",
        sw_hw                 = "Optional. Specifies if the disturbance applies to SW or HW",
        priority              = "Optional. Priority of event assignment to a pixel if more than one event occurs.",
        spatial_unit_id       = "Spatial unit ID",
        disturbance_type_id   = "Disturbance type ID",
        disturbance_matrix_id = "Disturbance matrix ID",
        name                  = "Disturbance name",
        description           = "Disturbance description"
      )),
    expectsInput(
      objectName = "masterRaster", objectClass = "raster",
      desc = "Raster template for stand pixels. If provided, it is used to map results")
  ),
  outputObjects = bindrows(
    createsOutput(
      objectName = "spinupInput", objectClass = "data.table",
      desc = "Spinup inputs"),
    createsOutput(
      objectName = "spinupResult", objectClass = "data.frame",
      desc = "Spinup results"),
    createsOutput(
      objectName = "cbmPools", objectClass = "data.frame",
      desc = "Cohort group ages and pools"),
    createsOutput(
      objectName = "NPP", objectClass = "data.table",
      desc = "Cohort group NPP"),
    createsOutput(
      objectName = "emissionsProducts", objectClass = "data.table",
      desc = paste(
        "Emissions and product totals for each simulation year.",
        "Choose which columns to return with the 'emissionsProductsCols' parameter.")),
    createsOutput(
      objectName = "cohortGroups", objectClass = "data.table",
      desc = "Cohort group shared attributes"),
    createsOutput(
      objectName = "cohortGroupKeep", objectClass = "data.table",
      desc = paste(
        "Key connecting `cohortID` with current and previous `cohortGroupID`",
        "associations for each year of the simulation")),
    createsOutput(
      objectName = "cbm_vars", objectClass = "list",
      desc = paste(
        "List of 4 data tables: parameters, pools, flux, and state.",
        "This is created initially during the spinup and updated each year."))
  )
))

doEvent.CBM_core <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {

      # Initiate module
      sim <- Init(sim)

      # Schedule spinup
      sim <- scheduleEvent(sim, start(sim), "CBM_core", "spinup")

      # Schedule annual event
      sim <- scheduleEvent(sim, start(sim), eventPriority = 10, "CBM_core", "annual")

      # need this to be after the saving of outputs -- so very low priority
      ##TODO this is not happening because P(sim)$.plotInterval is NULL
      # sim <- scheduleEvent(sim, min(end(sim), start(sim) + P(sim)$.plotInterval),
      #                      "CBM_core", "accumulateResults", eventPriority = 11)
      ##So, I am making this one until we figure out how to do both more
      ##generically
      sim <- scheduleEvent(sim, end(sim), "CBM_core", "accumulateResults", eventPriority = 11)
      # sim <- scheduleEvent(sim, end(sim), "CBM_core", "plot",  eventPriority = 12)


      #sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "CBM_core", "save")
      #sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "CBM_core", "plot", eventPriority = 12 )
      # sim <- scheduleEvent(sim, end(sim), "CBM_core", "savePools", .last())
    },

    spinup = {

      sim <- spinup(sim)
    },

    annual = {

      sim <- annual(sim)
      sim <- scheduleEvent(sim, time(sim) + 1, eventPriority = 10, "CBM_core", "annual")
    },

    accumulateResults = {
      outputDetails <- as.data.table(outputs(sim))
      objsToLoad <- c("cbmPools", "NPP")
      for (objToLoad in objsToLoad) {
        if (any(outputDetails$objectName == objToLoad)) {
          out <- lapply(outputDetails[objectName == objToLoad & saved == TRUE]$file, function(f) {
            readRDS(f)
          })
          sim[[objToLoad]] <- rbindlist(out)
        }
      }
    },

    plot = {
      ## TODO: spatial plots at .plotInterval; summary plots at end(sim) --> separate into 2 plot event types
      figPath <- file.path(outputPath(sim), "CBM_core_figures")
      if (time(sim) != start(sim)) {
        cPlot <- carbonOutPlot(
          emissionsProducts = sim$emissionsProducts)
        SpaDES.core::Plots(cPlot,
                           filename = "carbonOutPlot",
                           path = figPath,
                           ggsaveArgs = list(width = 14, height = 5, units = "in", dpi = 300),
                           types = "png")

        bPlot <- barPlot(
          cbmPools = sim$cbmPools)
        SpaDES.core::Plots(bplot,
                           filename = "barPlot",
                           path = figPath,
                           ggsaveArgs = list(width = 7, height = 5, units = "in", dpi = 300),
                           types = "png")

        if (!is.null(sim$masterRaster)){
          nPlot <- NPPplot(
            spatialDT = sim$spatialDT,
            NPP = sim$NPP,
            masterRaster = sim$masterRaster)
          SpaDES.core::Plots(nPlot,
                             filename = "NPPTest",
                             path = figPath,
                             types = "png")
        }
      }

      if (!is.null(sim$masterRaster)){
        spatialPlot(
          cbmPools = sim$cbmPools,
          years = time(sim),
          masterRaster = sim$masterRaster,
          spatialDT = sim$spatialDT
        )
      }

      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "CBM_core", "plot", eventPriority = 12)
    },

    savePools = {

      data.table::fwrite(
        sim$cbmPools[, .SD, .SDcols = c("simYear", "cohortGroupID", "N", "age", sim$pooldef)],
        file.path(outputPath(sim), "cPoolsPixelYear.csv"))
    },

    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

Init <- function(sim){

  # Set up Python virtual environment
  reticulate::virtualenv_create(
    "r-spadesCBM",
    python = if (!reticulate::virtualenv_exists("r-spadesCBM")){
      CBMutils::ReticulateFindPython(version = ">=3.9,<=3.12.7", versionInstall = "3.10:latest")
    },
    packages = c(
      "numpy<2",
      "pandas>=1.1.5",
      "scipy",
      "numexpr>=2.8.7",
      "numba",
      "pyyaml",
      "mock",
      "openpyxl",
      "libcbm"
    ))

  # Use Python virtual environment
  reticulate::use_virtualenv("r-spadesCBM")

  # Return simList
  return(invisible(sim))

}

spinup <- function(sim) {

  if (!"delay" %in% names(sim$cohortDT)) message(
    "Spinup using the default regeneration delay: ", P(sim)$default_delay)
  if (!"historical_disturbance_type" %in% names(sim$standDT)) message(
    "Spinup using the default historical disturbance type ID: ", P(sim)$default_historical_disturbance_type)
  if (!"last_pass_disturbance_type"  %in% names(sim$standDT)) message(
    "Spinup using the default last pass disturbance type ID: ", P(sim)$default_last_pass_disturbance_type)

  # Use alternative ages for spinup
  ##TODO: confirm if still the case where CBM_vol2biomass won't translate <3 years old
  cohortDT <- sim$cohortDT
  if ("ageSpinup" %in% names(sim$cohortDT)){
    cohortDT <- data.table::copy(cohortDT)
    data.table::setnames(cohortDT, c("ages", "ageSpinup"), c("agesReal", "ages"))
  }

  ## Use an area of 1m for each pixel
  ## Results will later be multiplied by area to total emissions
  cohortSpinup <- cbmExnSpinupCohorts(
    cohortDT      = cohortDT,
    standDT       = sim$standDT[, .SD, .SD = setdiff(names(sim$standDT), "area")],
    gcMetaDT      = sim$gcMeta,
    gcIndex       = "gcids",
    default_area  = 1,
    default_delay = P(sim)$default_delay,
    default_historical_disturbance_type = P(sim)$default_historical_disturbance_type,
    default_last_pass_disturbance_type  = P(sim)$default_last_pass_disturbance_type
  )

  # Spinup
  spinupOut <- cbmExnSpinup(
    cohortDT   = cohortSpinup,
    spinupSQL  = sim$spinupSQL[, mean_annual_temperature := historic_mean_temperature],
    growthIncr = sim$growth_increments,
    gcIndex    = "gcids"
  ) |> Cache()

  # Save spinup input and output
  sim$spinupInput  <- spinupOut["increments"]
  sim$spinupResult <- spinupOut$output$pools

  # If LandRCBM, adjust biomass values to match LandR.
  if ("LandRCBM_split3pools" %in% modules(sim)) {
    # 1. Expand spinup output to have 1 row per cohort
    spinupOut$output <- lapply(spinupOut$output, function(tbl){
      tbl <- tbl[spinupOut$key$cohortGroupID,]
    })
    
    # 2. Replace above ground pools with the LandR biomass.
    spinupOut$output$pools[, c("Merch", "Foliage", "Other")] <- sim$aboveGroundBiomass[,.(merch, foliage, other)]
    
    # 3. Update below ground live pools.
    #### DC 06-05-2025 VALUES ARE HARDCODED - TODO get values from cbm_exn_get_default_parameters?
    #### Confirm equation are correct and wrap into a CBMutils function?
    totAGB <- rowSums(spinupOut$output$pools[, c("Merch", "Foliage", "Other")])
    # convert to mg/ha of total biomass
    totAGB <- totAGB * 2
    rootTotBiom <- ifelse(spinupOut$output$state$sw_hw == 1,
                          0.222 * totAGB,
                          1.576 * totAGB ^ 0.615)
    # reconvert to carbon tonnes/ha
    rootTotC <- rootTotBiom * 0.5
    fineRootProp <- 0.072 + 0.354 * exp(-0.060212 * rootTotC)
    spinupOut$output$pools$CoarseRoots <- rootTotC * (1-fineRootProp) 
    spinupOut$output$pools$FineRoots <- rootTotC * fineRootProp 
    
    # 4. Update cohortGroupID
    spinupOut$key$cohortGroupID <- spinupOut$key$cohortID
  }
  
  # Save cohort group key
  sim$cohortGroupKeep <- merge(spinupOut$key, sim$cohortDT, by = "cohortID")[, .(cohortID, pixelIndex, cohortGroupID)]
  sim$cohortGroupKeep[, cohortGroupPrev := NA_integer_]
  sim$cohortGroupKeep[, spinup          := cohortGroupID]
  data.table::setkey(sim$cohortGroupKeep, cohortID)

  # Prepare spinup output data for annual event
  ## data.table with row_idx to match cohortGroupID
  if ("LandRCBM_split3pools" %in% modules(sim)) {
    sim$cbm_vars <- lapply(spinupOut$output, function(tbl){
      tbl <- data.table::data.table(row_idx = sim$cohortGroupKeep$cohortGroupID, tbl)
      data.table::setkey(tbl, row_idx)
      tbl
    })
  } else {
    sim$cbm_vars <- lapply(spinupOut$output, function(tbl){
      tbl <- data.table::data.table(row_idx = unique(spinupOut[["increments"]]$row_idx), tbl)
      data.table::setkey(tbl, row_idx)
      tbl
    })
  }

  # Prepare cohort group attributes for annual event
  sim$cohortGroups <- unique(merge(
    sim$cohortGroupKeep[, .(cohortID, cohortGroupID)],
    merge(sim$standDT[, .(pixelIndex, spatial_unit_id)], sim$cohortDT, by = "pixelIndex"),
    by = "cohortID"
  )[, .SD, .SDcols = !c("cohortID", "pixelIndex")])
  data.table::setkey(sim$cohortGroups, cohortGroupID)

  if (any(duplicated(sim$cohortDT$pixelIndex))) warning(
    "emissions and product totals cannot yet be calculated" ,
    "if there are multiple cohorts per stand. ",
    "The 'emissionsProducts` output table has not be created.")

  # Return simList
  return(invisible(sim))
}

annual <- function(sim) {

  ## READ DISTURBANCES ----

  # Read disturbance metadata
  if (is.null(sim$disturbanceMeta)) stop("'disturbanceMeta' input not found")
  distMeta <- copy(sim$disturbanceMeta)
  data.table::setnames(distMeta, "rasterID", "eventID", skip_absent = TRUE)
  if (!"sw_hw" %in% names(distMeta)) distMeta <- rbind(
    copy(distMeta)[, sw_hw := "sw"], copy(distMeta)[, sw_hw := "hw"])

  ## Check that there is one disturbance_matrix_id for each SPU and disturbance type
  ##TODO: this check doesn't need to happen yearly
  distUnq <- c("spatial_unit_id", "sw_hw", "eventID")
  if (nrow(unique(distMeta[, c(distUnq, "disturbance_matrix_id"), with = FALSE])) >
      nrow(unique(distMeta[, distUnq, with = FALSE]))) stop(
        "'disturbanceMeta' must have only 1 'disturbance_matrix_id' each combination of ",
        paste(sQuote(distUnq), collapse = ", "))

  # Read disturbance events
  if (is.null(sim$disturbanceEvents)) stop("'disturbanceEvents' input not found")
  if (!all(c("pixelIndex", "year", "eventID") %in% names(sim$disturbanceEvents))) stop(
    "'disturbanceEvents' table requires columns: 'pixelIndex', year', 'eventID'")

  distCohorts <- merge(
    sim$cohortGroupKeep[, .(cohortID, pixelIndex, cohortGroupID)],
    subset(sim$disturbanceEvents, year == as.character(time(sim))),
    by = "pixelIndex")

  if (nrow(distCohorts) > 0){

    # Choose events for each pixel based on priority
    if ("priority" %in% names(distMeta)){

      eventPriority <- unique(distMeta[, .(eventID, priority)])
      if (any(duplicated(eventPriority$eventID))){
        stop("'disturbanceMeta' event \"priority\" must be the same for all spatial_unit_id")
      }

      distCohorts <- distCohorts[eventPriority, on = "eventID"]
      distCohorts[order(cohortID, priority)]

    }else if (any(duplicated(unique(distCohorts[, .(pixelIndex, eventID)])$pixelIndex))) warning(
      "Multiple disturbance events found in one or more pixels for year ", time(sim), ". ",
      "Use the 'disturbanceMeta' \"priority\" field to control event precendence.")

    distCohorts <- distCohorts[, eventID := as.integer(first(eventID)), by = "cohortID"][
      , .(cohortID, pixelIndex, cohortGroupID, eventID)]

  }else{

    message("No disturbance events for year ", time(sim))
  }


  ## SET COHORT GROUPS ----

  # Set previous group IDs
  sim$cohortGroupKeep[, cohortGroupPrev := cohortGroupID]
 
  if ("LandRCBM_split3pools" %in% modules(sim)) { # With LandRCBM
    
    # Get the pools for the cohorts of the previous timestep
    cohorts <- merge(sim$cohortGroupKeep[, .(pixelIndex, cohortGroupPrev)],
                     sim$cbm_vars$state[, .(row_idx, age, species_id = species)],
                     by.x = "cohortGroupPrev",
                     by.y = "row_idx")
    
    # Match the cohort pools to this timestep cohorts based on pixel, age, and species.
    cohorts <- merge(
      cohorts[, age := age + 1],
      merge(sim$cohortDT[, .(pixelIndex, age, gcids)], sim$gcMeta[, .(gcids, species_id)]),
      by = c("pixelIndex", "age", "species_id"),
      all = TRUE
    )
    
    # Add spatial unit
    cohorts <- merge(cohorts, sim$standDT, by = "pixelIndex")
    cohorts[, cohortGroupID := gcids]
    
    # Cohorts that have disappeared
    if(any(is.na(cohorts$cohortGroupID))){
      missingCohorts <- cohorts[is.na(cohortGroupID), ]
      missingCohorts[, gcids := 0]
      missingCohorts[, cohortGroupID := .GRP + max(cohorts$cohortGroupID, na.rm = TRUE), by = pixelIndex]
      cohorts[is.na(cohortGroupID), ] <- missingCohorts
    }
    
    # Update cohortGroupKeep
    sim$cohortGroupKeep <- merge(
      sim$cohortGroupKeep[, cohortGroupID := NULL],
      cohorts[, .(pixelIndex, cohortGroupPrev, cohortGroupID)],
      by = c("pixelIndex", "cohortGroupPrev"),
      all.y = TRUE
    )
    setkey(sim$cohortGroupKeep, cohortID)
    
    # Update cohortGroups
    sim$cohortGroups <- unique(cohorts[, .(cohortGroupID, spatial_unit_id, age, gcids)])
    setkey(sim$cohortGroups, cohortGroupID)
    
  } else if (nrow(distCohorts) > 0){ # DC 28.04.2025: In standard CBM, cohorts change if disturbed.
    
    # Get attributes and disturbance information about disturbed cohorts
    distCohorts <- merge(distCohorts, sim$cohortGroups, by = "cohortGroupID")
    distCohorts <- merge(distCohorts, sim$gcMeta[, .(gcids, sw_hw)], by = "gcids", all.x = TRUE)
    distCohorts <- merge(distCohorts, distMeta, by = c("spatial_unit_id", "sw_hw", "eventID"), all.x = TRUE)
    distCohorts <- distCohorts[, .SD, .SDcols = unique(c("cohortID", "pixelIndex", "cohortGroupID", names(distCohorts)))]
    data.table::setnames(distCohorts, "cohortGroupID", "cohortGroupPrev")
    data.table::setkey(distCohorts, cohortID)

    # Create new groups that include events and carbon from
    # previous group since that changes the amount and destination of the
    # carbon being moved.
    cPoolNames <- c("Input", "Merch", "Foliage", "Other", "CoarseRoots", "FineRoots",
                    "AboveGroundVeryFastSoil", "BelowGroundVeryFastSoil",
                    "AboveGroundFastSoil", "BelowGroundFastSoil",
                    "MediumSoil", "AboveGroundSlowSoil", "BelowGroundSlowSoil",
                    "StemSnag", "BranchSnag", "CO2", "CH4", "CO", "NO2", "Products")
    cohortGroupCols <- c(setdiff(names(sim$cohortGroups), "cohortGroupID"), "eventID", cPoolNames)

    distCohortCpools <- merge(
      distCohorts, sim$cbm_vars$pools,
      by.x = "cohortGroupPrev", by.y = "row_idx", all.x = TRUE)
    data.table::setkey(distCohortCpools, cohortID)

    ##TODO: Check why a bunch of extra columns are being created. remove
    ##unnecessary cols from generatePixelGroups.
    distCohortCpools$cohortGroupNew <- LandR::generatePixelGroups(
      distCohortCpools, maxPixelGroup = max(sim$cohortGroupKeep$cohortGroupPrev),
      columns = cohortGroupCols
    )

    # Update cohortGroupKeep
    sim$cohortGroupKeep <- merge(
      sim$cohortGroupKeep, distCohortCpools[, .(cohortID, cohortGroupNew)],
      by = "cohortID", all.x = TRUE)
    sim$cohortGroupKeep[, cohortGroupID  := data.table::fcoalesce(cohortGroupNew, cohortGroupPrev)]
    sim$cohortGroupKeep[, cohortGroupNew := NULL]
    setkey(sim$cohortGroupKeep, cohortID)

    # Update cohortGroups
    sim$cohortGroups <- rbind(
      sim$cohortGroups,
      unique(distCohortCpools[, cohortGroupID := cohortGroupNew][, .SD, .SDcols = names(sim$cohortGroups)]))
    data.table::setkey(sim$cohortGroups, cohortGroupID)

    rm(distCohortCpools)

  }

  # Set cohort groups for the year
  sim$cohortGroupKeep[[as.character(time(sim))]] <- sim$cohortGroupKeep$cohortGroupID


  ## PREPARE PYTHON INPUTS ----
  if(!("LandRCBM_split3pools" %in% modules(sim))) { # Standard CBM
    
    # Get data for existing groups
    cbm_vars <- lapply(sim$cbm_vars, function(tbl) subset(tbl, row_idx %in% sim$cohortGroupKeep$cohortGroupID))
    
    # Set groups as undisturbed in current year
    ## This may contain the disturbance type from the previous year
    cbm_vars$parameters$disturbance_type <- 0L
    
    # Set ages from state
    cbm_vars$parameters$age <- cbm_vars$state$age
    
    # Prepare data for new groups
    if (nrow(distCohorts) > 0){
      
      cbm_vars_new <- list()
      
      newRowIDs <- unique(
        subset(sim$cohortGroupKeep, cohortGroupID != cohortGroupPrev)[
          , .(row_idx = cohortGroupID, cohortGroupPrev)]
      )
      data.table::setkey(newRowIDs, row_idx)
      
      # Set disturbed group parameters
      ## Set age = 1
      cbm_vars_new[["parameters"]] <- merge(
        newRowIDs[, .(row_idx)],
        unique(merge(distCohorts, sim$cohortGroupKeep, by = "cohortID", all.x = TRUE)[
          , .(row_idx = cohortGroupID, age = 1L, disturbance_type = disturbance_type_id)]),
        by = "row_idx", all.x = TRUE)
      
      # Set disturbed group pools from data of previous group
      ## Set Input = 1
      cbm_vars_new[["pools"]] <- merge(
        newRowIDs, sim$cbm_vars[["pools"]], by.x = "cohortGroupPrev", by.y = "row_idx", all.x = TRUE)[
          , .SD, .SDcols = names(cbm_vars[["pools"]])]
      cbm_vars_new[["pools"]]$Input <- 1L
      
      # Set disturbed group flux from data of previous group
      cbm_vars_new[["flux"]]  <- merge(
        newRowIDs, sim$cbm_vars[["flux"]], by.x = "cohortGroupPrev", by.y = "row_idx", all.x = TRUE)[
          , .SD, .SDcols = names(cbm_vars[["flux"]])]
      
      # Set disturbed group state from data of previous group
      ## Clear information about previous disturbances
      cbm_vars_new[["state"]] <- merge(
        newRowIDs, sim$cbm_vars[["state"]], by.x = "cohortGroupPrev", by.y = "row_idx", all.x = TRUE)[
          , .SD, .SDcols = names(cbm_vars[["state"]])]
      cbm_vars_new[["state"]][, time_since_last_disturbance := NA_real_]
      cbm_vars_new[["state"]][, time_since_land_use_change  := NA_real_]
      cbm_vars_new[["state"]][, last_disturbance_type       := NA_real_]
      
      # Merge new group data
      for (tableName in names(cbm_vars)){
        cbm_vars[[tableName]] <- data.table::rbindlist(
          list(cbm_vars[[tableName]], unique(cbm_vars_new[[tableName]])), fill = TRUE)
        data.table::setkey(cbm_vars[[tableName]], row_idx)
      }
    }
    
    # Set mean annual temperature
    cbm_vars$parameters <- merge(
      cbm_vars$parameters[, .SD, .SDcols = !"mean_annual_temperature"],
      merge(sim$cohortGroups, sim$spinupSQL, by.x = "spatial_unit_id", by.y = "id")[
        , .(row_idx = cohortGroupID, mean_annual_temperature)],
      by = "row_idx", all.x = TRUE)
    
    # Set growth increments: join via spinup cohort group IDs and age
    annualIncr <- cbm_vars$parameters[, .(row_idx, age)]
    growthIncr <- sim$spinupInput$increments
    data.table::setkeyv(growthIncr, c("row_idx", "age"))
    
    ## JAN 2025: This sets any ages <= 0 to 1. Without this fix we lose cohorts
    annualIncr$age <- replace(annualIncr$age, annualIncr$age <= 0, 1)
    
    ## Extend increments to maximum age found in parameters
    ## This handles cases where the cohort ages exceed what is available in the increments
    maxIncr <- growthIncr[growthIncr[, .I[which.max(age)], by = c("gcids", "row_idx")]$V1,]
    if (any(maxIncr$age < max(cbm_vars$parameters$age))){
      
      warning("Cohort ages exceed growth increment ages. ",
              "Increments for the greatest available age have been applied to older cohorts.")
      
      growthIncr <- rbind(
        growthIncr, data.table::rbindlist(
          lapply(which(maxIncr$age < max(cbm_vars$parameters$age)), function(i){
            cbind(age = (maxIncr[i,]$age + 1):(max(cbm_vars$parameters$age) + 250),
                  maxIncr[i,][, -("age")])
          }), use.names = TRUE))
      data.table::setkeyv(growthIncr, c("row_idx", "age"))
      
      sim$spinupInput$increments <- growthIncr
    }
    
    annualIncr <- merge(
      annualIncr,
      unique(sim$cohortGroupKeep[, .(cohortGroupID, spinup)]),
      by.x = "row_idx", by.y = "cohortGroupID",
      all.x = TRUE)
    annualIncr <- merge(
      annualIncr, growthIncr,
      by.x = c("spinup", "age"), by.y = c("row_idx", "age"),
      all.x = TRUE)
    annualIncr <- unique(annualIncr[, .(row_idx, merch_inc, foliage_inc, other_inc)])
    
    if (any(is.na(annualIncr))) stop(
      "Growth increments not found for ID(s): ", paste(shQuote(as.character(
        unique(subset(annualIncr, is.na(merch_inc) | is.na(foliage_inc) | is.na(other_inc))$gcids)
      )), collapse = ", "))
    
    cbm_vars$parameters <- merge(
      cbm_vars$parameters[, .SD, .SDcols = -c("merch_inc", "foliage_inc", "other_inc")],
      annualIncr, by = "row_idx", all.x = TRUE)
    data.table::setkey(cbm_vars$parameters, row_idx)
    
    rm(annualIncr)
    rm(growthIncr)
  } else { # With LandRCBM
    
    # Update cbm_vars$pools
    new_cbm_pools <- merge(sim$cohortGroupKeep[, .(cohortGroupID, cohortGroupPrev)],
                           sim$cbm_vars$pools,
                           by.x = "cohortGroupPrev",
                           by.y = "row_idx",
                           all.x = TRUE)
    new_cbm_pools[, cohortGroupPrev := NULL]
    setnames(new_cbm_pools, old = "cohortGroupID", new = "row_idx")
    
    # Handle new cohorts
    if(any(is.na(new_cbm_pools))) {
      new_cbm_pools$Input[is.na(new_cbm_pools$Input)] <- 1L
      setnafill(new_cbm_pools, fill = 0L)
    }
    
    # Handle DOM cohorts
    if(any(sim$cohortGroups$gcids == 0)){
      pool_columns <- setdiff(colnames(new_cbm_pools), "row_idx")
      new_cbm_pools <- new_cbm_pools[, lapply(.SD, sum), by = row_idx, .SDcols = pool_columns]
      new_cbm_pools$Input <- 1L
    }
    setkey(new_cbm_pools, row_idx)
    
    # Update cbm_vars$flux
    new_cbm_flux <- merge(sim$cohortGroupKeep[, .(cohortGroupID, cohortGroupPrev)],
                          sim$cbm_vars$flux,
                          by.x = "cohortGroupPrev",
                          by.y = "row_idx",
                          all.x = TRUE)
    new_cbm_flux[, cohortGroupPrev := NULL]
    setnames(new_cbm_flux, old = "cohortGroupID", new = "row_idx")
    
    # Handle new cohorts
    if(any(is.na(new_cbm_flux))) {
      setnafill(new_cbm_flux, fill = 0L)
    }
    
    # Handle DOM cohorts
    if(any(sim$cohortGroups$gcids == 0)){
      flux_columns <- setdiff(colnames(new_cbm_flux), "row_idx")
      new_cbm_flux <- new_cbm_flux[, lapply(.SD, sum), by = row_idx, .SDcols = flux_columns]
    }
    
    setkey(new_cbm_flux, row_idx)
    
    # Update cbm_vars$parameters
    new_cbm_parameters <- merge(sim$cohortGroups,
                                sim$spinupSQL[, .(id, mean_annual_temperature)],
                                by.x = "spatial_unit_id",
                                by.y = "id",
                                all.x = TRUE)
    new_cbm_parameters[, disturbance_type := 0]
    new_cbm_parameters <- merge(new_cbm_parameters,
                                sim$growth_increments,
                                by = c("gcids", "age"),
                                all.x = TRUE)
    # For the cohorts that disappear set increments to 0 and age to 0?
    setnafill(new_cbm_parameters, fill = 0L, cols = c("merch_inc", "foliage_inc", "other_inc"))
    
    new_cbm_parameters <- new_cbm_parameters[, .(
      row_idx = cohortGroupID,
      mean_annual_temperature,
      disturbance_type,
      merch_inc,
      foliage_inc,
      other_inc
    )]
    
    # Update cbm_vars$state
    new_cbm_state <-  merge(sim$cohortGroupKeep[, .(cohortGroupID, cohortGroupPrev)],
                            sim$cbm_vars$state,
                            by.x = "cohortGroupPrev",
                            by.y = "row_idx",
                            all.x = TRUE)
    new_cbm_state[, cohortGroupPrev := NULL]
    setnames(new_cbm_state, old = "cohortGroupID", new = "row_idx")
    
    # Handle DOM cohorts
    if(any(sim$cohortGroups$gcids == 0)){
      DOMcohorts <- sim$cohortGroups[gcids == 0, cohortGroupID]
      new_cbm_state[row_idx %in% DOMcohorts, age := 0]
      new_cbm_state[row_idx %in% DOMcohorts, species := NA]
      new_cbm_state[row_idx %in% DOMcohorts, sw_hw := NA]
      new_cbm_state[row_idx %in% DOMcohorts, time_since_last_disturbance := 0]
      new_cbm_state[row_idx %in% DOMcohorts, time_since_land_use_change  := -1]
      new_cbm_state[row_idx %in% DOMcohorts, last_disturbance_type := -1]
      new_cbm_state <- unique(new_cbm_state)
    }
    
    # Handle new cohorts
    if(any(is.na(new_cbm_state))) {
      newCohorts_cbm_state <- new_cbm_state[is.na(area), ]
      setnafill(newCohorts_cbm_state, fill = 1L, cols = c("area", "last_disturbance_type", "enabled"))
      setnafill(newCohorts_cbm_state, fill = -1L, cols = c("land_class_id", "time_since_land_use_change")) 
      
      # Get gc info
      newCohorts_cbm_state[sim$cohortGroups, on = c("row_idx" = "cohortGroupID"), `:=`(
        spatial_unit_id = fifelse(is.na(spatial_unit_id), i.spatial_unit_id, spatial_unit_id),
        age  = fifelse(is.na(age),  i.age,  age)
      )]
      
      # DC 01-05-2025: gcids and cohortGroupID are the same in LandRCBM.
      newCohorts_gcMeta <- sim$gcMeta[match(newCohorts_cbm_state$row_idx, sim$gcMeta$gcids)]
      newCohorts_cbm_state[, species := newCohorts_gcMeta$species_id]
      newCohorts_cbm_state[, sw_hw := as.integer(newCohorts_gcMeta$sw_hw == "sw")]
      newCohorts_cbm_state[, time_since_last_disturbance := age] # DC 01-05-2025: Make sure that this is correct
      
      # Combine
      new_cbm_state <- rbind(
        new_cbm_state[!is.na(area),],
        newCohorts_cbm_state
      )
    }
    setkey(new_cbm_state, row_idx)
    
    # Put in cbm_vars
    cbm_vars <- list(
      pools = new_cbm_pools[!is.na(row_idx)],
      flux = new_cbm_flux[!is.na(row_idx)],
      parameters = new_cbm_parameters[!is.na(row_idx)],
      state = new_cbm_state[!is.na(row_idx)]
    )
    
    # Add disturbed cohorts
    if (nrow(distCohorts) > 0) {
      # DC 2025-04-30: Have to make sure that disturbed cohorts are in sim$cohortDT. If not needs to add distCohort to sim$cohortGroupKeep
      newDistCohortGroups <- unique(sim$cohortGroupKeep[cohortGroupPrev %in% distCohorts$cohortGroupID, .(cohortGroupID, cohortGroupPrev, pixelIndex)])
      
      # Update CBM parameters
      disturbanceTypes <- merge(
        newDistCohortGroups,
        distCohorts,
        by.x = c("cohortGroupPrev", "pixelIndex"),
        by.y = c("cohortGroupID", "pixelIndex")
      )
      cbm_vars[["parameters"]][disturbanceTypes$cohortGroupID, "disturbance_type"] <- disturbanceTypes$eventID
      # DC 29-04-2025: Not sure what should be the increments for disturbed cohorts.
      cbm_vars[["parameters"]][disturbanceTypes$cohortGroupID, merch_inc := 0L]
      cbm_vars[["parameters"]][disturbanceTypes$cohortGroupID, foliage_inc := 0L]
      cbm_vars[["parameters"]][disturbanceTypes$cohortGroupID, other_inc := 0L]
    }
  }
  
  ## RUN PYTHON -----

  # Temporarily remove row_idx column
  row_idx <- cbm_vars$pools$row_idx
  cbm_vars <- lapply(cbm_vars, function(tbl) tbl[, -("row_idx")])

  # Call Python
  mod$libcbm_default_model_config <- libcbmr::cbm_exn_get_default_parameters()
  step_ops <- libcbmr::cbm_exn_step_ops(cbm_vars, mod$libcbm_default_model_config)

  cbm_vars <- libcbmr::cbm_exn_step(
    cbm_vars,
    step_ops,
    libcbmr::cbm_exn_get_step_disturbance_ops_sequence(),
    libcbmr::cbm_exn_get_step_ops_sequence(),
    mod$libcbm_default_model_config
  )

  # Prepare output data for next annual event
  sim$cbm_vars <- lapply(cbm_vars, function(tbl){
    tbl <- data.table::data.table(row_idx = row_idx, tbl)
    data.table::setkey(tbl, row_idx)
    tbl
  })


  ## ASSEMBLE OUTPUTS -----

  # Set new cohort group ages
  sim$cohortGroups$ages <- sim$cbm_vars$state$age[
    match(sim$cohortGroups$cohortGroupID, sim$cbm_vars$state$row_idx)
  ]

  # Set cohort count
  cohortCount <- sim$cohortGroupKeep[, .(cohortGroupID)][, .N, by = cohortGroupID]
  data.table::setkey(cohortCount, cohortGroupID)

  # Update the final simulation horizon table with all the pools/year/cohortGroupID
  sim$cbmPools <- cbind(
    simYear = as.integer(time(sim)),
    merge(
      cohortCount,
      cbind(sim$cbm_vars$state, sim$cbm_vars$pools[,-1]),
      by.x = "cohortGroupID", by.y = "row_idx")[
        , .SD, .SDcols = c(names(cohortCount), "age", sim$pooldef)]
  )

  # NPP
  ## NPP used in building sim$NPP and for plotting
  NPP <- (
    sim$cbm_vars$flux$DeltaBiomass_AG
    + sim$cbm_vars$flux$DeltaBiomass_BG
    + sim$cbm_vars$flux$TurnoverMerchLitterInput
    + sim$cbm_vars$flux$TurnoverFolLitterInput
    + sim$cbm_vars$flux$TurnoverOthLitterInput
    + sim$cbm_vars$flux$TurnoverCoarseLitterInput
    + sim$cbm_vars$flux$TurnoverFineLitterInput
  )
  sim$NPP <- cbind(
    simYear = as.integer(time(sim)),
    merge(
      cohortCount,
      data.table::data.table(cohortGroupID = sim$cbm_vars$flux$row_idx, NPP = NPP),
      by = "cohortGroupID")
  )

  ############# Update emissions and products
  #Note: details of which source and sink pools goes into each of the columns in
  #cbm_vars$flux can be found here:
  #https://cat-cfs.github.io/libcbm_py/cbm_exn_custom_ops.html
  ##TODO double-check with Scott Morken that the cbm_vars$flux are in metric
  ##tonnes of carbon per ha like the rest of the values produced.

  if (!any(duplicated(sim$cohortDT$pixelIndex))){

    # Summarize emissions
    emissions <- sim$cbm_vars$flux[, .(
      row_idx,
      DisturbanceBioCO2Emission, DisturbanceBioCH4Emission,DisturbanceBioCOEmission,
      DecayDOMCO2Emission,
      DisturbanceDOMCO2Emission, DisturbanceDOMCH4Emission,DisturbanceDOMCOEmission)]

    emissions[, `:=`(Emissions, (DisturbanceBioCO2Emission + DisturbanceBioCH4Emission +
                                 DisturbanceBioCOEmission + DecayDOMCO2Emission +
                                 DisturbanceDOMCO2Emission + DisturbanceDOMCH4Emission +
                                 DisturbanceDOMCOEmission))]
    ##TODO: this combined emissions column might not be needed.

    ##NOTE: SK: CH4 and CO are 0 in 1999 and 2000
    emissions[, `:=`(CO2, (DisturbanceBioCO2Emission + DecayDOMCO2Emission + DisturbanceDOMCO2Emission))]
    emissions[, `:=`(CH4, (DisturbanceBioCH4Emission + DisturbanceDOMCH4Emission))]
    emissions[, `:=`(CO,  (DisturbanceBioCOEmission  + DisturbanceDOMCOEmission))]

    ## Choose emissions columns
    reqCols <- c("CO2", "CH4", "CO", "Emissions")
    epCols  <- intersect(names(emissions), c(P(sim)$emissionsProductsCols, reqCols))
    if (!identical(sort(P(sim)$emissionsProductsCols), sort(epCols))) warning(
      "'emissionsProducts' including required columns: ", paste(shQuote(reqCols), collapse = ", "))
    emissions <- emissions[, .SD, .SDcols = c("row_idx", epCols)]

    # Merge with products
    emissionsProducts <- merge(sim$cbm_vars$pools[, .(row_idx, Products)], emissions, by = "row_idx")

    # Multiply by group areas
    if (!"area" %in% names(sim$standDT)) stop(
      "standDT requires the \"area\" column to calculate emissions and product totals.")

    groupAreas <- unique(merge(
      sim$cohortGroupKeep[, .(pixelIndex, row_idx = cohortGroupID)],
      sim$standDT[, .(pixelIndex, area)],
      by = "pixelIndex", all.x = TRUE)[, pixelIndex := NULL][
        , area := sum(area), by = row_idx])

    emissionsProducts <- colSums(
      emissionsProducts[, .SD, .SDcols = !"row_idx"] *
        (groupAreas$area[match(emissionsProducts$row_idx, groupAreas$row_idx)] / 10000))

    sim$emissionsProducts <- rbind(sim$emissionsProducts, c(simYear = time(sim), emissionsProducts))

  }

  ##TODO Check if fluxes are per year Emissions should not define the
  #pixelGroups, they should be re-zeros every year. Both these values are most
  #commonly required on a yearly basis.

  ##TODO need to track emissions and products. First check that cbm_vars$fluxes
  ##are yearly (question for Scott or we found out by mapping the Python
  ##functions ourselves)


  ## RETURN SIMLIST -----

  return(invisible(sim))

}

.inputObjects <- function(sim){

  return(sim)
}


