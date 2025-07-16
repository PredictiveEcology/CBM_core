
#' Spinup
#'
#' Spinup cohort data with libcbmr.
cbmExnSpinupCBM <- function(
    cohortDT, standDT, gcMetaDT,
    colname_gc    = "gcids",
    colname_age   = "age",
    colname_delay = "delay",
    ...){

  # On exit: restore cohortDT table
  cohortInput <- list(key = data.table::key(cohortDT), cols = names(cohortDT))
  on.exit({
    cohortDT <- cohortDT[, .SD, .SDcols = cohortInput$cols]
    data.table::setkeyv(cohortDT, cohortInput$key)
  })

  # Read input tables
  reqCols <- list(
    cohortDT = c("cohortID", "pixelIndex", colname_gc, colname_age),
    standDT  = c("pixelIndex", "spatial_unit_id"),
    gcMetaDT = c(colname_gc, "species_id", "sw_hw")
  )
  cohortDT <- readDataTable(cohortDT, "cohortDT", colRequired = reqCols$cohortDT)
  standDT  <- readDataTable(standDT,  "standDT",  colRequired = reqCols$standDT)
  gcMetaDT <- readDataTable(gcMetaDT, "gcMetaDT", colRequired = reqCols$gcMetaDT)

  # Join all cohort data
  cohortDT <- cohortDT |>
    merge(standDT,  by = "pixelIndex", all.x = TRUE) |>
    merge(gcMetaDT, by = colname_gc,   all.x = TRUE)
  data.table::setkey(cohortDT, cohortID)

  # Set age column
  if (colname_age != "age"){
    data.table::setnames(cohortDT, c("age", colname_age), c("age_in", "age"), skip_absent = TRUE)
    on.exit(
      data.table::setnames(cohortDT, c("age_in", "age"), c("age", colname_age), skip_absent = TRUE),
      add = TRUE, after = FALSE)
  }

  # Set delay column
  if (colname_delay != "delay"){
    data.table::setnames(cohortDT, c("delay", colname_delay), c("delay_in", "delay"), skip_absent = TRUE)
    on.exit(
      data.table::setnames(cohortDT, c("delay_in", "delay"), c("delay", colname_delay), skip_absent = TRUE),
      add = TRUE, after = FALSE)
  }

  # Spinup
  cbmExnSpinup(cohortDT = cohortDT, colname_gc = colname_gc, ...)
}


#' Spinup
#'
#' Spinup cohort data with libcbmr.
cbmExnSpinup <- function(cohortDT, growthIncr, spinupSQL, colname_gc = "gcids",
                         default_delay = 0L,
                         default_historical_disturbance_type = 1L,
                         default_last_pass_disturbance_type  = 1L){

  ## Prepare input for spinup ----

  # Read input tables
  reqCols <- list(
    cohortDT   = c("cohortID", "spatial_unit_id", colname_gc, "species_id", "sw_hw", "age"),
    growthIncr = c(colname_gc, "age", "merch_inc", "foliage_inc", "other_inc"),
    spinupSQL  = c("id", "return_interval", "min_rotations", "max_rotations", "mean_annual_temperature")
  )
  cohortDT   <- readDataTable(cohortDT,   "cohortDT",   colRequired = reqCols$cohortDT)
  growthIncr <- readDataTable(growthIncr, "growthIncr", colRequired = reqCols$growthIncr)
  spinupSQL  <- readDataTable(spinupSQL,  "spinupSQL",  colRequired = reqCols$spinupSQL)

  # Create cohort groups: groups of cohorts with the same attributes
  ## Allow all cohortDT attributes to be considered in unique groupings
  groupCols <- setdiff(names(cohortDT), c("cohortID", "pixelIndex"))
  cohortDT[, cohortGroupID := .GRP, by = groupCols]
  on.exit(cohortDT[, cohortGroupID := NULL])

  # Isolate unique groups and join with spatial unit data
  cohortGroups <- unique(cohortDT[, .SD, .SDcols = c("cohortGroupID", groupCols)])
  cohortGroups <- merge(cohortGroups, spinupSQL, by.x = "spatial_unit_id", by.y = "id", all.x = TRUE)
  setkey(cohortGroups, cohortGroupID)

  # Prepare sw_hw column for Python
  data.table::setnames(cohortGroups, "species_id", "species")
  if (is.character(cohortGroups$sw_hw)) cohortGroups$sw_hw <- as.integer(cohortGroups$sw_hw == "sw")

  # Set area to 1ha
  cohortGroups$area <- 1L # 1ha

  # Set defaults
  if ("delay" %in% names(cohortGroups)){
    cohortGroups$delay[is.na(delay), delay := default_delay]
  }else{
    cohortGroups$delay <- default_delay
  }
  if ("historical_disturbance_type" %in% names(cohortGroups)){
    cohortGroups[is.na(historical_disturbance_type), historical_disturbance_type := default_historical_disturbance_type]
  }else{
    cohortGroups$historical_disturbance_type <- default_historical_disturbance_type
  }
  if ("last_pass_disturbance_type" %in% names(cohortGroups)){
    cohortGroups[is.na(last_pass_disturbance_type), last_pass_disturbance_type := default_last_pass_disturbance_type]
  }else{
    cohortGroups$last_pass_disturbance_type <- default_last_pass_disturbance_type
  }

  # Join growth increments with cohort group IDs
  ## Drop growth increments age <= 0
  growthIncrGroups <- merge(
    cohortGroups[, .SD, .SDcols = c("cohortGroupID", colname_gc)],
    subset(growthIncr, age > 0),
    by = colname_gc, allow.cartesian = TRUE)

  growthIncrGroups <- data.table::data.table(
    row_idx = growthIncrGroups$cohortGroupID,
    growthIncrGroups[, -("cohortGroupID")])
  data.table::setkeyv(growthIncrGroups, c("row_idx", "age"))


  ## Spinup ----

  spinup_input <- list(
    parameters = cohortGroups,
    increments = growthIncrGroups
  )

  mod$libcbm_default_model_config <- libcbmr::cbm_exn_get_default_parameters()
  spinup_op_seq <- libcbmr::cbm_exn_get_spinup_op_sequence()

  spinup_ops <- libcbmr::cbm_exn_spinup_ops(
    spinup_input, mod$libcbm_default_model_config
  )

  cbm_vars <- libcbmr::cbm_exn_spinup(
    spinup_input,
    spinup_ops,
    spinup_op_seq,
    mod$libcbm_default_model_config
  )

  # Return input and results
  list(
    key        = data.table::setkey(cohortDT[, .(cohortID, cohortGroupID)], cohortID),
    increments = growthIncrGroups,
    output     = cbm_vars
  )
}

# Helper function: read as data.table and check for required columns
readDataTable <- function(table, tableName = NULL, colRequired = NULL, copy = FALSE){

  if (is.null(table)) stop(c(tableName, "table")[[1]], " not found")

  if (!data.table::is.data.table(table)){
    table <- tryCatch(
      data.table::as.data.table(table),
      error = function(e) stop(
        c(tableName, "table")[[1]],
        " failed to be read as data.table: ", e$message,
        call. = FALSE))
  }

  if (!is.null(colRequired)){

    if (!all(colRequired %in% names(table))) stop(
      c(tableName, "table")[[1]], " missing column(s): ",
      paste(shQuote(setdiff(colRequired, names(table))), collapse = ", "))
  }

  if (copy) table <- data.table::copy(table)

  return(table)
}



