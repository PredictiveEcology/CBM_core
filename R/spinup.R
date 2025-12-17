
#' CBM EXN spinup
cbmExnSpinup <- function(cohortDT, spuMeta, growthMeta, growthIncr,
                         colname_gc      = "gcids",
                         colname_species = "species",
                         colname_age     = "age",
                         colname_delay   = "delay",
                         default_delay   = 0L,
                         default_historical_disturbance_type = 1L,
                         default_last_pass_disturbance_type  = 1L,
                         parallel.cores = NULL, parallel.chunkSize = 500L, ...){

  ## Prepare input for spinup ----

  # Read input tables
  reqCols <- list(
    cohortDT   = c("cohortID", "spatial_unit_id", colname_gc, colname_age),
    spuMeta    = c("id", "return_interval", "min_rotations", "max_rotations", "mean_annual_temperature"),
    growthMeta = c(colname_gc, colname_species, "sw_hw"),
    growthIncr = c(colname_gc, "age", "merch_inc", "foliage_inc", "other_inc")
  )
  cohortDT   <- readDataTable(cohortDT,   "cohortDT",   colRequired = reqCols$cohortDT)
  spuMeta    <- readDataTable(spuMeta,    "spuMeta",    colRequired = reqCols$spuMeta)
  growthMeta <- readDataTable(growthMeta, "growthMeta", colRequired = reqCols$growthMeta)
  growthIncr <- readDataTable(growthIncr, "growthIncr", colRequired = reqCols$growthIncr)

  # Create cohort groups: groups of cohorts with the same attributes
  ## Allow all cohortDT attributes to be considered in unique groupings
  groupCols <- setdiff(names(cohortDT), c(
    "cohortID", "pixelIndex", "area",
    if (colname_species != "species" && "species" %in% names(cohortDT)) "species",
    if (colname_delay   != "delay"   && "delay"   %in% names(cohortDT)) "delay"
  ))
  cohortDT[, row_idx := .GRP, by = groupCols]
  on.exit(cohortDT[, row_idx := NULL])

  # Isolate unique groups and join with parameters
  cohortGroups <- unique(cohortDT[, .SD, .SDcols = c("row_idx", groupCols)])
  cohortGroups <- cohortGroups |>
    data.table::merge.data.table(spuMeta, by.x = "spatial_unit_id", by.y = "id",
                                 suffixes = c("", ".y"), all.x = TRUE) |>
    data.table::merge.data.table(growthMeta, by = colname_gc,
                                 suffixes = c("", ".y"), all.x = TRUE)
  cohortGroups[, which(grepl("\\.y$", names(cohortGroups))) := NULL]
  data.table::setkey(cohortGroups, row_idx)

  # Set area to 1ha
  cohortGroups[, area := 1L] # 1ha

  # Prepare sw_hw column for Python
  if (is.character(cohortGroups$sw_hw)) cohortGroups[, sw_hw := data.table::fifelse(sw_hw == "sw", 0L, 1L)]

  # Set column names for Python
  if (colname_species != "species"){
    data.table::setnames(
      cohortGroups, c(colname_species, "species"), c("species", "species_in"), skip_absent = TRUE)
  }
  if (colname_age != "age"){
    data.table::setnames(
      cohortGroups, c(colname_age, "age"), c("age", "age_in"), skip_absent = TRUE)
  }
  if (colname_delay != "delay" && colname_delay %in% names(cohortGroups)){
    data.table::setnames(cohortGroups, colname_delay, "delay")
  }

  # Set defaults
  if ("delay" %in% names(cohortGroups)){
    cohortGroups[is.na(delay), delay := default_delay]
  }else{
    cohortGroups[, delay := default_delay]
  }
  if ("historical_disturbance_type" %in% names(cohortGroups)){
    cohortGroups[is.na(historical_disturbance_type),
                 historical_disturbance_type := default_historical_disturbance_type]
  }else{
    cohortGroups[, historical_disturbance_type := default_historical_disturbance_type]
  }
  if ("last_pass_disturbance_type" %in% names(cohortGroups)){
    cohortGroups[is.na(last_pass_disturbance_type),
                 last_pass_disturbance_type := default_last_pass_disturbance_type]
  }else{
    cohortGroups[, last_pass_disturbance_type := default_last_pass_disturbance_type]
  }


  ## Spinup ----

  row_idx <- cohortGroups$row_idx
  if (is.null(parallel.cores) || is.na(parallel.cores)){
    parallel.cores <- 1L
    rowGroups <- list(row_idx)
  }else{
    rowGroups <- split(row_idx, ceiling(1:length(row_idx) / parallel.chunkSize))
  }

  cbm_vars <- parallel::mclapply(
    mc.cores = parallel.cores, mc.silent = TRUE, ...,
    rowGroups,
    function(row_idx_chunk){

      cgChunk <- cohortGroups
      if (length(row_idx_chunk) != length(row_idx)){
        cgChunk <- cgChunk[row_idx %in% row_idx_chunk,]
      }

      # Join growth increments with cohort group IDs
      ## Drop growth increments age <= 0
      growthIncrGroups <- data.table::merge.data.table(
        cgChunk[, .SD, .SDcols = c("row_idx", colname_gc)],
        subset(growthIncr, age > 0),
        by = colname_gc, allow.cartesian = TRUE)[, gcids := NULL]
      data.table::setkey(growthIncrGroups, row_idx, age)

      # Call Python
      reticulate::use_virtualenv("r-spadesCBM")

      mod$libcbm_default_model_config <- libcbmr::cbm_exn_get_default_parameters()
      spinup_op_seq <- libcbmr::cbm_exn_get_spinup_op_sequence()

      spinup_input <- list(
        parameters = cgChunk,
        increments = growthIncrGroups
      )

      spinup_ops <- libcbmr::cbm_exn_spinup_ops(
        spinup_input, mod$libcbm_default_model_config
      )

      libcbmr::cbm_exn_spinup(
        spinup_input,
        spinup_ops,
        spinup_op_seq,
        mod$libcbm_default_model_config
      )
    })

  # Convert to list of data.table with row_idx
  if (length(cbm_vars) == 1){
    cbm_vars <- cbm_vars[[1]]
  }else{
    tblNames <- names(cbm_vars[[1]])
    cbm_vars <- lapply(tblNames, function(tblName) data.table::rbindlist(lapply(cbm_vars, `[[`, tblName)))
    names(cbm_vars) <- tblNames
  }

  for (i in 1:length(cbm_vars)){
    if (!data.table::is.data.table(cbm_vars[[i]])) cbm_vars[[i]] <- data.table::as.data.table(cbm_vars[[i]])
    cbm_vars[[i]][, row_idx := row_idx]
    data.table::setkey(cbm_vars[[i]], row_idx)
    data.table::setcolorder(cbm_vars[[i]])
    cbm_vars[[i]]
  }

  # Add cohort group attributes to state table
  cohortGroups <- cohortGroups[, .SD, .SDcols = intersect(
    names(cohortGroups), c(
      #"age_in",
      setdiff(groupCols, names(cbm_vars$state)),
      "mean_annual_temperature"
    ))]
  cbm_vars$state <- cbind(cbm_vars$state, cohortGroups)

  # Return results
  cohortKey <- cohortDT[, .SD, .SDcols = intersect(
    c("cohortID", "pixelIndex", "row_idx"), names(cohortDT))]
  data.table::setkey(cohortKey, cohortID)

  c(list(key = cohortKey), cbm_vars)
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



