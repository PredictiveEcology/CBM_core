
#' cbm4_format_increments
cbm4_format_increments <- function(classifiers, gcMeta, gcIncr, cbm_defaults_db = NULL, long = TRUE){

  # Get key
  gcKey <- data.table::key(gcMeta)
  if (length(gcKey) > 1) stop("gcMeta requires a singular key column")
  if (is.null(gcKey)){
    if ("eventID" %in% names(gcMeta) & "eventID" %in% names(gcIncr)){
      gcKey <- "eventID"
    }else stop("gcMeta requires a key column")
  }

  # Check table columns
  check_table_columns("gcMeta", gcMeta, c(gcKey, classifiers, "sw_hw"))
  check_table_columns("gcIncr", gcIncr, c(gcKey, "age", "merch_inc", "foliage_inc", "other_inc"))

  # Check for data gaps
  if (any(do.call(c, lapply(split(gcIncr$age, gcIncr$gcids), diff)) > 1)) stop("gcIncr must have increments for every year")

  # Set spatial_unit
  if (!"spatial_unit_id" %in% names(gcMeta)){

    check_table_columns("gcMeta", gcMeta, c("admin_boundary", "eco_boundary_id"))

    if (is.null(cbm_defaults_db)) stop("cbm_defaults_db required to set spatial_unit")
    cbmDBcon <- DBI::dbConnect(RSQLite::dbDriver("SQLite"), cbm_defaults_db)
    spatial_unit <- merge(
      DBI::dbReadTable(cbmDBcon, "spatial_unit"),
      subset(DBI::dbReadTable(cbmDBcon, "admin_boundary_tr"), locale_id == 1),
      by.x = "admin_boundary_id", by.y = "id", all.x = TRUE)
    RSQLite::dbDisconnect(cbmDBcon)

    gcMeta <- data.table::merge.data.table(
      gcMeta,
      data.table::as.data.table(spatial_unit)[, .(admin_boundary = name, eco_boundary_id, spatial_unit_id = id)],
      by = c("admin_boundary", "eco_boundary_id"), all.x = TRUE, sort = FALSE)

    # Check spatial unit IDs
    if (any(is.na(gcMeta$spatial_unit_id))){
      noMatch <- unique(gcMeta[is.na(spatial_unit_id), .(admin_boundary, eco_boundary_id)])
      data.table::setkey(noMatch, admin_boundary, eco_boundary_id)
      if (nrow(noMatch) > 0) stop(
        "spatial_unit_id not found for: ",
        paste(paste(noMatch$admin_boundary, "ecozone", noMatch$eco_boundary_id), collapse = "; "))
    }
  }

  # Format increments
  gcIncr <- data.table::copy(data.table::as.data.table(gcIncr))
  data.table::setnames(gcIncr, "age", "state.age")
  incrSW <- gcMeta$sw_hw[match(gcIncr$gcids, gcMeta$gcids)] == "sw"

  gcIncr[ incrSW, increment.SoftwoodMerch   := merch_inc]
  gcIncr[ incrSW, increment.SoftwoodFoliage := foliage_inc]
  gcIncr[ incrSW, increment.SoftwoodOther   := other_inc]
  gcIncr[!incrSW, increment.SoftwoodMerch   := 0]
  gcIncr[!incrSW, increment.SoftwoodFoliage := 0]
  gcIncr[!incrSW, increment.SoftwoodOther   := 0]

  gcIncr[!incrSW, increment.HardwoodMerch   := merch_inc]
  gcIncr[!incrSW, increment.HardwoodFoliage := foliage_inc]
  gcIncr[!incrSW, increment.HardwoodOther   := other_inc]
  gcIncr[ incrSW, increment.HardwoodMerch   := 0]
  gcIncr[ incrSW, increment.HardwoodFoliage := 0]
  gcIncr[ incrSW, increment.HardwoodOther   := 0]

  gcIncr[, merch_inc   := NULL]
  gcIncr[, foliage_inc := NULL]
  gcIncr[, other_inc   := NULL]
  rm(incrSW)

  # Format metadata
  gcMeta <- gcMeta[, .SD, .SDcols = c(gcKey, classifiers, "spatial_unit_id")]
  data.table::setnames(gcMeta, classifiers, paste0("classifiers.", classifiers))
  data.table::setnames(gcMeta, "spatial_unit_id", "inventory.spatial_unit")

  if (long){

    gcIncr <- merge(gcMeta, gcIncr, by = gcKey)
    gcIncr[, eval(gcKey) := NULL]

    # Add row for increments above greatest age
    gcIncrWC <- gcIncr[state.age == max(state.age), .SD, by = c(paste0("classifiers.", classifiers), "inventory.spatial_unit")]
    gcIncrWC[["state.age"]] <- "?"
    gcIncr <- rbind(gcIncr, gcIncrWC)
    data.table::setkeyv(gcIncr, c(paste0("classifiers.", classifiers), "inventory.spatial_unit"))

  }else{

    # Format increments wide
    incCols <- paste0(
      "increment.", c(
        paste0("Softwood", c("Merch", "Foliage", "Other")),
        paste0("Hardwood", c("Merch", "Foliage", "Other"))
      ))
    gcIncr <- data.table::mergelist(
      lapply(incCols, function(incCol){
        incWide <- data.table::dcast(gcIncr, get(gcKey) ~ state.age, value.var = incCol)
        data.table::setnames(incWide, "gcKey", gcKey)
        data.table::setnames(incWide, names(incWide)[-1], paste0(incCol, ".", names(incWide)[-1]))
        incWide
      }),
      on = gcKey, how = "left")

    gcIncr <- merge(gcMeta, gcIncr, by = gcKey)
    gcIncr[, eval(gcKey) := NULL]
  }

  return(gcIncr)
}

#' cbm4_write_spinup_parameters
cbm4_write_spinup_parameters <- function(
    cbm4_data = NULL, cbm_defaults_db,
    classifiers, gcMeta, gcIncr,
    template_dataset_name     = "inventory",
    template_dataset          = file.path(cbm4_data, template_dataset_name),
    spinup_parameters_dataset = file.path(cbm4_data, "spinup_parameters")
){

  # Initiate dataset
  cbm4_new_dataset_from_template(
    out_dataset_name      = "spinup_parameters",
    out_dataset           = spinup_parameters_dataset,
    template_dataset_name = template_dataset_name,
    template_dataset      = template_dataset,
    partitions            = list("chunk_index" = "int64")
  )

  # Format increments
  incWide <- cbm4_format_increments(
    classifiers     = classifiers,
    gcMeta          = gcMeta,
    gcIncr          = gcIncr,
    cbm_defaults_db = cbm_defaults_db,
    long            = FALSE
  ) |> Cache()

  # Format parameters_ecological
  cbmParams <- cbm4_format_parameters_ecological(cbm_defaults_db) |> Cache()

  # Write parameters
  arrow::write_dataset(cbmParams, file.path(spinup_parameters_dataset, "spinup_parameters-table-parameters_ecological"))
  arrow::write_dataset(incWide,   file.path(spinup_parameters_dataset, "spinup_parameters-table-parameters_increments_wide"))

  # Write parameter_table_metadata
  paramTables  <- data.table::data.table(table_name = c("parameters_ecological", "parameters_increments_wide"))
  paramTablesPath <- file.path(spinup_parameters_dataset, "spinup_parameters-table-parameter_table_metadata")
  arrow::write_dataset(paramTables, paramTablesPath)
}

#' cbm4_write_step_parameters
cbm4_write_step_parameters <- function(
    cbm4_data = NULL, cbm_defaults_db,
    classifiers, gcMeta, gcIncr,
    template_dataset_name   = "simulation",
    template_dataset        = file.path(cbm4_data, template_dataset_name),
    step_parameters_dataset = file.path(cbm4_data, "step_parameters")
){

  # Initiate dataset
  cbm4_new_dataset_from_template(
    out_dataset_name      = "step_parameters",
    out_dataset           = step_parameters_dataset,
    template_dataset_name = template_dataset_name,
    template_dataset      = template_dataset,
    partitions            = list("chunk_index" = "int64")
  )

  # Format increments
  incLong <- cbm4_format_increments(
    classifiers     = classifiers,
    gcMeta          = gcMeta,
    gcIncr          = gcIncr,
    cbm_defaults_db = cbm_defaults_db,
    long            = TRUE
  ) |> Cache()

  # Format parameters_ecological
  cbmParams <- cbm4_format_parameters_ecological(cbm_defaults_db) |> Cache()

  # Write parameters
  arrow::write_dataset(cbmParams, file.path(step_parameters_dataset, "step_parameters-table-parameters_ecological"))
  arrow::write_dataset(incLong,   file.path(step_parameters_dataset, "step_parameters-table-parameters_increments"))

  # Write parameter_table_metadata
  paramTables  <- data.table::data.table(table_name = c("parameters_ecological", "parameters_increments"))
  paramTablesPath <- file.path(step_parameters_dataset, "step_parameters-table-parameter_table_metadata")
  arrow::write_dataset(paramTables, paramTablesPath)
}

