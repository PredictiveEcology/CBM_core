
#' cbm4_format_disturbance
#'
#' @param def_proportion integer. TODO
#' @param def_enable_merge integer. TODO
#' @param def_sort_id integer. TODO
#' @param def_filter_id integer. TODO
#' @param def_undisturbed_transition_id integer. Set to 0 to indicate no transitions.
#' @param def_disturbed_transition_id integer. Set to 0 to indicate no transitions.
cbm4_format_disturbance <- function(
    standDT, distMeta, distEvents, classifiers = NULL,
    cbm_defaults_db               = NULL,
    def_proportion                = 1L,
    def_enable_merge              = 0L,
    def_sort_id                   = 0L,
    def_filter_id                 = 0L,
    def_undisturbed_transition_id = 0L,
    def_disturbed_transition_id   = 0L
){

  if (!is.null(classifiers)) stop("Disturbances do not yet support classifiers.")

  # Get key
  distKey <- data.table::key(distMeta)
  if (length(distKey) > 1) stop("distMeta requires a singular key column")
  if (is.null(distKey)){
    if ("eventID" %in% names(distMeta) & "eventID" %in% names(distEvents)){
      distKey <- "eventID"
    }else stop("distMeta requires a key column")
  }

  # Check table columns
  check_table_columns("standDT",    standDT,    c("pixelIndex", "raster_index", "chunk_index"))
  check_table_columns("distMeta",   distMeta,   c(distKey, "disturbance_type"))
  check_table_columns("distEvents", distEvents, c("pixelIndex", distKey, "timestep"))

  if (!any(c("disturbance_type", "disturbance_type_id") %in% names(distMeta))) stop(
    "distMeta requires one of columns 'disturbance_type' or 'disturbance_type_id'.")

  # Choose disturbance events by priority
  multiEvents <- distEvents[, .(N = .N, eventID = list(eventID)), by = c("pixelIndex", "year")][N > 1,]
  if (nrow(multiEvents) > 0){

    if (!"priority" %in% names(distMeta)) stop(
      "Multiple disturbance events found in one or more pixels. ",
      "Use the distMeta \"priority\" column to set event precendence.")

    multiEvents <- multiEvents[, .(eventID = unlist(eventID)), by = c("pixelIndex", "year")]
    multiEvents <- merge(multiEvents, distMeta, by = distKey, all.x = TRUE)

    multiEvents[, pri_highest := priority %in% min(priority), by = c("pixelIndex", "year")]
    multiEvents <- multiEvents[pri_highest == TRUE, .(N = .N, eventID = first(eventID)), by = c("pixelIndex", "year")]

    if (any(multiEvents$N > 1)) stop(
      "Multiple disturbance events found in one or more pixels ",
      "and distMeta \"priority\" indicates events have the same priority.")

    distEvents <- rbind(
      distEvents[!multiEvents, on = c("pixelIndex", "year")],
      distEvents[multiEvents,  on = c("pixelIndex", "year", "eventID")][, .SD, .SDcols = names(distEvents)]
    )
  }

  # Summarize disturbances
  dataFull <- merge(distEvents, standDT[, .(pixelIndex, raster_index, chunk_index)], by = "pixelIndex")
  dataFull <- merge(dataFull, distMeta, by = distKey)

  # Set disturbance_type
  if (!"disturbance_type" %in% names(dataFull)){

    if (is.null(cbm_defaults_db)) stop("cbm_defaults_db required to set disturbance_type")
    cbmDBcon <- DBI::dbConnect(RSQLite::dbDriver("SQLite"), cbm_defaults_db)
    disturbance_type_tr <- subset(DBI::dbReadTable(cbmDBcon, "disturbance_type_tr"), locale_id == 1)
    RSQLite::dbDisconnect(cbmDBcon)

    dataFull[, disturbance_type := disturbance_type_tr$disturbance_type[
      match(disturbance_type_id, disturbance_type_tr$disturbance_type_id)]]
  }

  # Set disturbance_type_id
  if (!"disturbance_type_id" %in% names(dataFull)){

    if (is.null(cbm_defaults_db)) stop("cbm_defaults_db required to set disturbance_type_id")
    cbmDBcon <- DBI::dbConnect(RSQLite::dbDriver("SQLite"), cbm_defaults_db)
    disturbance_type_tr <- subset(DBI::dbReadTable(cbmDBcon, "disturbance_type_tr"), locale_id == 1)
    RSQLite::dbDisconnect(cbmDBcon)

    dataFull[, disturbance_type_id := disturbance_type_tr$disturbance_type_id[
      match(disturbance_type, disturbance_type_tr$name)]]
  }

  # Set column names
  data.table::setnames(dataFull, distKey, "disturbance_id")
  data.table::setnames(dataFull, "disturbance_type_id", "default_disturbance_type_id")

  # Set index
  dataFull[, index := .GRP - 1, by = c("disturbance_id", "timestep", "chunk_index")]

  # Set disturbance_order
  ## This sets no order to the disturbances
  if (!"disturbance_order" %in% names(dataFull)) dataFull[, disturbance_order := 0]

  # Split by raster key and unique groups
  dataFull[, pixelIndex := NULL]
  dataIndex <- dataFull[, .(index, raster_index, disturbance_order, timestep, chunk_index)]
  data.table::setkey(dataIndex, index, raster_index, disturbance_order, timestep, chunk_index)

  dataFull <- unique(dataFull[, .SD, .SDcols = setdiff(names(dataFull), "raster_index")])
  data.table::setkey(dataFull, index, disturbance_order, timestep, chunk_index)
  data.table::setcolorder(dataFull)

  # Set defaults
  for (defArg in names(environment())[grepl("^def\\_", names(environment()))]){
    defCol <- sub("^def\\_", "", defArg)
    if (!defCol %in% names(dataFull)){
      dataFull[, eval(defCol) := get(defArg)]
    }else{
      data.table::setnafill(dataFull, cols = defCol, fill = get(defArg))
    }
  }

  # Return
  list(
    index = dataIndex,
    flat  = dataFull
  )
}

#' cbm4_write_disturbance
cbm4_write_disturbance <- function(
    cbm4_data = NULL, cbm_defaults_db = NULL,
    standDT = NULL, distMeta = NULL, distEvents = NULL, classifiers = NULL,
    template_dataset_name = "inventory",
    template_dataset      = file.path(cbm4_data, template_dataset_name),
    disturbance_dataset   = file.path(cbm4_data, "disturbance")
){

  # Initiate dataset
  if (!tryCatch(file.exists(disturbance_dataset), error = function(e) FALSE)){
    cbm4_new_dataset_from_template(
      out_dataset_name      = "disturbance",
      out_dataset           = disturbance_dataset,
      template_dataset_name = template_dataset_name,
      template_dataset      = template_dataset,
      partitions            = list("disturbance_order" = "int64", "timestep" = "int64", "chunk_index" = "int64")
    )
  }

  if (!is.null(distEvents) && nrow(distEvents) > 0){

    # Format disturbances
    dist <- cbm4_format_disturbance(
      cbm_defaults_db = cbm_defaults_db,
      standDT         = standDT,
      distMeta        = distMeta,
      distEvents      = distEvents,
      classifiers     = classifiers
    ) |> Cache()

    # Group disturbances
    dist$flat  <- dplyr::group_by(dist$flat,  disturbance_order, timestep, chunk_index)
    dist$index <- dplyr::group_by(dist$index, disturbance_order, timestep, chunk_index)

    # Write disturbances
    arrow::write_dataset(dist$flat,  file.path(disturbance_dataset, "disturbance"))
    arrow::write_dataset(dist$index, file.path(disturbance_dataset, "disturbance-raster_index"))
  }
}


