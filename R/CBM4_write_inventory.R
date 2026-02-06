
#' cbm4_format_inventory
#'
#' @param def_land_class character. Land class code.
#' Defined in CBM defaults database tables 'land_class' and 'land_class_tr'.
#' @param def_afforestation_pre_type character. TODO
#' Defined in CBM defaults database tables 'afforestation_pre_type'
#' @param def_historic_disturbance_type character. Historic disturbance type.
#' Defined in CBM defaults database tables 'disturbance_type' and 'disturbance_type_tr'.
#' @param def_last_pass_disturbance_type character. Last pass disturbance.
#' Defined in CBM defaults database tables 'disturbance_type' and 'disturbance_type_tr'.
#' @param def_delay integer. Regeneration delay.
#' @param def_cohort_proportion integer. A value between 0-1.
#' Percentage of the pixel's area that is attributed to the cohort.
cbm4_format_inventory <- function(
    cohortDT, standDT,
    cbm_defaults_db                = NULL,
    def_land_class                 = "UNFCCC_FL_R_FL", # "Forest Land remaining Forest Land"
    def_afforestation_pre_type     = "None",
    def_historic_disturbance_type  = "Wildfire",
    def_last_pass_disturbance_type = "Wildfire",
    def_delay                      = 0L,
    def_cohort_proportion          = 1L
  ){

  # Check table columns
  check_table_columns("standDT",  standDT,  c("pixelIndex", "raster_index", "chunk_index", "area", "admin_boundary", "eco_boundary_id"))
  check_table_columns("cohortDT", cohortDT, c("cohortID", "pixelIndex", "age"))

  # Summarize inventory
  dataFull <- merge(cohortDT, standDT, by = "pixelIndex", all.x = TRUE)[, .SD, .SDcols = unique(c(
    "cohortID",
    intersect(
      c("pixelIndex", "raster_index", "chunk_index", "area", "admin_boundary", "eco_boundary_id", "eco_boundary", "spatial_unit_id"),
      names(standDT)),
    names(cohortDT)
  ))]

  # Set column names
  if ("spatial_unit_id" %in% names(dataFull)) data.table::setnames(dataFull, "spatial_unit_id", "spatial_unit")
  if ("delay_spinup"    %in% names(dataFull)) data.table::setnames(dataFull, "delay_spinup",    "delay")
  if ("age_spinup"      %in% names(dataFull)) dataFull[, age := age_spinup]

  # Set cohort index
  if (any(duplicated(dataFull$pixelIndex))) stop("Multiple cohorts per pixel not yet supported")
  dataFull[, cohort_index := 0]

  # Set index
  dataFull[, index := .GRP - 1, by = setdiff(names(dataFull), c("cohortID", "pixelIndex", "raster_index", "area"))]

  # Set area
  dataFull[, area := sum(area), by = index]

  # Split by raster key and unique groups
  ## Keep cohortIndex and pixelIndex for now
  dataIndex <- dataFull[, .(index, raster_index, cohort_index, chunk_index, cohortID, pixelIndex)]
  data.table::setkey(dataIndex, index, raster_index, cohort_index, chunk_index)

  dataFull <- unique(dataFull[, .SD, .SDcols = setdiff(names(dataFull), c("cohortID", "pixelIndex", "raster_index"))])
  data.table::setkey(dataFull, index, cohort_index, chunk_index)
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

  # Set eco_boundary
  if (!"eco_boundary" %in% names(dataFull)){

    if (!"eco_boundary_id" %in% names(dataFull)) stop("cohortDT requires column 'eco_boundary_id' or 'eco_boundary'")

    if (is.null(cbm_defaults_db)) stop("cbm_defaults_db required to set eco_boundary")
    cbmDBcon <- DBI::dbConnect(RSQLite::dbDriver("SQLite"), cbm_defaults_db)
    eco_boundary_tr <- subset(DBI::dbReadTable(cbmDBcon, "eco_boundary_tr"), locale_id == 1)
    RSQLite::dbDisconnect(cbmDBcon)

    dataFull[, eco_boundary := factor(
      eco_boundary_tr$name[match(eco_boundary_id, eco_boundary_tr$id)],
      levels = eco_boundary_tr$name)]
  }

  # Set spatial_unit
  if (!"spatial_unit" %in% names(dataFull)){

    if (is.null(cbm_defaults_db)) stop("cbm_defaults_db required to set spatial_unit")
    cbmDBcon <- DBI::dbConnect(RSQLite::dbDriver("SQLite"), cbm_defaults_db)
    spatial_unit <- merge(
      DBI::dbReadTable(cbmDBcon, "spatial_unit"),
      subset(DBI::dbReadTable(cbmDBcon, "admin_boundary_tr"), locale_id == 1),
      by.x = "admin_boundary_id", by.y = "id", all.x = TRUE)
    RSQLite::dbDisconnect(cbmDBcon)

    dataFull <- data.table::merge.data.table(
      dataFull,
      data.table::as.data.table(spatial_unit)[, .(admin_boundary = name, eco_boundary_id, spatial_unit = id)],
      by = c("admin_boundary", "eco_boundary_id"), all.x = TRUE, sort = FALSE)
    data.table::setkey(dataFull, index, cohort_index, chunk_index)
    data.table::setcolorder(dataFull)

    # Check spatial unit IDs
    if (any(is.na(dataFull$spatial_unit_id))){
      noMatch <- unique(dataFull[is.na(spatial_unit_id), .(admin_boundary, eco_boundary_id)])
      data.table::setkey(noMatch, admin_boundary, eco_boundary_id)
      if (nrow(noMatch) > 0) stop(
        "spatial_unit_id not found for: ",
        paste(paste(noMatch$admin_boundary, "ecozone", noMatch$eco_boundary_id), collapse = "; "))
    }
  }

  # Return
  list(
    index = dataIndex,
    flat  = dataFull
  )
}

#' cbm4_write_inventory
cbm4_write_inventory <- function(
    cbm4_data = NULL, cbm_defaults_db,
    masterRaster, classifiers,
    cohortDT, standDT, ...,
    inventory_dataset = file.path(cbm4_data, "inventory")
){

  # Initiate dataset
  cbm4_write_inventory_initiate(
    inventory_dataset = inventory_dataset,
    cbm_defaults_db   = cbm_defaults_db,
    masterRaster      = masterRaster,
    classifiers       = classifiers
  )

  # Format inventory
  inv <- cbm4_format_inventory(
    cohortDT = cohortDT,
    standDT  = standDT,
    cbm_defaults_db = cbm_defaults_db,
    ...
  ) |> Cache()

  # Split cohort key and index
  inv$key <- data.table::copy(inv$index)
  inv$index[, pixelIndex := NULL]
  inv$index[, cohortID   := NULL]

  # Group inventory
  inv$flat  <- dplyr::group_by(inv$flat,  cohort_index, chunk_index)
  inv$index <- dplyr::group_by(inv$index, cohort_index, chunk_index)

  # Write inventory
  arrow::write_dataset(inv$flat,  file.path(inventory_dataset, "inventory"))
  arrow::write_dataset(inv$index, file.path(inventory_dataset, "inventory-raster_index"))

  # Return cohort key
  inv$key
}

cbm4_write_inventory_initiate <- function(
    cbm4_data = NULL,
    cbm_defaults_db,
    masterRaster, classifiers,
    inventory_dataset = file.path(cbm4_data, "inventory")
  ){

  if (length(classifiers) == 0) stop(">=1 'classifiers' are required.")

  arrow_space <- reticulate::import("arrow_space")
  pd <- reticulate::import("pandas")

  # Initiate dataset
  arrow_space$dataset_common$create_empty(

    out_dataset_name = "inventory",
    out_storage_type = "local_storage",
    out_storage_path_or_uri = inventory_dataset,

    partitions = list("cohort_index" = "int64", "chunk_index" = "int64"),

    chunks = list(
      arrow_space$geospatial$raster_bound$RasterBound(
        0, 0, terra::ncol(masterRaster), terra::nrow(masterRaster)
      )
    ),

    geo_metadata = arrow_space$dataset_metadata$GeoMetadata(
      nrows           = terra::nrow(masterRaster),
      ncols           = terra::ncol(masterRaster),
      projection      = terra::crs(masterRaster),
      geo_transform_0 = terra::xmin(masterRaster),
      geo_transform_1 = terra::res(masterRaster)[[1]],
      geo_transform_2 = 0,
      geo_transform_3 = terra::ymax(masterRaster),
      geo_transform_4 = 0,
      geo_transform_5 = -terra::res(masterRaster)[[2]]
    ),

    tags = if (!is.null(classifiers)) pd$DataFrame(
      columns = c("layer_name", "tag"),
      data = reticulate::dict(
        layer_name = as.list(classifiers),
        tag        = as.list(rep("classifier", length(classifiers)))
      )
    )
  )

  # Write CBM defaults database
  arrow_space$raster_indexed_dataset$RasterIndexedDataset(
    dataset_name = "inventory",
    storage_type = "local_storage",
    storage_path_or_uri = inventory_dataset
  )$write_file_or_dir("cbm_defaults", cbm_defaults_db)
}


