
#' CBM4 Spinup
#'
#' @param cbm4_data character. Path to CBM4 spatial parquet dataset directory.
#' @param cbm_defaults_db character. Path to CBM defaults SQLite database.
#' @param max_workers integer. Number of parallel processes to use.
#' NULL or NA will bypass this and and cap processing at the system's available resources.
#' A positive integer will spawn multiple processes assigned to each `chunk_index`.
cbm4_spinup <- function(
    cbm4_data,
    cbm_defaults_db,
    max_workers = NULL,
    spinup_parameters_dataset = file.path(cbm4_data, "spinup_parameters"),
    inventory_dataset         = file.path(cbm4_data, "inventory"),
    simulation_dataset        = file.path(cbm4_data, "simulation")
  ){

  spatial_cbm4_app <- reticulate::import("cbm4.app.spatial.spatial_cbm4.spatial_cbm4_app")

  cbm4_datasets <- list(
    spinup_parameters = list(
      dataset_name = "spinup_parameters",
      storage_type = "local_storage",
      path_or_uri  = spinup_parameters_dataset
    ),
    inventory = list(
      dataset_name = "inventory",
      storage_type = "local_storage",
      path_or_uri  = inventory_dataset
    ),
    simulation = list(
      dataset_name = "simulation",
      storage_type = "local_storage",
      path_or_uri  = simulation_dataset
    )
  )

  for (dataset in cbm4_datasets){
    if (length(dataset$path_or_uri) == 0) stop(
      "CBM4 ", shQuote(dataset$dataset_name), " dataset path invalid")
    if (dataset$dataset_name != "simulation" && !file.exists(dataset$path_or_uri)) stop(
      "CBM4 ", shQuote(dataset$dataset_name), " dataset not found: ", dataset$path_or_uri)
  }

  cbmspec_config <- reticulate::dict(
    "package_name"       = "cbmspec_cbm3",
    "factory_function"   = "cbmspec_cbm3.models.cbmspec_cbm3_single_matrix.model_create",
    "factory_parameters" = reticulate::dict(
      "cbm_defaults_path" = cbm_defaults_db
    ))

  spatial_cbm4_app$create_simulation_dataset(reticulate::dict(
    "cbmspec_model_config"   = cbmspec_config,
    "inventory_dataset"      = cbm4_datasets$inventory,
    "out_simulation_dataset" = cbm4_datasets$simulation
  ))

  spinupIn <- list(
    "cbmspec_model_config" = cbmspec_config,
    "parameter_dataset"    = cbm4_datasets$spinup_parameters,
    "inventory_dataset"    = cbm4_datasets$inventory,
    "simulation_dataset"   = cbm4_datasets$simulation,
    "max_workers"          = max_workers
  )
  if (is.null(max_workers) || is.na(max_workers)) spinupIn[["max_workers"]] <- NULL

  spatial_cbm4_app$spinup_all(reticulate::dict(spinupIn))
}


#' CBM4 Step
#'
#' @param cbm4_data character. Path to CBM4 spatial parquet dataset directory.
#' @param cbm_defaults_db character. Path to CBM defaults SQLite database.
#' @param timestep integer. Simulation timestep with 1 representing the first year.
#' @param area_unit_conversion numeric. Conversion factor of area to hectares (ha).
#' @param write_parameters logical. TODO
#' @param max_workers integer. Number of parallel processes to use.
#' NULL or NA will bypass this and and cap processing at the system's available resources.
#' A positive integer will spawn multiple processes assigned to each `chunk_index`.
cbm4_step <- function(
    cbm4_data,
    cbm_defaults_db,
    timestep,
    area_unit_conversion    = 0.0001,
    write_parameters        = TRUE,
    max_workers             = NULL,
    step_parameters_dataset = file.path(cbm4_data, "step_parameters"),
    inventory_dataset       = file.path(cbm4_data, "inventory"),
    disturbance_dataset     = file.path(cbm4_data, "disturbance"),
    simulation_dataset      = file.path(cbm4_data, "simulation")
  ){

  spatial_cbm4_app <- reticulate::import("cbm4.app.spatial.spatial_cbm4.spatial_cbm4_app")

  cbm4_datasets <- list(
    step_parameters = list(
      dataset_name = "step_parameters",
      storage_type = "local_storage",
      path_or_uri  = step_parameters_dataset
    ),
    inventory = list(
      dataset_name = "inventory",
      storage_type = "local_storage",
      path_or_uri  = inventory_dataset
    ),
    disturbance = list(
      dataset_name = "disturbance",
      storage_type = "local_storage",
      path_or_uri  = disturbance_dataset
    ),
    simulation = list(
      dataset_name = "simulation",
      storage_type = "local_storage",
      path_or_uri  = simulation_dataset
    )
  )

  for (dataset in cbm4_datasets){
    if (length(dataset$path_or_uri) == 0) stop(
      "CBM4 ", shQuote(dataset$dataset_name), " dataset path invalid")
    if (dataset$dataset_name != "inventory" && !file.exists(dataset$path_or_uri)) stop(
      "CBM4 ", shQuote(dataset$dataset_name), " dataset not found: ", dataset$path_or_uri)
  }

  cbmspec_config <- reticulate::dict(
    "package_name"       = "cbmspec_cbm3",
    "factory_function"   = "cbmspec_cbm3.models.cbmspec_cbm3_single_matrix.model_create",
    "factory_parameters" = reticulate::dict(
      "cbm_defaults_path" = cbm_defaults_db
    ))

  stepIn <- list(
    "cbmspec_model_config" = cbmspec_config,
    "timestep"             = timestep,
    "parameter_dataset"    = cbm4_datasets$step_parameters,
    "inventory_dataset"    = cbm4_datasets$inventory,
    "disturbance_dataset"  = cbm4_datasets$disturbance,
    "simulation_dataset"   = cbm4_datasets$simulation,
    "area_unit_conversion" = area_unit_conversion,
    "write_parameters"     = write_parameters,
    "max_workers"          = max_workers
  )
  if (is.null(max_workers) || is.na(max_workers)) stepIn[["max_workers"]] <- NULL

  spatial_cbm4_app$step_all(reticulate::dict(stepIn))
}

