
#' CBM-EXN Step
cbmEXN_step <- function(cbm_vars, cbm_defaults_db = NULL, cbm_exn_dir = NULL){

  # Set resource paths
  withr::local_options(list(
    libcbmr.cbm_defaults_path      = cbm_defaults_db,
    libcbmr.cbm_exn_parameters_dir = cbm_exn_dir
  ))

  # Set mean_annual_temperature
  if (!"mean_annual_temperature" %in% names(cbm_vars$parameters)){

    cbmDBcon <- RSQLite::dbConnect(RSQLite::dbDriver("SQLite"), libcbmr::get_cbm_defaults_path())
    meanTemp <- RSQLite::dbReadTable(cbmDBcon, "spatial_unit")[, c("id", "mean_annual_temperature")]
    RSQLite::dbDisconnect(cbmDBcon)

    cbm_vars$parameters[, mean_annual_temperature := meanTemp$mean_annual_temperature[match(
      cbm_vars$state$spatial_unit_id, meanTemp$id
    )]]
  }

  # Temporarily remove row_idx column
  row_idx <- cbm_vars$parameters$row_idx
  for (i in 2:length(cbm_vars)) cbm_vars[[i]][, row_idx := NULL]

  # Call Python
  mod$libcbm_default_model_config <- libcbmr::cbm_exn_get_default_parameters()
  step_ops <- libcbmr::cbm_exn_step_ops(cbm_vars, mod$libcbm_default_model_config)

  cbm_vars[-1] <- libcbmr::cbm_exn_step(
    cbm_vars[-1],
    step_ops,
    libcbmr::cbm_exn_get_step_disturbance_ops_sequence(),
    libcbmr::cbm_exn_get_step_ops_sequence(),
    mod$libcbm_default_model_config
  )

  # Implement delay
  delayRows <- with(cbm_vars$state, is.na(time_since_last_disturbance) | time_since_last_disturbance <= delay)
  if (any(delayRows)) {
    cbm_vars$state$age[delayRows] <- 0
    delayGrowth <- c("age", "merch_inc", "foliage_inc", "other_inc")
    cbm_vars$parameters[delayRows, delayGrowth] <- 0
  }

  # Prepare output data for next annual event
  for (i in 2:length(cbm_vars)){
    cbm_vars[[i]] <- data.table::data.table(row_idx = row_idx, cbm_vars[[i]], key = "row_idx")
  }

  return(cbm_vars)
}



