
#' CBM EXN step
cbmExnStep <- function(cbm_vars,
                       parallel.cores = NULL, parallel.chunkSize = 100L, ...){

  # Temporarily remove key
  cohortKey <- cbm_vars$key
  cbm_vars$key <- NULL

  # Temporarily remove row_idx column
  row_idx <- cbm_vars$parameters$row_idx
  for (i in 1:length(cbm_vars)) cbm_vars[[i]][, row_idx := NULL]

  if (is.null(parallel.cores) || is.na(parallel.cores)){
    parallel.cores <- 1L
    rowGroups <- list(NA)
  }else{
    rowGroups <- split(row_idx, ceiling(1:length(row_idx) / parallel.chunkSize))
  }

  cbm_vars <- parallel::mclapply(
    mc.cores = parallel.cores, mc.silent = TRUE, ...,
    rowGroups,
    function(row_idx_chunk){

      if (length(row_idx_chunk) == 1 && is.na(row_idx_chunk)){
        cbm_vars_chunk <- cbm_vars
      }else{
        cbm_vars_chunk <- lapply(1:length(cbm_vars), function(i) cbm_vars[[i]][row_idx %in% row_idx_chunk,])
        names(cbm_vars_chunk) <- names(cbm_vars)
      }

      # Call Python
      reticulate::use_virtualenv("r-spadesCBM")

      mod$libcbm_default_model_config <- libcbmr::cbm_exn_get_default_parameters()
      step_ops <- libcbmr::cbm_exn_step_ops(cbm_vars, mod$libcbm_default_model_config)

      libcbmr::cbm_exn_step(
        cbm_vars_chunk,
        step_ops,
        libcbmr::cbm_exn_get_step_disturbance_ops_sequence(),
        libcbmr::cbm_exn_get_step_ops_sequence(),
        mod$libcbm_default_model_config
      )
    })

  # Convert to list of data.table with row_idx
  tblNames <- names(cbm_vars[[1]])
  cbm_vars <- lapply(tblNames, function(tblName){
    tbl <- data.table::rbindlist(lapply(cbm_vars, `[[`, tblName))
    tbl[, row_idx := row_idx]
    data.table::setkey(tbl, row_idx)
    data.table::setcolorder(tbl)
    tbl
  })
  names(cbm_vars) <- tblNames

  # Implement delay
  delayRows <- with(cbm_vars$state, is.na(time_since_last_disturbance) | time_since_last_disturbance <= delay)
  if (any(delayRows)) {
    cbm_vars$state$age[delayRows] <- 0
    delayGrowth <- c("age", "merch_inc", "foliage_inc", "other_inc")
    cbm_vars$parameters[delayRows, delayGrowth] <- 0
  }

  # Return
  c(list(key = cohortKey), cbm_vars)
}



