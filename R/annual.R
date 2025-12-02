
#' CBM EXN step
cbmExnStep <- function(cbm_vars,
                       parallel.cores = NULL, parallel.chunkSize = 100L, ...){

  mod$libcbm_default_model_config <- libcbmr::cbm_exn_get_default_parameters()
  step_ops <- libcbmr::cbm_exn_step_ops(cbm_vars, mod$libcbm_default_model_config)

  if (is.null(parallel.cores) || is.na(parallel.cores)){
    parallel.cores <- 1L
    rowGroups <- list(cbm_vars$pools$row_idx)
  }else{
    rowGroups <- split(cbm_vars$pools$row_idx, ceiling(1:nrow(cbm_vars$pools) / parallel.chunkSize))
  }

  cbm_vars_new <- parallel::mclapply(
    mc.cores = parallel.cores, mc.silent = TRUE, ...,
    rowGroups,
    function(row_idx_chunk){

      # Temporarily remove row_idx column
      cbm_vars_chunk <- lapply(cbm_vars[-1], function(tbl) tbl[row_idx %in% row_idx_chunk, -"row_idx"])

      # Call Python
      cbm_vars_chunk <- libcbmr::cbm_exn_step(
        cbm_vars_chunk,
        step_ops,
        libcbmr::cbm_exn_get_step_disturbance_ops_sequence(),
        libcbmr::cbm_exn_get_step_ops_sequence(),
        mod$libcbm_default_model_config
      )

      # Convert to data.table with row_idx
      for (i in 1:length(cbm_vars_chunk)){
        cbm_vars_chunk[[i]] <- data.table::data.table(
          row_idx = row_idx_chunk,
          cbm_vars_chunk[[i]],
          key = "row_idx")
      }

      # Return
      cbm_vars_chunk
    })

  if (length(cbm_vars_new) == 1){
    cbm_vars_new <- cbm_vars_new[[1]]

  }else{
    tblNames <- names(cbm_vars_new[[1]])
    cbm_vars_new <- lapply(tblNames, function(tblName){
      tbl <- data.table::rbindlist(lapply(cbm_vars_new, `[[`, tblName))
      data.table::setkey(tbl, row_idx)
      tbl
    })
    names(cbm_vars_new) <- tblNames
  }

  cbm_vars <- c(cbm_vars[1], cbm_vars_new)

  # Implement delay
  delayRows <- with(cbm_vars$state, is.na(time_since_last_disturbance) | time_since_last_disturbance <= delay)
  if (any(delayRows)) {
    cbm_vars$state$age[delayRows] <- 0
    delayGrowth <- c("age", "merch_inc", "foliage_inc", "other_inc")
    cbm_vars$parameters[delayRows, delayGrowth] <- 0
  }
  rm(delayRows)

  # Return
  cbm_vars
}



