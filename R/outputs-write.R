
simSaveOutputs <- function(sim, year = time(sim), all = FALSE, yearly = FALSE, ...){

  tblList <- list(
    cohortGroupKeep = sim$cohortGroupKeep,
    cohortGroups    = sim$cohortGroups,
    state           = sim$cbm_vars$state,
    flux            = sim$cbm_vars$flux,
    pools           = sim$cbm_vars$pools
  )

  for (tblName in names(tblList)[!sapply(tblList, is.null)]){

    tbl <- data.table::copy(tblList[[tblName]])
    tbl[, year := year]
    data.table::setcolorder(tbl, "year", before = 1)
    data.table::setnames(tbl, "row_idx", "cohortGroupID", skip_absent = TRUE)

    arrow::write_dataset(
      tbl, partitioning = "year",
      file.path(sim$cbmOutputsDB, tblName),
      ...)
  }

  return(invisible())
}

