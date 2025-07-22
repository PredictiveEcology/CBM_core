
simSaveOutputs <- function(sim, year = time(sim), all = FALSE, yearly = FALSE, ...){

  tblList <- sim$cbm_vars

  for (tblName in names(tblList)[!sapply(tblList, is.null)]){

    tbl <- data.table::copy(tblList[[tblName]])
    tbl[, year := year]
    data.table::setcolorder(tbl, "year", before = 1)

    arrow::write_dataset(
      tbl, partitioning = "year",
      file.path(sim$cbmOutputsDB, tblName),
      ...)
  }

  return(invisible())
}

