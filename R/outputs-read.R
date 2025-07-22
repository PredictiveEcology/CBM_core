
# Cohort group NPP
simCohortGroupNPP <- function(sim){
  cbmOutputsDB_cohortGroupNPP(sim$cbmOutputsDB) |>
    Cache(.cacheExtra = file.info(sim$cbmOutputsDB)$mtime)
}

cbmOutputsDB_cohortGroupNPP <- function(cbmOutputsDB){

  pqTbl <- dplyr::left_join(

    arrow::open_dataset(file.path(cbmOutputsDB, "key")) |>
      dplyr::filter(year != 0) |>
      dplyr::select(year, row_idx) |>
      dplyr::collect() |>
      dplyr::group_by(year, row_idx) |>
      dplyr::summarize(N = dplyr::n()),

    arrow::open_dataset(file.path(cbmOutputsDB, "flux")) |>
      dplyr::filter(year != 0) |>
      dplyr::group_by(year, row_idx) |>
      dplyr::collect() |>
      dplyr::summarize(NPP = sum(
        DeltaBiomass_AG, DeltaBiomass_BG,
        TurnoverMerchLitterInput, TurnoverFolLitterInput,
        TurnoverOthLitterInput, TurnoverCoarseLitterInput, TurnoverFineLitterInput)),

    by = c("year", "row_idx"))

  pqTbl <- dplyr::rename(pqTbl, simYear = year, cohortGroupID = row_idx)
  pqTbl <- data.table::as.data.table(pqTbl, key = c("simYear", "cohortGroupID"))
  pqTbl

}

# Cohort group pools
simCohortGroupPools <- function(sim){
  cbmOutputsDB_cohortGroupPools(sim$cbmOutputsDB, poolsCols = sim$pooldef) |>
    Cache(.cacheExtra = file.info(sim$cbmOutputsDB)$mtime)
}

cbmOutputsDB_cohortGroupPools <- function(cbmOutputsDB, poolsCols = NULL){

  pqTbl <- dplyr::left_join(

    arrow::open_dataset(file.path(cbmOutputsDB, "key")) |>
      dplyr::filter(year != 0) |>
      dplyr::select(year, row_idx) |>
      dplyr::collect() |>
      dplyr::group_by(year, row_idx) |>
      dplyr::summarize(N = dplyr::n()),

    dplyr::left_join(

      arrow::open_dataset(file.path(cbmOutputsDB, "state")) |>
        dplyr::filter(year != 0) |>
        dplyr::select(year, row_idx, age) |>
        dplyr::collect(),

      if (!is.null(poolsCols)){
        arrow::open_dataset(file.path(cbmOutputsDB, "pools")) |>
          dplyr::filter(year != 0) |>
          dplyr::select(c("year", "row_idx", poolsCols)) |>
          dplyr::collect()
      }else{
        arrow::open_dataset(file.path(cbmOutputsDB, "pools")) |>
          dplyr::filter(year != 0) |>
          dplyr::collect()
      },

      by = c("year", "row_idx")),

    by = c("year", "row_idx"))

  pqTbl <- dplyr::rename(pqTbl, simYear = year, cohortGroupID = row_idx)
  pqTbl <- data.table::as.data.table(pqTbl, key = c("simYear", "cohortGroupID"))
  pqTbl
}
