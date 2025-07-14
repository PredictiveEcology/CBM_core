
# Cohort group NPP
simCohortGroupNPP <- function(sim){
  cbmOutputsDB_cohortGroupNPP(sim$cbmOutputsDB) |>
    Cache(.cacheExtra = file.info(sim$cbmOutputsDB)$mtime)
}

cbmOutputsDB_cohortGroupNPP <- function(cbmOutputsDB){

  pqTbl <- dplyr::left_join(

    arrow::open_dataset(file.path(cbmOutputsDB, "cohortGroupKeep")) |>
      dplyr::filter(year != 0) |>
      dplyr::select(year, cohortGroupID) |>
      dplyr::collect() |>
      dplyr::group_by(year, cohortGroupID) |>
      dplyr::summarize(N = dplyr::n()),

    arrow::open_dataset(file.path(cbmOutputsDB, "flux")) |>
      dplyr::filter(year != 0) |>
      dplyr::group_by(year, cohortGroupID) |>
      dplyr::collect() |>
      dplyr::summarize(NPP = sum(
        DeltaBiomass_AG, DeltaBiomass_BG,
        TurnoverMerchLitterInput, TurnoverFolLitterInput,
        TurnoverOthLitterInput, TurnoverCoarseLitterInput, TurnoverFineLitterInput)),

    by = c("year", "cohortGroupID"))

  pqTbl <- dplyr::rename(pqTbl, simYear = year)
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

    arrow::open_dataset(file.path(cbmOutputsDB, "cohortGroupKeep")) |>
      dplyr::filter(year != 0) |>
      dplyr::select(year, cohortGroupID) |>
      dplyr::collect() |>
      dplyr::group_by(year, cohortGroupID) |>
      dplyr::summarize(N = dplyr::n()),

    dplyr::left_join(

      arrow::open_dataset(file.path(cbmOutputsDB, "state")) |>
        dplyr::filter(year != 0) |>
        dplyr::select(year, cohortGroupID, age) |>
        dplyr::collect(),

      if (!is.null(poolsCols)){
        arrow::open_dataset(file.path(cbmOutputsDB, "pools")) |>
          dplyr::filter(year != 0) |>
          dplyr::select(c("year", "cohortGroupID", poolsCols)) |>
          dplyr::collect()
      }else{
        arrow::open_dataset(file.path(cbmOutputsDB, "pools")) |>
          dplyr::filter(year != 0) |>
          dplyr::collect()
      },

      by = c("year", "cohortGroupID")),

    by = c("year", "cohortGroupID"))

  pqTbl <- dplyr::rename(pqTbl, simYear = year)
  pqTbl <- data.table::as.data.table(pqTbl, key = c("simYear", "cohortGroupID"))
  pqTbl
}
