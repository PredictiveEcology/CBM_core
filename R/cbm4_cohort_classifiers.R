
# Auto detect cohort classifier columns
cohortClassifiers <- function(sim){

  if (!is.null(sim$cohortClassifiers)){
    classifiers <- sim$cohortClassifiers

  }else{

    poolCols <- CBM4r::cbm_defaults_readTable("pool", cbm_defaults_db = sim$cbm_defaults_db)$code

    classifiers <- setdiff(names(sim$cohortDT), c(
      "cohortID", "pixel_index", "age", "delay", "delay_spinup", "delay_regen", poolCols, paste0("pools.", poolCols)))
  }

  if ("gcID" %in% classifiers){
    classifiers[classifiers == "gcID"] <- "gc_id"
  }

  classifiers
}



