
disturbanceMatch <- function(disturbanceNames, cbm_defaults_db = NULL){

  # Set resource paths
  if (is.null(cbm_defaults_db)) cbm_defaults_db <- libcbmr::get_cbm_defaults_path()

  askUser <- interactive() & !identical(Sys.getenv("TESTTHAT"), "true")
  if (askUser) message("Prompting user to match input disturbances with CBM disturbances:")

  sapply(disturbanceNames, function(name){
    distMatch <- CBMutils::distMatch(
      name,
      cbm_defaults_db = cbm_defaults_db,
      ask = askUser
    ) |> reproducible::Cache()
    distMatch$disturbance_type_id
  })
}

