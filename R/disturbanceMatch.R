
disturbanceMatch <- function(disturbanceNames, cbm_defaults_db = NULL){

  # Set resource paths
  if (is.null(cbm_defaults_db)) cbm_defaults_db <- libcbmr::get_cbm_defaults_path()

  askUser <- interactive() & !identical(Sys.getenv("TESTTHAT"), "true")
  if (askUser) message("Prompting user to match input disturbances with CBM disturbances:")

  cbm_db_md5hash <- unname(tools::md5sum(cbm_defaults_db))

  sapply(disturbanceNames, function(name){
    distMatch <- CBMutils::distMatch(
      name,
      cbm_defaults_db = cbm_defaults_db,
      ask = askUser
    ) |> reproducible::Cache(omitArgs = c("cbm_defaults_db", "ask"), .cacheExtra = cbm_db_md5hash)
    distMatch$disturbance_type_id
  })
}

