
cbm4_format_parameters_ecological <- function(cbm_defaults_db){

  # Read tables
  paramTables <- c(
    "spatial_unit", "spinup_parameter", "eco_boundary", "turnover_parameter", "root_parameter",
    "decay_parameter", "dom_pool", "pool",
    "slow_mixing_rate", "biomass_to_carbon_rate"
  )
  cbmDBcon <- RSQLite::dbConnect(RSQLite::dbDriver("SQLite"), cbm_defaults_db)
  paramTables <- lapply(setNames(paramTables, paramTables), function(tableName){
    data.table::as.data.table(RSQLite::dbReadTable(cbmDBcon, tableName))
  })
  RSQLite::dbDisconnect(cbmDBcon)

  # Transform tables
  data.table::setnames(paramTables$spatial_unit, "id", "inventory.spatial_unit")

  turnoverRemap <- list(
    id                                     = "id",
    turnover.sw_merch                      = "stem_turnover", # TODO: check
    turnover.sw_foliage                    = "sw_foliage",
    turnover.sw_other                      = "sw_branch", # TODO: check
    turnover.sw_stem_snag                  = "sw_stem_snag",
    turnover.sw_branch_snag                = "sw_branch_snag",
    turnover.sw_other_to_branch_snag_split = "branch_snag_split",  # TODO: check
    turnover.sw_coarse_root                = "coarse_root",
    turnover.sw_coarse_root_ag_split       = "coarse_ag_split",
    turnover.sw_fine_root                  = "fine_root",
    turnover.sw_fine_root_ag_split         = "fine_ag_split",
    turnover.hw_merch                      = "stem_turnover", # TODO: check
    turnover.hw_foliage                    = "hw_foliage",
    turnover.hw_other                      = "hw_branch", # TODO: check
    turnover.hw_stem_snag                  = "hw_stem_snag",
    turnover.hw_branch_snag                = "hw_branch_snag",
    turnover.hw_other_to_branch_snag_split = "branch_snag_split", # TODO: check
    turnover.hw_coarse_root                = "coarse_root",
    turnover.hw_coarse_root_ag_split       = "coarse_ag_split",
    turnover.hw_fine_root                  = "fine_root",
    turnover.hw_fine_root_ag_split         = "fine_ag_split"
  )
  paramTables$turnover_parameter <- data.table::as.data.table(
    lapply(turnoverRemap, function(x) paramTables$turnover_parameter[[x]]))

  rootRemap <- list(
    id         = "id",
    root.hw_a  = "hw_a",
    root.sw_a  = "sw_a",
    root.hw_b  = "hw_b",
    root.frp_a = "frp_a",
    root.frp_b = "frp_b",
    root.frp_c = "frp_c"
  )
  data.table::setnames(paramTables$root_parameter, do.call(c, rootRemap), names(rootRemap))

  decayRemap <- list(
    dom_pool_id               = "dom_pool_id",
    decay.POOL_NAME.base_rate = "base_decay_rate",
    decay.POOL_NAME.tref      = "reference_temp",
    decay.POOL_NAME.q10       = "q10",
    decay.POOL_NAME.p_atm     = "prop_to_atmosphere",
    decay.POOL_NAME.max_rate  = "max_rate"
  )
  domPools <- merge(paramTables$dom_pool, paramTables$pool, by.x = "pool_id", by.y = "id")
  paramTables$decay_parameter <- data.table::cbindlist(
    lapply(split(paramTables$decay_parameter, paramTables$decay_parameter$dom_pool_id), function(r){
      pool_name <- domPools[id == r$dom_pool_id]$code
      data.table::setnames(r, do.call(c, decayRemap), gsub("POOL_NAME", pool_name, names(decayRemap)))
      r[, -c("dom_pool_id")]
    })
  )

  # Merge tables
  params <- paramTables$spatial_unit |>
    merge(paramTables$spinup_parameter,   by.x = "spinup_parameter_id",   by.y = "id") |>
    merge(paramTables$eco_boundary,       by.x = "eco_boundary_id",       by.y = "id") |>
    merge(paramTables$turnover_parameter, by.x = "turnover_parameter_id", by.y = "id") |>
    merge(paramTables$root_parameter,     by.x = "root_parameter_id",     by.y = "id")

  params <- cbind(params, paramTables$decay_parameter)

  params[, turnover.slow_mixing_rate   := paramTables$slow_mixing_rate$rate]
  params[, root.biomass_to_carbon_rate := paramTables$biomass_to_carbon_rate$rate]

  # Set enabled
  params[, enabled := 1]

  # Format and return
  data.table::setkey(params, inventory.spatial_unit)
  data.table::setcolorder(params)
  params
}


