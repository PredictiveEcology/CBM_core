
# Rename table columns for duration of module events.

cbm4_table_rename <- function() list(

  cohortDT = c(
    "pixelIndex" = "pixel_index",
    "gcID"       = "gc_id"
  ),
  standDT = c(
    "pixelIndex"      = "pixel_index",
    "admin_id"        = "admin_boundary_id",
    "admin_name"      = "admin_boundary",
    "eco_id"          = "eco_boundary_id",
    "eco_name"        = "eco_boundary",
    "spatial_unit_id" = "spatial_unit"
  ),
  gcMeta = c(
    "gcID"            = "gc_id",
    "admin_id"        = "admin_boundary_id",
    "admin_name"      = "admin_boundary",
    "eco_id"          = "eco_boundary_id",
    "eco_name"        = "eco_boundary",
    "spatial_unit_id" = "spatial_unit"
  ),
  gcIncrements = c(
    "gcID" = "gc_id"
  ),
  disturbanceMeta = c(
    "eventID" = "disturbance_id",
    "disturbance_type_name" = "disturbance_type"
  ),
  disturbanceEvents = c(
    "eventID"    = "disturbance_id",
    "pixelIndex" = "pixel_index"
  )
)

cbm4_table_setnames <- function(sim, cohortRename = NULL){
  colRename <- cbm4_table_rename()
  colRename$cohortDT <- c(colRename$cohortDT, cohortRename)
  for (table in intersect(names(colRename), names(sim))){
    data.table::setnames(sim[[table]], names(colRename[[table]]), colRename[[table]], skip_absent = TRUE)
  }
}

cbm4_table_setnames_revert <- function(sim, cohortRename = NULL){
  colRename <- cbm4_table_rename()
  colRename$cohortDT <- c(colRename$cohortDT, cohortRename)
  for (table in intersect(names(colRename), names(sim))){
    data.table::setnames(sim[[table]], colRename[[table]], names(colRename[[table]]), skip_absent = TRUE)
  }
}


