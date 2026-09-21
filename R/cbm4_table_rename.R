
# Rename table columns for duration of module events.

cbm4_table_rename <- function() c(

  "pixelIndex"      = "pixel_index",
  "admin_id"        = "admin_boundary_id",
  "admin_name"      = "admin_boundary",
  "eco_id"          = "eco_boundary_id",
  "eco_name"        = "eco_boundary",
  "spatial_unit_id" = "spatial_unit",

  "gcID" = "gc_id",

  "eventID" = "disturbance_id",
  "disturbance_type_name" = "disturbance_type"
)

cbm4_table_setnames <- function(sim, cohortRename = NULL){
  colRename <- c(cbm4_table_rename(), cohortRename)
  for (objectName in inputObjects(sim, "CBM_core")$objectName){
    if (data.table::is.data.table(sim[[objectName]])){
      data.table::setnames(sim[[objectName]], names(colRename), colRename, skip_absent = TRUE)
    }
  }
  if ("gcID" %in% sim$cohortClassifiers) sim$cohortClassifiers[sim$cohortClassifiers %in% "gcID"] <- "gc_id"
}

cbm4_table_setnames_revert <- function(sim, cohortRename = NULL){
  colRename <- c(cbm4_table_rename(), cohortRename)
  for (objectName in inputObjects(sim, "CBM_core")$objectName){
    if (data.table::is.data.table(sim[[objectName]])){
      data.table::setnames(sim[[objectName]], colRename, names(colRename), skip_absent = TRUE)
    }
  }
  if ("gc_id" %in% sim$cohortClassifiers) sim$cohortClassifiers[sim$cohortClassifiers %in% "gc_id"] <- "gcID"
}


