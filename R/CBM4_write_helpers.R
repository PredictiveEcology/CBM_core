
cbm4_new_dataset_from_template <- function(
  cbm4_data = NULL,
  out_dataset_name = NULL,
  template_dataset_name = NULL,
  partitions = NULL,
  classifiers = NULL,
  copy_raster_index_data = FALSE,
  out_dataset      = file.path(cbm4_data, out_dataset_name),
  template_dataset = file.path(cbm4_data, template_dataset_name)
){

  if (length(out_dataset) == 0)       stop("out dataset path is invalid")
  if (length(template_dataset) == 0)  stop("template dataset path is invalid")
  if (!file.exists(template_dataset)) stop("template dataset not found: ", template_dataset)

  if (!is.null(classifiers)){
    pd <- reticulate::import("pandas")
    tags <- pd$DataFrame(
      columns = c("layer_name", "tag"),
      data = reticulate::dict(
        layer_name = as.list(classifiers),
        tag        = as.list(rep("classifier", length(classifiers)))
      )
    )
  }else tags <- NULL

  arrow_space <- reticulate::import("arrow_space")

  inventory_dataset <- arrow_space$raster_indexed_dataset$RasterIndexedDataset(
    dataset_name = template_dataset_name,
    storage_type = "local_storage",
    storage_path_or_uri = template_dataset
  )
  arrow_space$raster_indexed_dataset$RasterIndexedDataset$create_new(
    inventory_dataset,
    out_dataset_name = out_dataset_name,
    out_storage_type = "local_storage",
    out_storage_path_or_uri = out_dataset,
    partitions = partitions,
    tags       = tags,
    copy_raster_index_data = copy_raster_index_data
  )
}

check_table_columns <- function(tableName, table, colNames){

  if (is.null(table)) stop(tableName, " object is NULL")

  if (!all(colNames %in% names(table))) stop(
    tableName, " missing column(s): ",
    paste(shQuote(setdiff(colNames, names(table))), collapse = ", "))
}


