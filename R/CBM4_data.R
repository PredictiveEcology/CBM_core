
cbm4_read_dataset <- function(
    cbm4_data = NULL,
    dataset_name,
    dataset_path = file.path(cbm4_data, dataset_name)
  ){

  dataTablePaths <- list.files(dataset_path, full.names = TRUE)
  names(dataTablePaths) <- basename(dataTablePaths)
  dataTables <- lapply(dataTablePaths, function(p){
    arrow::open_dataset(p) |> dplyr::collect() |> data.table::as.data.table()
  })
  dataTables
}

