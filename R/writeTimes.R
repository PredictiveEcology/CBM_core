
writeTimes <- function(sim, process, timestep, start, end){

  timeTblPath <- file.path(sim$CBM4data, "times.csv")
  timeTbl <- if (file.exists(timeTblPath)) data.table::fread(timeTblPath) else data.table::data.table()

  tsDir <- file.path(sim$CBM4data, "simulation", "simulation", paste0("timestep=", timestep))

  newRow <- data.table::data.table(
    start        = start,
    end          = end,
    duration     = as.integer(difftime(end, start, units = "secs")),
    timestep     = timestep,
    process      = process,
    pixels       = nrow(sim$standDT),
    cohorts      = nrow(sim$cohortDT),
    chunk_size   = P(sim)$.chunk_size,
    chunk_index  = sum(!is.na(unique(sim$standDT$chunk_index))),
    cohort_index = length(list.dirs(tsDir, recursive = FALSE)),
    partitions   = length(list.files(tsDir, pattern = "\\.parquet$", ignore.case = TRUE, recursive = TRUE))
  )

  timeTbl <- rbind(timeTbl, newRow, fill = TRUE)[order(start)]

  data.table::fwrite(timeTbl, timeTblPath)

}
