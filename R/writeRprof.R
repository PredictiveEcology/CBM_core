
writeRprof <- function(sim, chunk_size, process, timestep, outFile){

  timeTblPath <- file.path(sim$CBM4data, "Rprof.csv")
  timeTbl <- if (file.exists(timeTblPath)) data.table::fread(timeTblPath) else data.table::data.table()

  tsDir <- file.path(sim$CBM4data, "simulation", "simulation", paste0("timestep=", timestep))

  sumRprof <- summaryRprof(outFile, memory = "both")
  sumRprofRow <- sumRprof$by.total[paste0("\"", process, "\""),]

  newRow <- data.table::data.table(
    pixels       = nrow(sim$standDT),
    cohorts      = nrow(sim$cohortDT),
    chunk_size   = chunk_size,
    chunk_index  = sum(!is.na(unique(sim$standDT$chunk_index))),
    cohort_index = length(list.files(tsDir)),
    partitions   = length(list.files(tsDir, recursive = TRUE)),
    process      = process,
    time         = as.numeric(sumRprofRow$total.time),
    mem          = as.numeric(sumRprofRow$mem.total),
    start        = file.info(outFile)$ctime
  )

  timeTbl <- rbind(timeTbl, newRow, fill = TRUE)[order(start)]

  data.table::fwrite(timeTbl, timeTblPath)

}

# writeTimes <- function(sim, process, timestep, start, end, mem = NA){
#
#   timeTblPath <- file.path(sim$CBM4data, "times.csv")
#   timeTbl <- if (file.exists(timeTblPath)) data.table::fread(timeTblPath) else data.table::data.table()
#
#   tsDir <- file.path(sim$CBM4data, "simulation", "simulation", paste0("timestep=", timestep))
#
#   newRow <- data.table::data.table(
#     pixels       = nrow(sim$standDT),
#     cohorts      = nrow(sim$cohortDT),
#     chunk_size   = P(sim)$.chunk_size,
#     chunk_index  = sum(!is.na(unique(sim$standDT$chunk_index))),
#     cohort_index = length(list.files(tsDir)),
#     partitions   = length(list.files(tsDir, recursive = TRUE)),
#     process      = process,
#     duration     = as.integer(difftime(end, start, units = "secs")),
#     mem          = as.numeric(mem),
#     start        = start,
#     end          = end
#   )
#
#   timeTbl <- rbind(timeTbl, newRow, fill = TRUE)[order(start)]
#
#   data.table::fwrite(timeTbl, timeTblPath)
#
# }
