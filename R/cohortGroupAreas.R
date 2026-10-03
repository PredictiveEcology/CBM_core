
# Total area (ha) of each cohort group, ordered by row_idx, from the pixel areas in standDT.
# Looks up pixel areas by match() and sums with rowsum() instead of merging the pixel-level tables.
cohortGroupAreas <- function(key, standDT){
  idx  <- match(key$pixelIndex, standDT$pixelIndex)
  keep <- !is.na(idx)
  as.vector(rowsum(as.numeric(standDT$area[idx[keep]]), key$row_idx[keep])) / 10000
}

