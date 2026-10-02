
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("cohortGroupAreas equals the pixel-level merge() summary", {

  set.seed(1)
  n <- 5000
  standDT <- data.table::data.table(pixelIndex = sample.int(n), area = runif(n, 800, 1000))
  key     <- data.table::data.table(pixelIndex = sample.int(n), row_idx = sample.int(300, n, replace = TRUE))

  old <- merge(key, standDT, by = "pixelIndex")[, .(area = sum(area) / 10000), by = row_idx]
  data.table::setkey(old, row_idx)

  new <- cohortGroupAreas(key, standDT)

  expect_equal(new, old$area, tolerance = 1e-12)

  # Pixels not in standDT are dropped, as with the inner merge
  key2 <- rbind(key, data.table::data.table(pixelIndex = n + 1L, row_idx = 1L))
  expect_equal(cohortGroupAreas(key2, standDT), new)
})

test_that("flux emissions totals equal the whole-table multiplication", {

  set.seed(2)
  flux <- data.table::data.table(row_idx = 1:200, a = runif(200), b = runif(200), c = runif(200))
  area <- runif(200)

  old <- (flux * area)[, lapply(.SD, sum), .SDcols = !"row_idx"]
  new <- flux[, lapply(.SD, function(x) sum(x * area)), .SDcols = !"row_idx"]

  expect_identical(new, old)
})
