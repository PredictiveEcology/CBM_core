
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

# This example is based on cbm4 use case example 4_2 from cbm4 v2.14.2
# https://github.com/cat-cfs/cbm4/tree/2.14.2/use_cases/Scenario_4/Run4_2

test_that("cbm4_spinup", {

  # Source functions
  for (f in list.files(file.path(spadesTestPaths$RProj, "R", pattern = "\\.R$"), full.names = TRUE)) source(f)

  # Set virtual environment
  reticulate::use_virtualenv("C:/Users/sumurray/cbm4/.venv")

  # Download CBM-CFS3 defaults database
  cbm_defaults_db <- file.path(spadesTestPaths$temp$inputs, "cbm_defaults.db")
  if (!file.exists(cbm_defaults_db)) download.file(
    "https://raw.githubusercontent.com/cat-cfs/libcbm_py/main/libcbm/resources/cbm_defaults_db/cbm_defaults_v1.2.9300.391.db",
    cbm_defaults_db, mode = "wb", quiet = TRUE)

  # Copy CBM4 datasets
  cbm4_data_in <- file.path(spadesTestPaths$testdata,     "CBM4", "data.run4_2_spinup.zip")
  cbm4_data    <- file.path(spadesTestPaths$temp$outputs, "CBM4", tools::file_path_sans_ext(basename(cbm4_data_in)))

  unlink(cbm4_data, recursive = TRUE)
  if (file.exists(cbm4_data)) stop("Could not remove cbm4_data: ", cbm4_data)
  dir.create(dirname(cbm4_data), recursive = TRUE, showWarnings = FALSE)
  utils::unzip(cbm4_data_in, exdir = dirname(cbm4_data))

  # Spinup
  cbm4_spinup(
    cbm4_data, cbm_defaults_db = cbm_defaults_db
  )

  # Validate
  expect_true(file.exists(file.path(cbm4_data, "simulation")))
  simDataStep0 <- arrow::open_dataset(file.path(cbm4_data, "simulation", "simulation")) |>
    dplyr::filter(timestep == 0) |>
    dplyr::collect()
  expect_equal(nrow(simDataStep0), 2)

})

test_that("cbm4_step", {

  # Source functions
  for (f in list.files(file.path(spadesTestPaths$RProj, "R", pattern = "\\.R$"), full.names = TRUE)) source(f)

  # Set virtual environment
  reticulate::use_virtualenv("C:/Users/sumurray/cbm4/.venv")

  # Download CBM-CFS3 defaults database
  cbm_defaults_db <- file.path(spadesTestPaths$temp$inputs, "cbm_defaults.db")
  if (!file.exists(cbm_defaults_db)) download.file(
    "https://raw.githubusercontent.com/cat-cfs/libcbm_py/main/libcbm/resources/cbm_defaults_db/cbm_defaults_v1.2.9300.391.db",
    cbm_defaults_db, mode = "wb", quiet = TRUE)

  # Copy CBM4 datasets
  cbm4_data_in <- file.path(spadesTestPaths$testdata,     "CBM4", "data.run4_2_step.zip")
  cbm4_data    <- file.path(spadesTestPaths$temp$outputs, "CBM4", tools::file_path_sans_ext(basename(cbm4_data_in)))

  unlink(cbm4_data, recursive = TRUE)
  if (file.exists(cbm4_data)) stop("Could not remove cbm4_data: ", cbm4_data)
  dir.create(dirname(cbm4_data), recursive = TRUE, showWarnings = FALSE)
  utils::unzip(cbm4_data_in, exdir = dirname(cbm4_data))

  ## Alter the disturbance to occur at year 2
  ## This includes a disturbance with a transition
  file.rename(file.path(cbm4_data, "disturbance/disturbance/disturbance_order=0/timestep=61"),
              file.path(cbm4_data, "disturbance/disturbance/disturbance_order=0/timestep=2"))
  file.rename(file.path(cbm4_data, "disturbance/disturbance-raster_index/disturbance_order=0/timestep=61"),
              file.path(cbm4_data, "disturbance/disturbance-raster_index/disturbance_order=0/timestep=2"))

  # Step: timestep 1
  cbm4_step(
    cbm4_data, cbm_defaults_db = cbm_defaults_db,
    timestep = 1
  )

  # Validate
  simDataStep1 <- arrow::open_dataset(file.path(cbm4_data, "simulation", "simulation")) |>
    dplyr::filter(timestep == 1) |>
    dplyr::collect()
  expect_equal(nrow(simDataStep1), 2)

  # Step: timestep 2
  cbm4_step(
    cbm4_data, cbm_defaults_db = cbm_defaults_db,
    timestep = 2
  )

  # Validate
  simDataStep2 <- arrow::open_dataset(file.path(cbm4_data, "simulation", "simulation")) |>
    dplyr::filter(timestep == 2) |>
    dplyr::collect()
  expect_equal(nrow(simDataStep2), 3)

})


