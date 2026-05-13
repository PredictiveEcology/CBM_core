# CBM_core

::: rmdimportant
This documentation is a work in progress. Potential discrepancies and omissions may exist for the time being. If you find any, please contact us [here](%22https://github.com/PredictiveEcology/CBM_core/issues%22).
:::

## Overview

[CBM_core](https://github.com/PredictiveEcology/CBM_core) is the central module of [spadesCBM](https://github.com/PredictiveEcology/spadesCBM) implementing the [Carbon Budget Model of the Canadian Forest Sector](https://natural-resources.canada.ca/climate-change/climate-change-impacts-forests/carbon-budget-model) version 4 (CBM-CFS4). This is where carbon transfers are calculated at every time step, disturbances are applied, and stocks are tracked.

The [CBM4r](https://github.com/PredictiveEcology/CBM4r) R package provides an R interface to CBM4 Python applications. The [CBMutils](https://github.com/PredictiveEcology/CBMutils) R package contains tools to plot simulation results.

The SpaDES toolkit enables a modular, repeatable, and continuous workflow. This brings the transparency and flexibility needed for scientists to modify, evaluate, and test new inputs, new data, and new algorithms while also providing accessibility to non-researcher users.

## Module inputs

The simulation study area is set as a raster grid (input `masterRaster`). In *CBM_core*, each forested `masterRaster` pixel represents a "stand" (input `standDT`) with one or more cohorts. Cohorts (input `cohortDT`) share the same age (time since disturbance) and other classifiers (such as tree species) associating them to growth curves (inputs `gcMeta` and `gcIncrements`). Disturbance events (inputs `disturanceMeta` and `disturbanceEvents`) occur within stands throughout the simulation affecting their cohorts.

CBM parameters can be altered by providing a customized version of the CBM defaults SQLite database (input `cbm_defaults_db`).

  -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  Name                Class        Description                                                                                                                                                                                       Table columns
  ------------------- ------------ ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  masterRaster        SpatRaster   Raster grid defining the study area.                                                                                                                                                              

  standDT             data.table   Table of stand attributes. Stands can have 1 or more cohorts.                                                                                                                                     *pixelIndex*: Stand ID`<br>`{=html}*area*: Stand area in meters`<br>`{=html}*admin_name*: Canada province or territory name`<br>`{=html}*admin_abbrev*: Optional. Canada province or territory 2-character abbreviation. 'admin_name' or 'admin_abbrev' required.`<br>`{=html}*eco_id*: Canada ecozone ID`<br>`{=html}*historic_disturbance_type*: Optional. Historic disturbance type. Defaults to parameter `def_historic_disturbance_type``<br>`{=html}*last_pass_disturbance_type*: Optional. Last pass disturbance type. Defaults to parameter `def_last_pass_disturbance_type`

  cohortDT            data.table   Table of cohort attributes. Must contain one or more additional classifier columns.                                                                                                               *pixelIndex*: Stand ID`<br>`{=html}*age*: Cohort age at simulation start`<br>`{=html}*ageSpinup*: Optional. Alternative cohort age to use in the spinup`<br>`{=html}*delay_spinup*: Optional. Regeneration delay used in the spinup. Defaults to parameter `def_delay_spinup``<br>`{=html}*delay_regen*: Optional. Regeneration delay post disturbance in years. Defaults to parameter `def_delay_regen`

  cohortClassifiers   character    Optional. Name(s) of cohort classifier columns. Defaults to all additional `cohortDT` columns.                                                                                                    

  gcMeta              data.table   Growth curve metadata. One or more `cohortClassifiers` columns must be present. Columns `admin_name`, `admin_abbrev`, and/or `eco_id` may be present to associate curves with specific regions.   *gcID*: Growth curve ID`<br>`{=html}*sw*: TRUE (softwood) or FALSE (hardwood)

  gcIncrements        data.table   Growth curve increments.                                                                                                                                                                          *gcID*: Growth curve ID`<br>`{=html}*age*: Cohort age`<br>`{=html}*merch_inc*: Change in carbon (MgC/ha/year) in merchantable pools`<br>`{=html}*foliage_inc*: Change in carbon (MgC/ha/year) in foliage pools`<br>`{=html}*other_inc*: Change in carbon (MgC/ha/year) in other pools

  disturbanceMeta     data.table   Disturbance event types. `cohortClassifiers` columns can be present.                                                                                                                              *eventID*: Event type ID`<br>`{=html}*disturbance_type_name*: Disturbance type name`<br>`{=html}*disturbance_type_id*: Optional. CBM disturbance type ID. Can use this or 'disturbance_type_name'.`<br>`{=html}*priority*: Optional. Priority of event assignment to a pixel if more than one event occurs.`<br>`{=html}*description*: Optional. Disturbance description

  disturbanceEvents   data.table   Disturbance events.                                                                                                                                                                               *pixelIndex*: Stand ID`<br>`{=html}*year*: Year of disturbance`<br>`{=html}*eventID*: Event type ID. This associates events to metadata in the 'disturbanceMeta' table.

  cbm_defaults_db     character    Optional. Path to an SQLite database of CBM parameters Defaults to the most latest version of the CBM defaults database.                                                                          
  -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

  : *CBM_core* input objects

  ---------------------------------------------------------------------------------------------------------
  Name                             Class       Default    Description
  -------------------------------- ----------- ---------- -------------------------------------------------
  fixedCohorts                     logical     TRUE       Stand cohorts are fixed for simulation duration

  def_delay_spinup                 integer     0          Default regeneration delay used in the spinup

  def_delay_regen                  integer     0          Default regeneration delay post disturbance

  def_historic_disturbance_type    character   Wildfire   Default historic disturbance type.

  def_last_pass_disturbance_type   character   Wildfire   Default last pass disturbance type.

  .virtualenv                      character   r-CBM4     Python virtual environment

  .cbm4vers                        character   NA         CBM4 version

  .useCache                        logical     FALSE      Cache module events

  .useCacheCBM4                    logical     TRUE       Cache CBM4 processes

  .chunks                          integer     1          Number of partition chunks

  .max_workers                     integer     NA         Number of parallel processes

  .saveAll                         logical     FALSE      Save all available data

  .plot                            logical     TRUE       Plot simulation results
  ---------------------------------------------------------------------------------------------------------

  : *CBM_core* input parameters

## Module events

Each of the following events are run for the first simulation year. The `annualDisturbances` and `annualStep` events are repeated for each following simulation year.

### `Init`

This event sets the CBM4 data directory path (output `CBM4data`) and links *CBM4r* to the CBM parameters database (input `cbm_defaults_db`) if provided.

The `r-CBM4` Python virtual environment is set up and activated. If a suitable version of Python is not available, it will be installed.

2.  `setStands`

This event reads the input stand table (input `standDT`) and adds additional attributes required throughout the simulation. See [`CBM4r::cbm4_set_grid_meta`](https://github.com/PredictiveEcology/CBM4r/blob/main/inst/docs/CBM4r_Reference_Manual.md#cbm4_set_grid_meta-cbm4-set-grid-metadata) for more details.

### `spinup`

This event initializes the landscape by performing the CBM spinup.

The cohort inventory (input `cohortDT`) is written the CBM4 data directory with [`CBM4r::cbm4_write_inventory`](https://github.com/PredictiveEcology/CBM4r/blob/main/inst/docs/CBM4r_Reference_Manual.md#cbm4_write_inventory-cbm4-write-inventory). Growth increments (inputs `gcMeta` and `gcIncrements`) and ecological parameters (input `cbm_defaults_db`) are written to the CBM4 data directory with [`CBM4r::cbm4_write_spinup_parameters`](https://github.com/PredictiveEcology/CBM4r/blob/main/inst/docs/CBM4r_Reference_Manual.md#cbm4_write_spinup_parameters-cbm4-write-spinup-parameters). The spinup is performed with [`CBM4r::cbm4_spinup`](https://github.com/PredictiveEcology/CBM4r/blob/main/inst/docs/CBM4r_Reference_Manual.md#cbm4_spinup-cbm4-spinup).

If parameter `fixedCohorts = FALSE` the cohort inventory and pools state will be read into the `cohortDT` object for use by other modules.

### `annualDisturbances`

This event writes disturbance events for the current simulation year to the CBM4 data directory to be used in the `annualStep`. This process is repeated yearly to allow for disturbances to be provided by other modules on a yearly basis.

Disturbances are written to file with [`CBM4r::cbm4_write_disturbance`](https://github.com/PredictiveEcology/CBM4r/blob/main/inst/docs/CBM4r_Reference_Manual.md#cbm4_write_disturbance-cbm4-write-disturbance).

### `annualStep`

This event runs the CBM annual step where carbon transfers are applied for each simulation year.

The cohort inventory and pools state will be read directly from the CBM4 data directory unless parameter `fixedCohorts = FALSE` in which case the cohort inventory and pools state (input `cohortDT`) will be written the CBM4 data directory with [`CBM4r::cbm4_write_simulation_inventory`](https://github.com/PredictiveEcology/CBM4r/blob/main/inst/docs/CBM4r_Reference_Manual.md#cbm4_write_simulation_inventory-cbm4-write-simulation-inventory).

Growth increments (inputs `gcMeta` and `gcIncrements`) and ecological parameters (input `cbm_defaults_db`) are written to the CBM4 data directory with [`CBM4r::cbm4_write_step_parameters`](https://github.com/PredictiveEcology/CBM4r/blob/main/inst/docs/CBM4r_Reference_Manual.md#cbm4_write_step_parameters-cbm4-write-step-parameters).

If parameter `fixedCohorts = FALSE` the updated cohort inventory and pools state will be read into the `cohortDT` object for use by other modules.

### `plot`

This final optional event is where all plotting occurs. Plots will be saved as `.png` files in the project outputs folder.

## Module outputs

The primary output produced by *CBM_core* is the CBM4 data directory containing simulation data in the [CBM4 spatial dataset](https://github.com/cat-cfs/tech_docs/tree/main/dev/cbm4/structure#cbm4-spatial-datasets) format. Simulation results can either be read directly as Parquet tables or more easily with the [CBM4 results processor](https://github.com/cat-cfs/tech_docs/tree/main/dev/cbm4/structure#results-processing). For more information see the [CBM4r documentation](https://github.com/PredictiveEcology/CBM4r).

  -----------------------------------------------------------------------------------------------------------------------
  Name                Class        Description
  ------------------- ------------ --------------------------------------------------------------------------------------
  CBM4data            character    Path to CBM4 spatial dataset directory containing simulation data in Parquet format.

  emissionsProducts   data.table   Emissions and product totals for each simulation year.
  -----------------------------------------------------------------------------------------------------------------------

  : *CBM_core* output objects

### Plotting outputs with CBMutils

``` r
# Read results as CBM4 SQLResultsProcessor
cbm4_results <- CBM4r::cbm4_results_processor(sim$CBM4data)

# Plot emissions and products
CBMutils::cbm4PlotEmissionsProducts(cbm4_results, yearStart = start(sim))

# Plot pool proportions
CBMutils::cbm4PlotPoolProportions(cbm4_results, yearStart = start(sim))

# Plot Net Primary Productivity (NPP)
plotNPP <- CBMutils::cbm4MapNPP(cbm4_results, yearStart = start(sim))
plotNPP[[as.character(start(sim))]]
plotNPP[[as.character(end(sim))]]

# Plot total carbon
plotTC <- CBMutils::cbm4MapTotalCarbon(cbm4_results, yearStart = start(sim))
plotTC[[as.character(start(sim))]]
plotTC[[as.character(end(sim))]]
```

## Links to other modules

- [CBM_defaults](https://github.com/PredictiveEcology/CBM_defaults)
- [CBM_dataPrep](https://github.com/PredictiveEcology/CBM_dataPrep)
- [CBM_dataPrep_SK](https://github.com/PredictiveEcology/CBM_dataPrep_SK)
- [CBM_vol2biomass](https://github.com/PredictiveEcology/CBM_vol2biomass)
