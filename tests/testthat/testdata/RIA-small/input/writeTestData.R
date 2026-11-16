
source(testthat::test_path("setup.R"))

testData <- list(
  pixelGroupMap         = file.path(spadesTestPaths$testdata, "LandRCBM-RIA-small/input", "pixelGroupMap.tif") |> terra::rast(),
  cohortData            = file.path(spadesTestPaths$testdata, "LandRCBM-RIA-small/input", "cohortData.rds") |> readRDS(),
  yieldTablesCumulative = file.path(spadesTestPaths$testdata, "LandRCBM-RIA-small/input", "yieldTablesCumulative.rds") |> readRDS(),
  yieldTablesId         = file.path(spadesTestPaths$testdata, "LandRCBM-RIA-small/input", "yieldTablesId.rds") |> readRDS(),
  table6tb              = data.table::fread("https://nfi.nfis.org/resources/biomass_models/appendix2_table6_tb.csv"),
  table7tb              = data.table::fread("https://nfi.nfis.org/resources/biomass_models/appendix2_table7_tb.csv"),
  tableMerchantability  = reproducible::prepInputs(
    url = "https://drive.google.com/file/d/1wa2QMd7Eo-bPpfigchdpPPPxo7NVpPiC",
    destinationPath = tempdir(),
    fun = data.table::fread
  ) |> cbind(minAge = 15L)
)
testData$standDT <- data.table::data.table(
  pixelIndex   = 1:terra::ncell(testData$pixelGroupMap),
  admin_name   = "British Columbia",
  admin_abbrev = "BC",
  eco_id       = 14
)

AGB <- data.table::data.table(
  pixelIndex = terra::cells(testData$pixelGroupMap)
)
AGB[, pixelGroup := terra::extract(testData$pixelGroupMap, pixelIndex)]
AGB <- merge(AGB, testData$cohortData,    by = "pixelGroup")
AGB <- merge(AGB, testData$standDT,       by = "pixelIndex")
AGB <- merge(AGB, testData$yieldTablesId, by = "pixelIndex")
AGB <- cbind(AGB, CBMutils::sppMatch(
  AGB$speciesCode,
  match = "LandR", return = c("Broadleaf", "CanfiCode"))[
    , .(sw = !Broadleaf, canfi_species = CanfiCode)])

data.table::setcolorder(AGB, c("pixelIndex", "speciesCode", "age"))
AGB[, gcID := .GRP, by = .(admin_abbrev, eco_id, yieldTableIndex, speciesCode)]
AGB[, cohortID := .I]
data.table::setkey(AGB, cohortID)

testData$cohortDT <- AGB[, .(cohortID, pixelIndex, age, gcID)]
data.table::setkey(testData$cohortDT, cohortID)

testData$standDT  <- testData$standDT[, .(pixelIndex, admin_abbrev, eco_id)]
data.table::setkey(testData$cohortDT, pixelIndex)

testData$gcMeta <- unique(AGB[, .(gcID, sw)])
data.table::setkey(testData$gcMeta, gcID)

gcIncr <- AGB[, .(gcID, yieldTableIndex, juris_id = admin_abbrev, ecozone = eco_id, speciesCode, canfi_species)] |> unique()
gcIncr <- merge(
  gcIncr,
  testData$yieldTablesCumulative,
  by = c("yieldTableIndex", "speciesCode"),
  allow.cartesian = TRUE
)

# Convert biomass units from g/m^2 to tonnes/ha: 1 g/m^2 = 0.01 tonnes/ha
data.table::setnames(gcIncr, "biomass", "B")
gcIncr[, B := B / 100]
gcIncr[age==0, B:= 0]

gcIncr <- CBMutils::cumPoolsCreateAGB(
  gcIncr,
  table6tb   = testData$table6tb,
  table7tb   = testData$table7,
  tableMerch = testData$tableMerchantability
)
data.table::setkey(gcIncr, gcID, age)

poolCols <- c("merch", "foliage", "other")
incCols <- c("merch_inc", "foliage_inc", "other_inc")
gcIncr[, (incCols) := lapply(.SD, function(x) c(NA, diff(x))), .SDcols = poolCols, by = "gcID"]
gcIncr[age == 0, c("merch_inc", "foliage_inc", "other_inc") := list(0, 0, 0)]
testData$gcIncrements <- gcIncr[, .(gcID, age, merch_inc, foliage_inc, other_inc)]

# Write to file
for (table in c("cohortDT", "gcMeta", "gcIncrements")){
  saveRDS(testData[[table]], file.path(spadesTestPaths$testdata, "RIA-small/input", paste0(table, ".rds")))
}

