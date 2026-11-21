
setupProjectTest <- function(
    projectName,
    times,
    modules     = moduleName,
    projectPath = getwd(),
    modulePath  = file.path(tempDir, "modules"),
    inputPath   = file.path(tempDir, "inputs"),
    cachePath   = file.path(tempDir, "cache"),
    outputPath  = file.path(tempDir, "outputs", projectName),
    packagePath = Sys.getenv("R_LIBS_USER"),
    tempDir     = file.path(tempdir(), "testthat-module"),
    ...){

  # Get module directory and name
  moduleDir  <- normalizePath(test_path("../.."), winslash = "/")
  moduleName <- basename(moduleDir)

  # Interactive testing: test module in place
  if (!"modulePath" %in% names(match.call()) &
      !testthat::is_testing() & identical(modules, moduleName)){
    modulePath <- dirname(moduleDir)
  }

  # Copy module to modulePath
  if (!file.exists(file.path(modulePath, moduleName))){
    dir.create(file.path(modulePath, moduleName), recursive = TRUE, showWarnings = FALSE)
    for (f in file.path(moduleDir, c(paste0(moduleName, ".R"), "R", "data"))){
      if (file.exists(f)) file.copy(f, file.path(modulePath, moduleName), recursive = TRUE)
    }
  }

  # Set up project
  simInitInput <- SpaDES.project::setupProject(
    Restart = FALSE,
    updateRprofile = FALSE,
    modules = modules,
    times = times,
    paths = list(
      projectPath = projectPath,
      modulePath  = modulePath,
      packagePath = packagePath,
      inputPath   = inputPath,
      cachePath   = cachePath,
      outputPath  = outputPath,
      testdata    = testthat::test_path("testdata")
    ),
    options = list(
      spades.useRequire = !identical(modules, moduleName)
    )
  )
  simInitInput <- c(simInitInput, list(...))
  return(simInitInput)
}

