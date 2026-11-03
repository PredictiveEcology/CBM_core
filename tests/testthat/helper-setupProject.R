
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
    ...){

  if (!testthat::is_testing() & identical(modules, moduleName)){
    modulePath <- dirname(moduleDir) # Test module in place
  }else{
    if (!file.exists(file.path(modulePath, moduleName))){
      dir.create(modulePath, recursive = TRUE, showWarnings = FALSE)
      file.copy(moduleDir, modulePath, recursive = TRUE)
    }
  }

  simInitInput <- SpaDES.project::setupProject(
    modules = modules,
    times   = times,
    paths   = list(
      projectPath = projectPath,
      modulePath  = modulePath,
      packagePath = packagePath,
      inputPath   = inputPath,
      cachePath   = cachePath,
      outputPath  = outputPath,
      testdata    = testthat::test_path("testdata")
    )
  )
  simInitInput <- c(simInitInput, list(...))
  return(simInitInput)
}

