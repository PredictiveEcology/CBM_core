
CacheCBM4dataset <- function(cacheOut, cbm4_data, dataset_name, overwrite = TRUE){

  cacheId  <- reproducible::cacheId(cacheOut)
  if (is.null(cacheId)) return(invisible())

  cacheZIP <- file.path(getOption("reproducible.cachePath"), "CBM_core", dataset_name, paste0(cacheId, ".zip"))

  if (attr(cacheOut, ".Cache")$newCache){
    dir.create(dirname(cacheZIP), recursive = TRUE, showWarnings = FALSE)
    zip::zip(cacheZIP, root = cbm4_data, files = dataset_name)
    message(cli::col_blue("Cached CBM4 dataset: ", dataset_name))

  }else{
    if (file.exists(cacheZIP)){
      dir.create(cbm4_data, recursive = TRUE, showWarnings = FALSE)
      zip::unzip(cacheZIP, exdir = cbm4_data, overwrite = overwrite)
      message(cli::col_blue("Cached CBM4 dataset retrieved: ", dataset_name))

    }else{
      reproducible::clearCache(cacheId = cacheId, ask = FALSE, verbose = FALSE)
      stop("CBM4 dataset cache not found; re-run simulation to write dataset and fix cache")
    }
  }
}

digestFile <- function(f){
  md5hash <- tools::md5sum(f)
  names(md5hash) <- NULL
  md5hash
}

digestDir <- function(d){
  fs <- list.files(d, recursive = TRUE)
  fs_hash <- setNames(tools::md5sum(file.path(d, fs)), fs)
  digest::digest(fs)
}


