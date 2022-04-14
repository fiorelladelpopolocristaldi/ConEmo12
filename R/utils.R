# --- Utils

# these functions are used for the internal project organization and for running
# each step of the analysis

# put_packages_readme -----------------------------------------------------

get_all_packages <- function(){
  
  all_pkgs <- suppressMessages(renv::dependencies(progress = FALSE))
  all_pkgs <- unique(all_pkgs$Package)
  
  all_pkgs <- paste0("`", all_pkgs, "`")
  all_pkgs <- paste("-", all_pkgs)
  
  return(all_pkgs)
  
}

# run_script --------------------------------------------------------------

run_script <- function(file, analysis = c("visual", "auditory", "1vs2")){
  
  fun_name <- deparse(substitute(file))
  
  analysis = match.arg(analysis)
  
  suppressMessages(suppressWarnings(source(here::here(paste0("script_", analysis), file))))
  
  clean_env()
  
  cli::cli_alert_success(paste(fun_name, "finished!"))
  
}

# session_info ------------------------------------------------------------

session_info <- function(){
  session <- sessionInfo()
  tibble::tibble(
    Info = c("R version", "Platform", "OS"),
    Value = c(
      session$R.version$version.string,
      session$platform,
      session$running
    )
  )
  
}


# clean_env ---------------------------------------------------------------

clean_env <- function(){
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
}

# conditional -------------------------------------------------------------

conditional <- function(fun){
  function(..., execute) {
    if (execute) fun(...) else ..1
  }
}