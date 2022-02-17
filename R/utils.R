# --- Utils

# these functions are used for the internal project organization and for running
# each step of the analysis

# get_packages ------------------------------------------------------------

get_all_pakages <- function(folder_list, exts){
  folder_list <- here::here(folder_list)
  folder_list |>
    get_relevant_file(exts) |>
    get_all_files_content() |>
    detect_packages() |>
    get_pkg_name() 
}

get_pkg_name <- function(pkgs){
  out <- stringr::str_extract(pkgs, pattern = "\\((.*?)\\)")
  out <- stringr::str_remove_all(out, "\\(|\\)")
  return(out)
}

detect_packages <- function(files_content){
  index <- stringr::str_detect(files_content, "library\\(")
  out <- files_content[index]
  unique(out)
}

get_relevant_file <- function(folder_list, exts){
  out <- unlist(purrr::map(folder_list, function(x) list.files(x, full.names = TRUE)))
  pattern <- paste0(exts, collapse = "|")
  index <- stringr::str_detect(out, pattern)
  out[index]
}

get_all_files_content <- function(files){
  unlist(purrr::map(files, function(x) readLines(x, warn = F)))
}


# put_packages_readme -----------------------------------------------------

get_all_packages <- function(){
  
  all_pkgs <- get_all_pakages(
    c(
      "script_auditory",
      "script_visual",
      "script_1vs2"
    ), 
    c(
      ".R",
      ".Rmd"
    )
  )
  
  all_pkgs <- paste0("`", all_pkgs, "`")
  all_pkgs <- paste("-", all_pkgs)
  
  return(all_pkgs)
  
}

# load_all_fun ----------------------------------------------------------------

load_all_fun <- function(){
  dirs <- c("script_auditory", "script_visual", "script_1vs2")
  files <- unlist(sapply(dirs, function(x) list.files(here::here(x), full.names = TRUE)))
  funs <- lapply(files, function(x) source(here::here(x)))
}

# run_script --------------------------------------------------------------

run_script <- function(file, analysis = c("visual", "auditory", "1vs2")){
  
  fun_name <- deparse(substitute(file))
  
  analysis = match.arg(analysis)
  
  suppressMessages(suppressWarnings(source(here::here(paste0("script_", analysis), file))))
  
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

# conditional -------------------------------------------------------------

conditional <- function(fun){
  function(..., execute) {
    if (execute) fun(...) else ..1
  }
}
