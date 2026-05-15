
#' Format SQUBA tables
#'
#' @param squba_rslt_directory result directory where SQUBA result CSVs live
#'
#' @returns list of dataframes that will be used by the app
#'
#' @importFrom dplyr pull
#' @importFrom dplyr mutate
#' @importFrom dplyr distinct
#' @importFrom stringr str_split
#'
#'
format_squba_tbls <- function(squba_rslt_directory = Sys.getenv('squba_rslt_dir__')){

  squba_files <- list.files(squba_rslt_directory)

  rslt_list <- list()

  for(i in squba_files){

    if(!grepl('.csv$', i)){
      cli::cli_alert_warning(paste0(i, ' is not in csv format. Skipping...'))
      next()
    }

    dat <- readr::read_csv(file.path(squba_rslt_directory, i))

    if(any(colnames(dat) == 'time_start')){
      dat <- dat %>% dplyr::mutate(time_start = as.Date(time_start))
    }

    output_func <- dat %>% dplyr::distinct(output_function) %>% dplyr::pull()

    if(grepl('^cnc_sp', output_func)){
      output_split <- stringr::str_split(output_func, '_', n = 3)
      mod_nm <- paste(output_split[[1]][1], output_split[[1]][2], sep = '_')
      check_nm <- output_split[[1]][3]
    }else{
      output_split <- stringr::str_split(output_func, '_', n = 2)
      mod_nm <- output_split[[1]][1]
      check_nm <- output_split[[1]][2]
    }

    int_list <- list()
    int_list[[i]] <- dat

    if(length(rslt_list[[mod_nm]]) == 0){
      rslt_list[[mod_nm]][[check_nm]] <- int_list
    }else{
      rslt_list[[mod_nm]][[check_nm]] <- append(rslt_list[[mod_nm]][[check_nm]], int_list)
      ## check if mod_nm$check_nm exists; if it does, then pull that table & union?
      ## maybe offer the option to error in case they aren't expecting duplicates?
      ## add a default differentiator column?
    }

  }

  rslt_list$metadata$total_mod <- length(rslt_list)
  k <- 0
  for(j in 1:rslt_list$metadata$total_mod){
    chk_ct <- length(rslt_list[[j]])

    k <- k + chk_ct
  }

  rslt_list$metadata$total_check <- k

  rslt_list

}
