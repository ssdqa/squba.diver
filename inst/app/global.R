
##' `Load Packages`

#' @import shiny
#' @import shinydashboard
#' @import shinydashboardPlus
#' @import bslib
#' @import shinybusy
#' @import gt
#' @import ggiraph
#' @import ggplot2
#' @import plotly
#' @import squba
#' @import stringr
#' @import dplyr
NULL

##' Format input data
source('format_squba_tbls.R')
Sys.setenv('squba_rslt_dir__' = '../dummy_rslts')
squba_input <- format_squba_tbls()

#### Check Registry ####
squba_reg <- list(
  list(id = "ca", name = "Cohort Attrition", data = "ca", ref_val = 2),
  list(id = "evp", name = "Expected Variables Present", data = "evp", ref_val = 3),
  list(id = "pf", name = "Patient Facts", data = "pf", ref_val = 4)
)

##' Source misc funcs
source('misc_funcs.R')

for(i in list.files('modules')){
  source(file.path('modules', i))
}
