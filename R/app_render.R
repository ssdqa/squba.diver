
#' SQUBA Dive
#'
#' Render the Shiny interface for SQUBA Diver. This function will
#' read all CSV files in the indicated directory and render a bespoke
#' interface displaying the results. Users can then use the reactively
#' generated toggles to explore the data in different ways, like viewing
#' different numeric columns on the plot or utilizing the large N
#' functionality. Users can also take notes on the plot and save them to
#' a reactive table, or download the plots to save and use in manuscripts.
#'
#' @param squba_results_directory
#'     the local directory where results in CSV format are stored
#'
#'
#' @returns
#'  a Shiny app displaying all of (and limited to only) the results in
#'  the input directory
#'
#' @export
#'
#' @examples
#' \dontrun{
#' squba_dive('path/to/results')
#' }
#'
squba_dive <- function(squba_results_directory = '../dummy_rslts'){

  Sys.setenv('squba_rslt_dir__' = squba_results_directory)

  local({

    squba_input <- format_squba_tbls()

    #### Check Registry ####
    squba_reg <- list(
      list(id = "ca", name = "Cohort Attrition", data = "ca", ref_val = 2),
      list(id = "evp", name = "Expected Variables Present", data = "evp", ref_val = 3),
      list(id = "pf", name = "Patient Facts", data = "pf", ref_val = 4)
    )

    app_dir = system.file('app', package = 'squba.diver')

    server <- source(file.path(app_dir, "server.R"), local = environment())
    ui <- source(file.path(app_dir, "ui.R"), local = environment())

    shiny::shinyApp(ui = ui, server = server)
  })

}
