
#' Server backend for PF module
#'
#' @param id x
#' @param data x
#' @param input x
#' @param output x
#'
#' @returns XX
#'
#' @import shiny
#'
pf_server <- function(id, data, input, output) {

  #### Load check-specific data ####
  pf_dat <- reactive({
    squba.diver::load_check_data(dat_list = data(),
                                 input = input,
                                 type_input = 'pf_type')
  })

  #### Build sidebar selectors ####
  output$pf_selects <- renderUI({

    inps <- list()

    if('site' %in% colnames(pf_dat())){
      bloop <- pf_dat() %>% dplyr::distinct(site) %>% dplyr::pull()
    }

    if('time_increment' %in% colnames(pf_dat())){
      t <- pf_dat() %>% dplyr::distinct(time_increment) %>% dplyr::pull()
    }else{t <- NULL}

    if('visit_type' %in% colnames(pf_dat())){
      vis <- pf_dat() %>% dplyr::distinct(visit_type) %>% dplyr::pull()
    }else{vis <- NULL}

    if('domain' %in% colnames(pf_dat())){
      d <- pf_dat() %>% dplyr::distinct(domain) %>% dplyr::pull()
    }else{d <- NULL}

    base_type <- stringr::str_remove(input$pf_type, ' \\(.*')

    if(base_type %in% c("Single Site, Exploratory, Cross-Sectional",
                            "Multi Site, Exploratory, Cross-Sectional")){
      opts <- c('Median (All Patients)', 'Median (Patients with a Fact)')
    }else if(base_type == "Single Site, Anomaly Detection, Cross-Sectional"){
      opts <- c('Count Anomalous Facts (Patients with a Fact)', 'Proportion Anomalous Facts (Patients with a Fact)',
                'Count Anomalous Facts (All Patients)', 'Proportion Anomalous Facts (All Patients)')
    }else if(base_type %in% c("Single Site, Exploratory, Longitudinal",
                                  "Multi Site, Exploratory, Longitudinal")){
      opts <- c('Median Fact Count', 'Total Fact Count')
    }else{
      opts <- NULL
    }

    ## Site
    ## Output Type
    ## Visit Type
    ## Domain
    ## Time Increment

    if(!grepl('Single Site', input$pf_type) & input$largen){
      inps <- c(inps, list(selectizeInput(inputId = 'pf_site_m', label = 'Site',
                                          choices = bloop, multiple = TRUE)))
    }else if(grepl('Single Site', input$pf_type)){
      inps <- c(inps, list(selectInput(inputId = 'pf_site_s', label = 'Site',
                                       choices = bloop)))
    }

    if(!is.null(opts)){
      inps <- c(inps, list(selectInput(inputId = 'pf_val', label = 'Output Type',
                                       choices = opts)))
    }

    if(base_type %in% c("Single Site, Anomaly Detection, Longitudinal",
                        "Multi Site, Anomaly Detection, Cross-Sectional",
                        "Multi Site, Anomaly Detection, Longitudinal")){
      inps <- c(inps, list(selectInput(inputId = 'pf_visit', label = 'Visit Type',
                                       choices = vis)))
    }

    if(base_type %in% c("Multi Site, Anomaly Detection, Longitudinal",
                        "Single Site, Anomaly Detection, Longitudinal")){
      inps <- c(inps, list(selectInput(inputId = 'pf_domain', label = 'Domain',
                                       choices = d)))
    }

    if(grepl('Longitudinal', base_type) & base_type != 'Multi Site, Anomaly Detection, Longitudinal'){
      inps <- c(inps, list(selectInput(inputId = 'pf_time', label = 'Time Increment',
                                       choices = t)))
    }

    tagList(inps)
  })

  #### Create graph object ####
  pf_graph <- reactive({
    req(input$pf_type)
    base_type <- stringr::str_remove(input$pf_type, ' \\(.*')
    if(base_type %in% c("Multi Site, Anomaly Detection, Longitudinal",
                            "Single Site, Anomaly Detection, Longitudinal")){
      req(input$pf_domain)
    }
    if(base_type %in% c("Single Site, Anomaly Detection, Longitudinal",
                            "Multi Site, Anomaly Detection, Cross-Sectional",
                            "Multi Site, Anomaly Detection, Longitudinal")){
      req(input$pf_visit)
    }
    if(grepl('Single', input$pf_type)){
      req(input$pf_site_s)
    }
    if(grepl('Longitudinal', input$pf_type) & input$pf_type != 'Multi Site, Anomaly Detection, Longitudinal'){
      req(input$pf_time)
    }

    if(grepl('Single Site', input$pf_type) & !is.null(input$pf_site_s)){
      pfdf <- pf_dat() %>% dplyr::filter(site == input$pf_site_s)
    }else{
      pfdf <- pf_dat()
    }

    if(grepl('Longitudinal', input$pf_type) & !is.null(input$time)){
      pfdf <- pf_dat() %>% dplyr::filter(time_increment == input$pf_time)
    }else{
      pfdf <- pf_dat()
    }


      if(input$pf_val == "Median (All Patients)"){
        opval <- 'median_site_with0s'
      }else if(input$pf_val == "Median (Patients with a Fact)"){
        opval <- 'median_site_without0s'
      }else if(input$pf_val == "Count Anomalous Facts (Patients with a Fact)"){
        opval <- 'outlier_fact'
      }else if(input$pf_val == "Count Anomalous Facts (All Patients)"){
        opval <- 'outlier_tot'
      }else if(input$pf_val == "Proportion Anomalous Facts (Patients with a Fact)"){
        opval <- 'prop_outlier_fact'
      }else if(input$pf_val == "Proportion Anomalous Facts (All Patients)"){
        opval <- 'prop_outlier_tot'
      }else if(input$pf_val == "Median Fact Count"){
        opval <- 'median_fact_ct'
      }else if(input$pf_val == "Total Fact Count"){
        opval <- 'sum_fact_ct'
      }

    tryCatch(
      {
        grph <- pf_output(process_output = pfdf,
                          output = opval,
                          domain_filter = input$pf_domain,
                          visit_filter = input$pf_visit,
                          large_n = input$largen,
                          large_n_sites = input$pf_site_m)
      }, error = function(msg){
        return(grph <- ggplot2::ggplot() + ggplot2::geom_blank())
      }
    )

  })

  #### Build plot 1 ####
  output$pf_plt1 <- renderUI({
    base_type <- stringr::str_remove(input$pf_type, ' \\(.*')
    if(base_type == 'Single Site, Anomaly Detection, Cross-Sectional'){
      req(input$pf_val)
      req(input$pf_site_s)
    }
    if(base_type == 'Multi Site, Exploratory, Longitudinal'){
      req(input$pf_val)
      req(input$pf_time)
    }

    squba.diver::plt_engine_selector(plt_rctv = pf_graph(),
                                     n_plt = 1)

  })

  #### Build plot 2 ####
  output$pf_plt2 <- renderUI({

    squba.diver::plt_engine_selector(plt_rctv = pf_graph(),
                                     n_plt = 2)

  })

  #### Build plot 3 ####
  output$pf_plt3 <- renderUI({

    squba.diver::plt_engine_selector(plt_rctv = pf_graph(),
                                     n_plt = 3)

  })

  output$pf_dwnld <- squba.diver::plot_downloader(plt_obj = pf_graph,
                                                  id = 'pf')
}
