
#' Server backend for EVP module
#'
#' @param id x
#' @param data x
#' @param input x
#' @param output x
#'
#' @returns xx
#'
#' @import shiny
#'
evp_server <- function(id, data, input, output) {

    #### Load check-specific data ####
    evp_dat <- reactive({
      load_check_data(dat_list = data(),
                                   input = input,
                                   type_input = 'evp_type')
    })

    #### Build sidebar selectors ####
    output$evp_selects <- renderUI({

      inps <- list()

      if('site' %in% colnames(evp_dat())){
        bloop <- evp_dat() %>% dplyr::distinct(site) %>% dplyr::pull()
      }else(bloop <- evp_dat() %>% dplyr::distinct(grp) %>% dplyr::pull())

      if('time_increment' %in% colnames(evp_dat())){
        t <- evp_dat() %>% dplyr::distinct(time_increment) %>% dplyr::pull()
      }else{t <- NULL}

      if('variable' %in% colnames(evp_dat())){
        v <- evp_dat() %>% distinct(variable) %>% pull()
      }else{v <- NULL}

      ## Site
      ## Variable
      ## Analysis Level
      ## Time Increment

      base_type <- stringr::str_remove(input$evp_type, ' \\(.*')

      if(!grepl('Single Site', input$evp_type) & input$largen){
        inps <- c(inps, list(selectizeInput(inputId = 'evp_site_m', label = 'Site',
                                            choices = bloop, multiple = TRUE)))
      }else if(grepl('Single Site', input$evp_type)){
        inps <- c(inps, list(selectInput(inputId = 'evp_site_s', label = 'Site',
                                         choices = bloop)))
        }

      if(base_type == "Multi Site, Exploratory, Longitudinal"){
        inps <- c(inps, list(selectizeInput(inputId = 'evp_var_m', label = 'Variable(s)',
                                            choices = v, multiple = TRUE, selected = v[1])))
      }else if(base_type == "Multi Site, Anomaly Detection, Longitudinal" |
               base_type == "Single Site, Anomaly Detection, Longitudinal"){
        inps <- c(inps, list(selectInput(inputId = 'evp_var_s', label = 'Variable',
                                         choices = v)))
      }

      if(base_type != "Single Site, Anomaly Detection, Cross-Sectional" &
         base_type != "Multi Site, Anomaly Detection, Longitudinal"){
        inps <- c(inps, list(selectInput(inputId = 'evp_val', label = 'Analysis Level',
                                    choices = c('patient', 'row'), selected = 'patient')))
      }

      if(base_type == "Single Site, Exploratory, Longitudinal" |
         base_type == "Multi Site, Exploratory, Longitudinal" |
         base_type == "Single Site, Anomaly Detection, Longitudinal"){
        inps <- c(inps, list(selectInput(inputId = 'evp_time', label = 'Time Increment',
                                         choices = t)))
      }

      tagList(inps)
    })

    #### Create graph object ####
    evp_graph <- reactive({
      req(input$evp_type)
      base_type <- stringr::str_remove(input$evp_type, ' \\(.*')

      if(base_type == 'Multi Site, Exploratory, Longitudinal'){
        req(input$evp_var_m)
      }
      if(base_type == 'Single Site, Anomaly Detection, Longitudinal'){
        req(input$evp_time)
      }

      if(base_type == 'Multi Site, Exploratory, Longitudinal'){
        var_input <- input$evp_var_m
      }else{
        var_input <- input$evp_var_s
      }

      if(is.null(var_input)){var_input <- 'test'}

      if(base_type == 'Single Site, Anomaly Detection, Cross-Sectional' & !is.null(input$evp_site_s)){
        evpdf <- evp_dat() %>% dplyr::filter(grp == input$evp_site_s)
      }else if(base_type != 'Single Site, Anomaly Detection, Cross-Sectional' & grepl('Single Site', base_type) &
               !is.null(input$evp_site_s)){
        evpdf <- evp_dat() %>% dplyr::filter(site == input$evp_site_s)
      }else{
        evpdf <- evp_dat()
      }

      if(is.null(input$evp_val)){
        val_new <- 'patient'
      }else(val_new <- input$evp_val)

      tryCatch(
        {grph <- evp_output(process_output = evpdf,
                            output_level = val_new,
                            filter_variable = var_input,
                            large_n = input$largen,
                            large_n_sites = input$evp_site_m)
        }, error = function(msg){
          return(grph <- ggplot2::ggplot() + ggplot2::geom_blank())
        }
      )

    })

    #### Build plot 1 ####
    output$evp_plt1 <- renderUI({
      base_type <- stringr::str_remove(input$evp_type, ' \\(.*')
      if(base_type == 'Single Site, Anomaly Detection, Longitudinal'){
        req(input$evp_time)
        req(input$evp_var_s)
      }
      if(base_type == 'Multi Site, Anomaly Detection, Longitudinal'){
        req(input$evp_var_s)
      }

      plt_engine_selector(plt_rctv = evp_graph(),
                                       n_plt = 1)

    })

    #### Build plot 2 ####
    output$evp_plt2 <- renderUI({
      base_type <- stringr::str_remove(input$evp_type, ' \\(.*')
      if(base_type == 'Single Site, Anomaly Detection, Longitudinal'){
        req(input$evp_time)
      }
      if(base_type == 'Multi Site, Anomaly Detection, Longitudinal'){
        req(input$evp_var_s)
      }

      plt_engine_selector(plt_rctv = evp_graph(),
                                       n_plt = 2)

    })

    #### Build plot 3 ####
    output$evp_plt3 <- renderUI({
      base_type <- stringr::str_remove(input$evp_type, ' \\(.*')
      if(base_type == 'Multi Site, Anomaly Detection, Longitudinal'){
        req(input$evp_var_s)
      }

      plt_engine_selector(plt_rctv = evp_graph(),
                                       n_plt = 3)

    })

    output$evp_dwnld <- plot_downloader(plt_obj = evp_graph,
                                                     id = 'evp')
}
