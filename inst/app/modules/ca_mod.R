
ca_server <- function(id, data, input, output) {

  #### Load check-specific data ####
  ca_dat <- reactive({
    load_check_data(dat_list = data(),
                    input = input,
                    type_input = 'ca_type')
  })

  #### Build sidebar selectors ####
  output$ca_selects <- renderUI({

    inps <- list()

    if('site' %in% colnames(ca_dat())){
      bloop <- ca_dat() %>% dplyr::distinct(site) %>% dplyr::pull()
    }

    ## Site
    ## Analysis Level
    ## Log Toggle

    if(!grepl('Single Site', input$ca_type) & input$largen){
      inps <- c(inps, list(selectizeInput(inputId = 'ca_site_m', label = 'Site',
                                          choices = bloop, multiple = TRUE)))
    }else if(grepl('Single Site', input$ca_type)){
      inps <- c(inps, list(selectInput(inputId = 'ca_site_s', label = 'Site',
                                       choices = bloop)))
    }

    inps <- c(inps, list(selectInput(inputId = 'ca_val', label = 'Analysis Level',
                                     choices = c('Number of Patients', 'Proportion Retained (Starting Step)',
                                                 'Proportion Retained (Prior Step)', 'Percent Difference (Prior Step)'),
                                     selected = 'Number of Patients')))

    if(!grepl('Multi Site, Anomaly Detection, Cross-Sectional', input$ca_type)){
      inps <- c(inps, list(checkboxInput(inputId = 'ca_log', label = 'Log Scale?')))
    }

    tagList(inps)
  })

  #### Create graph object ####
  ca_graph <- reactive({
    req(input$ca_type)
    req(input$ca_val)
    if(grepl('Single', input$ca_type)){
      req(input$ca_site_s)
    }

    if(input$ca_val == 'Number of Patients'){
      type_col <- 'num_pts'
    }else if(input$ca_val == 'Proportion Retained (Starting Step)'){
      type_col <- 'prop_retained_start'
    }else if(input$ca_val == 'Proportion Retained (Prior Step)'){
      type_col <- 'prop_retained_prior'
    }else if(input$ca_val == 'Percent Difference (Prior Step)'){
      type_col <- 'prop_diff_prior'
    }


    if(grepl('Single Site', input$ca_type) & !is.null(input$ca_site_s)){
      cadf <- ca_dat() %>% dplyr::filter(site == input$ca_site_s)
    }else{
      cadf <- ca_dat()
    }

    tryCatch(
      {grph <- ca_output(process_output = cadf,
                         log_scale = input$ca_log,
                         var_col = type_col,
                         large_n = input$largen,
                         large_n_sites = input$ca_site_m)
      }, error = function(msg){
        return(grph <- ggplot2::ggplot() + ggplot2::geom_blank())
      }
    )

  })

  #### Build plot 1 ####
  output$ca_plt1 <- renderUI({

    plt_engine_selector(plt_rctv = ca_graph(),
                        n_plt = 1)

  })

  #### Build plot 2 ####
  output$ca_plt2 <- renderUI({

    plt_engine_selector(plt_rctv = ca_graph(),
                        n_plt = 2)

  })

  #### Build plot 3 ####
  output$ca_plt3 <- renderUI({

    plt_engine_selector(plt_rctv = ca_graph(),
                        n_plt = 3)

  })

  output$ca_dwnld <- plot_downloader(plt_obj = ca_graph,
                                     id = 'ca')
}
