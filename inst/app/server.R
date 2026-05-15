#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  #### HOME TAB ####
    ## Build home page UI
    output$homeui <- renderUI({
      
      box(title = h1('Welcome to the SQUBA Visualizer!'),
          h4(HTML(paste0('Your data quality analysis consisted of results from <span style="color: blue;"><b>', squba_input$metadata$total_mod, 
                    '</b></span> SQUBA modules for a total of <span style="color: blue;"><b>', squba_input$metadata$total_check, 
                    '</b></span> checks'))),
          p('To start exploring your output, navigate to the "Module Selection" tab in the navigation bar 
             and select a module.'),
          p('Each check will default to the standard visualization option. If you would prefer to view the
            "large N" graphs, check the box below. This selection will be applied across the whole interface.'))
      
    })
    
    #### TAB MENU RENDER ####
    observe({
      for (m in 1:length(squba_reg)) {
        dat_type <- squba_reg[[m]]$id
        if (!is.null(squba_input[[dat_type]])){
          appendTab(inputId = 'nav',
                    menuName = 'navbar_ref',
                    tab = tabPanel(title = squba_reg[[m]]$name,
                                   id = squba_reg[[m]]$id,
                                   value = squba_reg[[m]]$ref_val,
                                   genUI(squba_reg[[m]]$id)))
        }
      }
    })
    
    notes_df <- reactiveValues(note_tbl = tibble(date = format(Sys.Date(), "%m/%d/%y"),
                                                 time = format(Sys.time(), "%H:%M:%S"),
                                                 module = 'Sample Module',
                                                 check = 'type goes here',
                                                 large_n = TRUE,
                                                 site = 'institution name goes here',
                                                 time_period = 'time period goes here',
                                                 other_input = 'any other relevant input goes here',
                                                 notes = 'actual note goes here'))
    
    #### Load CA server mod ####
    observe({
      if(!is.null(squba_input$ca)){
        update_type_input(dat_list = squba_input$ca,
                          ck_str = 'ca')
        ca_server('ca', reactive(squba_input$ca), input, output)
        observeEvent(input$ca_save_note, {
          
          notes_df$note_tbl <- render_update_notes(notes_rctval = notes_df$note_tbl,
                                                   id = 'ca',
                                                   mod_name = 'Cohort Attrition',
                                                   other_inps = str_c('- ', input$ca_val, '\n- Log = ', input$ca_log),
                                                   input = input)
          
          updateTextAreaInput(inputId = 'ca_note',
                              value = character(0))
          
          showNotification(list(icon('circle-check'), " Saved"), type = 'message')
        })
      }
    })
    
    #### Load EVP server mod ####
    observe({
      if(!is.null(squba_input$evp)){
        update_type_input(dat_list = squba_input$evp,
                          ck_str = 'evp')
        evp_server('evp', reactive(squba_input$evp), input, output)
        observeEvent(input$evp_save_note, {
          
          if(grepl("Multi Site, Exploratory, Longitudinal", input$evp_type)){
            vars <- paste0(input$evp_var_m, collapse = ', ')
          }else if(grepl("Multi Site, Anomaly Detection, Longitudinal", input$evp_type) |
                   grepl("Single Site, Anomaly Detection, Longitudinal", input$evp_type)){
            vars <- input$evp_var_s
          }else{vars <- NULL}
          
          notes_df$note_tbl <- render_update_notes(notes_rctval = notes_df$note_tbl,
                                                   id = 'evp',
                                                   mod_name = 'Expected Variables Present',
                                                   other_inps = ifelse(is.null(vars), input$evp_val,
                                                                       str_c('- ', input$evp_val, '\n- ', vars)),
                                                   input = input)
          
          updateTextAreaInput(inputId = 'evp_note',
                              value = character(0))
          
          showNotification(list(icon('circle-check'), " Saved"), type = 'message')
        })
      }
    })
    
    #### Load PF server mod ####
    observe({
      if(!is.null(squba_input$pf)){
        update_type_input(dat_list = squba_input$pf,
                          ck_str = 'pf')
        pf_server('pf', reactive(squba_input$pf), input, output)
        observeEvent(input$pf_save_note, {
          
          base_type <- str_remove(input$pf_type, ' \\(.*')
          
          if(!base_type %in% c('Multi Site, Anomaly Detection, Cross-Sectional',
                               'Multi Site, Anomaly Detection, Longitudinal',
                               'Single Site, Anomaly Detection, Longitudinal')){
            vl <- paste0('- ', input$pf_val)
          }else{vl <- NULL}
          
          if(base_type %in% c("Single Site, Anomaly Detection, Longitudinal", 
                              "Multi Site, Anomaly Detection, Cross-Sectional", 
                              "Multi Site, Anomaly Detection, Longitudinal")){
            vis <- paste0('\n- ', input$pf_visit)
          }else{vis <- NULL}
          
          if(base_type %in% c("Multi Site, Anomaly Detection, Longitudinal", 
                              "Single Site, Anomaly Detection, Longitudinal")){
            dom <- paste0('\n- ', input$pf_domain)
          }else{dom <- NULL}
          
          
          notes_df$note_tbl <- render_update_notes(notes_rctval = notes_df$note_tbl,
                                                   id = 'pf',
                                                   mod_name = 'Patient Facts',
                                                   other_inps = str_c(vl, vis, dom),
                                                   input = input)
          
          updateTextAreaInput(inputId = 'pf_note',
                              value = character(0))
          
          showNotification(list(icon('circle-check'), " Saved"), type = 'message')
        })
      }
    })
    
    #### Notes ####
    output$notes_gt <- render_gt({
      notes_df$note_tbl %>%
        filter(module != 'Sample Module') %>%
        arrange(desc(date), desc(time)) %>%
        gt() %>%
        cols_label('date' = 'Date',
                   'time' = 'Time',
                   'module' = 'Module',
                   'check' = 'Check Type',
                   'large_n' = 'Large N?',
                   'site' = 'Site',
                   'time_period' = 'Time Period',
                   'other_input' = 'Other Inputs',
                   'notes' = 'Note') %>%
        opt_interactive() %>%
        fmt_markdown() %>%
        sub_missing(missing_text = '')
      })
    
    output$notes_download <- downloadHandler(
      filename = 'paqs_squba_notes.csv',
      content = function(file) {
        readr::write_csv(notes_df$note_tbl %>%
                           filter(module != 'Sample Modules'), file)
      }
    )

}
