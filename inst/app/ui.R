#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/

##' `Start NavBar Page`

navbarPage(
  title = 'SQUBA Visualizer',
  id = 'nav',
  theme = bslib::bs_theme(preset = 'lumen'),

  #' `Apply Styling`
  tags$style(HTML(".navbar {background-color: #6eb4f5 !important;}
                   hr {border-top: 1px solid #000000;}
                   input[type='checkbox']:checked {background-color: #a60275 !important;}")),

  #' `Home Page`
  tabPanel('Home',
           value = 1,
           id = 'home',
           column(
             width = 12,
             align = 'center',
             uiOutput('homeui'),
             shinydashboard::box(
               title = 'Switch to Large N View',
               solidHeader = TRUE,
               background = 'blue',
               checkboxInput(inputId = 'largen',
                             label = 'View Large N'))
             )
           ),

  #' `Start Menu`
    navbarMenu(title = 'Module Selection',
               menuName = 'navbar_ref'),

  #' `Notes Tab`
  tabPanel('Notes',
           value = 99,
           id = 'notes',
           icon = icon('note-sticky'),
           gt::gt_output('notes_gt'),
           column(
             width = 12,
             downloadButton(outputId = 'notes_download',
                            label = 'Download Notes',
                            class = 'btn-primary',
                            icon = icon('download')),
             align = 'center'
           ))

)
