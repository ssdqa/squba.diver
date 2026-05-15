
#' General UI setup
#'
#' @param id xx
#'
#' @returns xx
#'
genUI <- function(id){
  #ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = stringr::str_c(id, '_type'),
                  label = 'Check Type',
                  choices = NULL),
      uiOutput(stringr::str_c(id, '_selects')),
      hr(),
      downloadButton(stringr::str_c(id, '_dwnld'),
                     label = 'Download Plots'),
      hr(),
      textAreaInput(inputId = stringr::str_c(id, '_note'),
                    label = 'Notes',
                    placeholder = 'Thoughts & takeaways from the graph'),
      actionButton(inputId = stringr::str_c(id, '_save_note'),
                   icon = icon('floppy-disk'),
                   class = 'btn-primary',
                   label = 'Save Note')
    ),

    mainPanel(
      add_busy_spinner(position = 'bottom-right',
                       spin = 'circle',
                       height = '100px',
                       width = '100px'),
      uiOutput(stringr::str_c(id, '_plt1')),
      uiOutput(stringr::str_c(id, '_plt2')),
      uiOutput(stringr::str_c(id, '_plt3'))
    )
  )
}
