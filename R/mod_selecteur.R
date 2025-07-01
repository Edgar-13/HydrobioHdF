#' selecteur UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_selecteur_ui <- function(id, titre = "", texte, choix, choix_multiple = FALSE){
  select_choices <- c("", choix)

  if (length(names(choix)) == 0) {
    names(select_choices) <- c(texte, rep("", length(choix)))
  } else {
    names(select_choices) <- c(texte, names(choix))
  }

  ns <- NS(id)
  tagList(
    selectInput(
      inputId = ns("select"),
      label = titre,
      choices = select_choices,
      selected = "",
      multiple = choix_multiple
    ),
    actionButton(ns("appliquer"), "Appliquer",width = "100%")
  )
}

#' selecteur Server Functions
#'
#' @noRd
# mod_selecteur_server <- function(id){
#   moduleServer( id, function(input, output, session){
#     reactive({
#       choix <- input$select
#
#       # Si "Hauts-de-France (tout)" sélectionné → on sélectionne tous les départements HDF
#       if ("HDF" %in% choix) {
#         choix <- unique(c(choix, "02", "59", "60", "62", "80"))
#         choix <- setdiff(choix, "HDF")
#       }
#       if ("GE" %in% choix) {
#         choix <- unique(c(choix, "08", "51", "52", "55"))
#         choix <- setdiff(choix, "GE")
#       }
#       if ("IdF" %in% choix) {
#         choix <- unique(c(choix, "95", "77"))
#         choix <- setdiff(choix, "IdF")
#       }
#       choix
#     })
#   })
# }

mod_selecteur_server <- function(id){
  moduleServer(id, function(input, output, session){

    valeurs <- reactiveVal(NULL)

    observeEvent(input$appliquer, {
      choix <- input$select

      # Remplacer les codes spéciaux par les départements associés
      if ("HDF" %in% choix) {
        choix <- unique(c(choix, "02", "59", "60", "62", "80"))
        choix <- setdiff(choix, "HDF")
      }
      if ("GE" %in% choix) {
        choix <- unique(c(choix, "08", "51", "52", "55"))
        choix <- setdiff(choix, "GE")
      }
      if ("IdF" %in% choix) {
        choix <- unique(c(choix, "95", "77"))
        choix <- setdiff(choix, "IdF")
      }

      valeurs(choix)
    })

    return(valeurs)
  })
}

## To be copied in the UI
# mod_selecteur_ui("selecteur_ui_1")

## To be copied in the server
# mod_selecteur_server("selecteur_ui_1")
