#' chiffres_cles_station UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
mod_chiffres_cles_station_ui <- function(id){
  ns <- NS(id)
  tagList(
    htmlOutput(ns("titre")),
    # htmlOutput(ns("FicheStation")),
    tableOutput(ns("tableau"))
  )
}

#' chiffres_cles_station Server Functions
#'
#' @noRd
mod_chiffres_cles_station_server <- function(id, resumes_listes, stations, choix_station, choix_eqb){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #Obtenir le code station et chercher un fichier qui contient ce code (soit dans le nom soit à l'intérieur du fichier)
    #3 repertoires différents : AEAP/DVM/DVO
    # Dossier de base
    # base_path <- "R:/ServicesRegionaux/Service_Connaissance/7-Laboratoire_hydrobiologie/Donnees/Referentiel_stations/Fiches_stations/Fiches_stations_DR_OFB_HdF"
    #
    # dossiers <- c("AEAP","DVM","DVO")
    # code_station <- choix_station()
    # print(paste0("code station est :",code_station))
    # fichier_trouve <- NULL
    # repertoire_trouve <- NULL
    #
    # for (dossier in dossiers) {
    #   path <- file.path(base_path, dossier)
    #   fichiers <- list.files(path, pattern = paste0(code_station,".*\\.pdf$"), full.names = FALSE, ignore.case = TRUE)
    #
    #   if (length(fichiers) >0) {
    #     fichier_trouve <- fichiers[1]
    #     repertoire_trouve <- dossier
    #     addResourcePath(repertoire_trouve, file.path(base_path, repertoire_trouve))
    #     break # On s'arrête dès qu'on trouve un match
    #   }
    # }
    #
    observe({

        resume_listes <- resumes_listes %>%
          filtrer_resumes(choix_station(), choix_eqb()) %>%
          dplyr::select(-code_station_hydrobio, -code_support)

        output$titre <- renderUI({
          with(
            filtrer_station(stations, choix_station()),
            HTML(paste0('<a href="', uri_station_hydrobio, '" target="_blank" title="Fiche station sandre"><b>',
                        libelle_station_hydrobio, '</b></a>'))
          )
        })
    #
    #     print(paste0("fichie_trouve est : ",fichier_trouve))
        # output$FicheStation <-
        #   renderUI({
        #     if (!is.null(fichier_trouve)){
        #     tags$a(href = file.path(repertoire_trouve,fichier_trouve), target = "_blank", "Fiche station")
        #    }
        #
        # else {
        #   tags$h5("Pas de fiche station en base")}
        #   })
        output$tableau <- renderTable(resume_listes)

    })
  })
}

## To be copied in the UI
# mod_chiffres_cles_station_ui("chiffres_cles_station_1")

## To be copied in the server
# mod_chiffres_cles_station_server("chiffres_cles_station_1")
