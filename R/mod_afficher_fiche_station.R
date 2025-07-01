#' afficher_fiche_station UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
mod_afficher_fiche_station_ui <- function(id){
  ns <- NS(id)
  tagList(
    htmlOutput(ns("FicheStationSN")),
    htmlOutput(ns("FicheStationAP"))
  )
}

#' afficher_fiche_station Server Functions
#'
#' @noRd

mod_afficher_fiche_station_server <- function(id, stations, choix_station, choix_eqb){
  moduleServer(id, function(input, output, session){
    ns <- session$ns



    # eventReactive qui va chercher et préparer les chemins des fichiers au clic du bouton
    fiche_fichiers <- reactive({

      req(choix_station())
      code_station <- choix_station()

      base_path <- "R:/ServicesRegionaux/Service_Connaissance/7-Laboratoire_hydrobiologie/Donnees/Referentiel_stations/Fiches_stations"

      dossiersSN <- c("Fiches_stations_DR_OFB_HdF/DVM","Fiches_stations_DR_OFB_HdF/DVO")

      aeap_types <- c("Stations", "Diatomees", "Invertebres", "Macrophytes", "Poissons")
      dossiersAP <- sapply(aeap_types, function(type) {
        matches <- Sys.glob(file.path(base_path, "*Fiches_stations_AEAP*", paste0("*", type, "*")))
        # Garder uniquement ceux qui sont des dossiers
        dirs <- matches[file.info(matches)$isdir]
        if (length(dirs) > 1) {
          warning(sprintf("Plusieurs dossiers trouvés pour le type '%s' : %s", type, paste(dirs, collapse = ", ")))
        }
        if (length(dirs) > 0) {
          return(dirs[1])  # on prend le premier dossier trouvé
        } else {
          return(NA)
        }
      })

      # Vérification des manquants
      if (any(is.na(dossiersAP))) {
        warning("Un ou plusieurs types AEAP manquants : ", paste(aeap_types[is.na(dossiersAP)], collapse = ", "))
      }

      fichierSN_trouve <- NULL
      repertoire_trouve <- NULL

      for (dossier in dossiersSN) {
        path <- file.path(base_path, dossier)
        fichiers <- list.files(path, pattern = paste0(code_station,".*\\.pdf$"), full.names = FALSE, ignore.case = TRUE)
        if (length(fichiers) > 0) {
          fichierSN_trouve <- fichiers[1]
          repertoire_trouve <- dossier
          addResourcePath(repertoire_trouve, file.path(base_path, repertoire_trouve))
          break
        }
      }

      if (is.null(fichierSN_trouve)) {
        for (type in names(dossiersAP)) {
          path <- dossiersAP[[type]]
          if (is.na(path) || path == "") {
            # Ignorer ce type s'il n'y a pas de dossier trouvé
            next
          }
          fichiers <- list.files(path, pattern = paste0(substr(code_station, 3, nchar(code_station)), ".*\\.pdf$"), full.names = FALSE, ignore.case = TRUE)
          if (length(fichiers) > 0) {
            alias <- basename(dossiersAP[[type]])
            addResourcePath(alias, dossiersAP[[type]])
            dossiersAP[[type]] <- file.path(path, fichiers[1])  # mettre le chemin complet vers le fichier PDF
          }
        }
      }

      if (any(sapply(dossiersAP, function(x) { #any Renvoie TRUE si au moins un élément est un fichier PDF.
        if (is.na(x)) {
          FALSE  # NA n'est pas un fichier PDF
        } else {
          grepl("\\.pdf$", basename(x), ignore.case = TRUE)
        }
      }))){localisation = "AP"}
      else if (!is.null(fichierSN_trouve)){localisation = "SN"}
      else {localisation = "SN_sans_fiche"}

      list(
        fichierSN = fichierSN_trouve,
        repSN = repertoire_trouve,
        dossiersAP = dossiersAP,
        localisation = localisation
      )
    })

    output$FicheStationSN <- renderUI({
      req(fiche_fichiers())
      fiche <- fiche_fichiers()
      if (fiche$localisation == "SN"){
        tags$a(href = file.path(fiche$repSN, fiche$fichierSN), target = "_blank", "Fiche station")
      } else if (fiche$localisation == "SN_sans_fiche"){
        tags$h5("Pas de fiche station en base")
      }
    })

    output$FicheStationAP <- renderUI({
      req(fiche_fichiers())
      fiche <- fiche_fichiers()
      if (fiche$localisation=="AP") {
        # Crée une liste vide pour stocker les balises UI
        ui_elements <- list()
        for (type in names(fiche$dossiersAP)) {
          path <- file.path(basename(dirname(fiche$dossiersAP[[type]])),basename(fiche$dossiersAP[[type]]))
          if (is.na(fiche$dossiersAP[[type]])){ui_elements[[length(ui_elements) + 1]] <- tags$h5("Problème d'accès au répertoire ", type)}
          else if (grepl("\\.pdf$", path, ignore.case = TRUE)) {
            ui_elements[[length(ui_elements) + 1]] <- tags$h5(tags$a(href = path, target = "_blank", paste("Fiche", type)))
          } else{
            ui_elements[[length(ui_elements) + 1]] <- tags$h5(paste("Pas de fiche", type, "en base"))
           }
        }

          # Retourne tous les éléments dans un bloc unique
          return(tagList(ui_elements))
        }
      else {return(NULL)}
    })
  })
}



## To be copied in the UI
# mod_afficher_fiche_station_ui("afficher_fiche_station_1")

## To be copied in the server
# mod_afficher_fiche_station_server("afficher_fiche_station_1")
