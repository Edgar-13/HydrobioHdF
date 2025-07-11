#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

library(webshot2)

app_server <- function(input, output, session) {
  # Your application server logic

  # Télécharge et charge dans l'espace de travail les données: "donnees_carte",
  # "donnees_carte_taxons", "indices", "listes_taxo", "resumes_listes",
  # "stations", "acronymes_indices", "date_donnees"
  mod_load_data_server("donnees")

  #load_data_hydrobio()

  library(sf)

  choix_departements <- mod_selecteur_server(id = "departements")
  choix_eqbs <- mod_selecteur_server(id = "eqb")
  choix_stations <- mod_regie_server(id = "regie", choix_eqb = choix_eqbs, choix_dep = choix_departements)

  station <- mod_carte_server(
    "carte",
    donnees_carte = donnees_carte,
    choix_stations = choix_stations
    )

  ordre_taxon <- mod_selecteur_ordre_taxons_server(
    id = "ordre_taxons",
    choix_station = station,
    choix_eqb = choix_eqbs
    )

  mod_synthese_toutes_stations_server(
    id = "bilan_stations",
    stations = stations,
    indices = etat_bio,
    choix_stations = choix_stations,
    choix_eqb = choix_eqbs
  )

  mod_synthese_station_server(
    id = "synthese_station",
    resumes_listes = resumes_listes,
    stations = stations,
    regie = regie,
    indices = indices,
    acronymes_indices = acronymes_indices,
    valeurs_seuils_stations = valeurs_seuils_stations,
    parametres_eqr = parametres_eqr,
    etat_bio = etat_bio,
    listes_taxo = listes_taxo,
    choix_station = station,
    choix_eqb = choix_eqbs,
    ordre_taxon = ordre_taxon,
    choix_stations = choix_stations
    )


  repartition <- mod_repartition_taxons_server(
    id = "carte_taxons",
    listes = donnees_carte_taxons,
    choix_stations = choix_stations,
    choix_eqbs = choix_eqbs
  )

  mod_synthese_taxon_server(
    id = "synthese_taxon",
    repartition = repartition,
    choix_stations = choix_stations
  )

  mod_export_print_server("export_print")

  # observeEvent(input$print_trigger, {
  #   leafletProxy("carte-carte_op")$invalidateSize()
  #   leafletProxy("carte_taxons-carte_taxon")$invalidateSize()
  # })

}
