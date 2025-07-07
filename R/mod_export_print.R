#' UI pour le module d'export / impression
#' @param id id du module
#' @return UI à inclure dans l'app
#' @export
mod_export_print_ui <- function(id) {
  ns <- NS(id)

  # CSS impression
  css <- HTML("
        @media print {

          .nav-tabs > li.active > a,
          .nav-pills > li.active > a {
            background-color: black !important;  /* couleur de surbrillance */
            color: black !important;                /* texte blanc */
            border: 3px solid #ddd !important;
          }

          /* Forcer fond blanc et texte noir sur la zone selectize */
          .search-station .selectize-input,
          .search-taxon .selectize-input {
            background-color: white !important;
            color: black !important;
            border: 1px solid #ccc !important;
          }

          /* Aussi le texte dans l'input */
          .search-station .selectize-input input,
          .search-taxon .selectize-input input {
            background-color: white !important;
            color: black !important;
          }

          .ma-carte {
            width: 800px !important;
            height: 800px !important;
/*
            position: fixed !important;
            top: 50% !important;
            left: 50% !important;
            transform: translate(-50%, -50%) !important;

            /* Optionnel : ajouter une bordure ou un fond */
            border: 1px solid #ccc !important;
            background-color: white !important;*/
          }

          /* masque les contrôles Leaflet pour un rendu propre à l'impression */
          .leaflet-control-container {
            display: none !important;
          }

          .no-print {
            display: none !important;
          }

          .mainPanel {
            width: 100% !important;
          }

          .page-break {
            page-break-after: always;
            break-after: page;  /* plus moderne */
          }

          .print-full-height {
            overflow: visible !important;
            height: auto !important;
            max-height: none !important;
          }

          page {
            margin: 1.5cm;
          }
        }"
      )


  tagList(
    tags$head(
      tags$style(css)
    ),

    actionButton(ns("print_btn"), "Exporter / imprimer la page", class = "no-print"),

    # Script JS pour imprimer
    tags$script(
      HTML(sprintf("
        document.addEventListener('DOMContentLoaded', function() {
          var printButton = document.getElementById('%s');
          if (printButton) {
            printButton.addEventListener('click', function() {
              window.print();
            });
          }
        });
      ", ns("print_btn")))
    ),
    tags$script(
      HTML("
    window.onbeforeprint = function() {
      Shiny.onInputChange('print_trigger', new Date());
    };
  ")
    )
  )
 }

#' Server pour le module d'export / impression
#' (vide ici car tout se fait côté client)
#'
#' @param id id du module
#' @export
mod_export_print_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Rien à faire côté serveur
  })
}
