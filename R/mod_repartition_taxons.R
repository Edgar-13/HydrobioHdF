#' repartition_taxons UI Function
#'
#' @description Module Shiny permettant d'afficher la répartition spatiale des taxons sur une carte interactive.
#' Le module génère une carte leaflet avec les stations où chaque taxon a été observé, avec des
#' fonctionnalités de recherche et de filtrage des taxons.
#'
#' @param id Internal parameter for {shiny}.
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @return Un objet tagList contenant une carte leaflet et des contrôles de recherche.
#'
#' @details Le module inclut des styles CSS personnalisés pour optimiser l'affichage de la carte
#' et des contrôles de recherche. La carte occupe la hauteur disponible de la fenêtre moins 200px.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom leaflet leafletOutput
mod_repartition_taxons_ui <- function(id){
  ns <- NS(id)

  css <- HTML(
    paste0(
      paste0("#", ns("carte_taxon"), " {margin-bottom:10px !important;height: calc(100vh - 200px) !important;}"),
      ".search-taxon {
            position: absolute;
            top: -5px;
            left: 100px;
          }

           .leaflet {
                margin-top:0px;
                padding:0px;
           }

           .leaflet-control-zoom, .leaflet-top, .leaflet-bottom {
           z-index: unset !important;
           }

           .leaflet-touch .leaflet-control-layers .leaflet-control-zoom .leaflet-touch .leaflet-bar {
           z-index: 10000000000 !important;
           }
          "
    )
  )

  tagList(
    tags$head(
      tags$style(css)
    ),
    column(
      width = 12,
      tags$div(
        class = "search-taxon",
        selectizeInput(
          inputId = ns("taxon"),
          label = "",
          choices = c(
            "Choisir un taxon" = ""
          ),
          multiple = FALSE
        )
      ),
      tags$div(
        class = "ma-carte",
        leaflet::leafletOutput(
        ns("carte_taxon"),
        width = '100%'
      )
      )
    )
  )
}

#' repartition_taxons Server Functions
#'
#' @noRd
#' @importFrom dplyr mutate select filter pull
#' @importFrom htmltools HTML
#' @importFrom leaflet renderLeaflet leaflet addMapPane addTiles WMSTileOptions providerTileOptions addPolygons pathOptions addWMSTiles addPolylines addLayersControl layersControlOptions fitBounds leafletProxy clearMarkers addCircleMarkers
#' @importFrom leaflet.extras addResetMapButton
#' @importFrom sf st_bbox
#' @importFrom shiny HTML
mod_repartition_taxons_server <- function(id, listes, choix_stations, choix_eqbs){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    BboxMap <- sf::st_bbox(listes)

    couleurs_etat <- c(
      `indéterminé` = "#CDC0B0",
      mauvais = "#EE2C2C",
      `médiocre` = "#FF7F00",
      moyen = "#FFC125",
      bon = "#A2CD5A",
      `très bon` = "#1874CD"
    )

    output$carte_taxon <- leaflet::renderLeaflet(
      leaflet::leaflet() %>%
        leaflet::addMapPane("background", zIndex = 400) %>%
        leaflet::addMapPane("masks", zIndex = 450) %>%
        leaflet::addMapPane("foreground", zIndex = 500) %>%
        leaflet::addTiles(map = .) %>%
        leaflet::addTiles("https://data.geopf.fr/wmts?service=WMTS&request=GetTile&version=1.0.0&tilematrixset=PM&tilematrix={z}&tilecol={x}&tilerow={y}&layer=ORTHOIMAGERY.ORTHOPHOTOS&format=image/jpeg&style=normal",
                          options = c(leaflet::WMSTileOptions(tileSize = 256),
                                      leaflet::providerTileOptions(minZoom = 1, maxZoom = 19)),
                          attribution='<a target="_blank" href="https://www.geoportail.gouv.fr/">Geoportail France</a>',
                          group = "Orthophoto"
        ) %>%
        addWMSTiles(
          baseUrl = "https://data.geopf.fr/wms-r/wms?",
          layers = "SCANREG_PYR-JPEG_WLD_WM",
          options = WMSTileOptions(
            format = "image/jpeg",
            transparent = FALSE,
            version = "1.3.0"
          ),
          attribution = '<a target="_blank" href="https://www.geoportail.gouv.fr/">Geoportail France</a>',
          group = "Scan Region"
        ) %>%
        leaflet::addPolygons(
          data = edl %>%
            dplyr::mutate(
              LABEL = paste0(NOM.MASSE.D.EAU, "<br>", ETAT.BIOLOGIQUE, " (", ANNEE, ")")
            ) %>%
            dplyr::select(LABEL, ETAT.BIOLOGIQUE),
          group = "Etat biologique",
          fillColor = ~unname(couleurs_etat[as.character(ETAT.BIOLOGIQUE)]),
          fillOpacity = .5,
          label = ~lapply(LABEL, htmltools::HTML),
          popup = NULL,
          weight = 1,
          options = leaflet::pathOptions(pane = "background")
        ) %>%
        addWMSTiles(
          baseUrl = "https://data.geopf.fr/private/wms-r?apikey=ign_scan_ws",
          layers = "SCAN25TOUR_PYR-JPEG_WLD_WM",
          options = WMSTileOptions(
            version = "1.3.0",
            format = "image/jpeg",
            transparent = FALSE
          ),
          attribution = "IGN",
          group = "SCAN25"
        )%>%
        leaflet::addWMSTiles(
          baseUrl = "https://services.sandre.eaufrance.fr/geo/topage",
          layers = "CoursEau_FXX",
          group = "Réseau hydrographique",
          options = leaflet::WMSTileOptions(
            pane = "masks",
            format = "image/png",
            transparent = TRUE,
            crs = 4326)
        ) %>%
        leaflet::addPolylines(
          data = limites_bassin_utiles_l,
          color = "black",
          opacity = 0.8,
          weight = 2.5,
          options = leaflet::pathOptions(pane = "masks")
        ) %>%
        leaflet::addPolylines(
          data = limites_dep_utiles_l,
          color = "#626669",
          opacity = 0.7,
          weight = 1,
          options = leaflet::pathOptions(pane = "masks")
        ) %>%
        # leaflet::addLabelOnlyMarkers(
        #   data = labels_df,
        #   lng = ~lng,
        #   lat = ~lat,
        #   label = ~label,
        #   labelOptions = labelOptions(noHide = TRUE)
        #   ) %>%
        leaflet::addLayersControl(
          baseGroups    = c("OSM","SCAN25","Orthophoto", "Scan Region", "Etat biologique"),
          overlayGroups = c("Réseau hydrographique"),
          options       = leaflet::layersControlOptions(collapsed = TRUE)
        ) %>%
        leaflet.extras::addResetMapButton() %>%
        leaflet::fitBounds(
          map = .,
          lng1 = BboxMap[["xmin"]],
          lat1 = BboxMap[["ymin"]],
          lng2 = BboxMap[["xmax"]],
          lat2 = BboxMap[["ymax"]]
        )
    )

    observe({
      req(choix_eqbs)

      eqbs <- choix_eqbs()
      if (is.null(eqbs))
        eqbs <- unique(listes$code_support)

      updateSelectizeInput(
        session = session,
        inputId = "taxon",
        choices = c(
          "Choisir un taxon" = "",
          listes %>%
            dplyr::filter(code_support %in% eqbs) %>%
            dplyr::pull(libelle_taxon) %>%
            unique()
        ),
        server = TRUE
      )

    })

    DonneesCarte <- reactive({
      listes %>%
      dplyr::filter(
        code_station_hydrobio %in% choix_stations(),
        libelle_taxon == input$taxon
      )
    })



    observe({
      req(choix_stations, input$taxon)

      BboxMap <- sf::st_bbox(
        DonneesCarte() %>%
          dplyr::filter(libelle_taxon == input$taxon)
        )

      leaflet::leafletProxy("carte_taxon") %>%
        leaflet::fitBounds(
          map = .,
          lng1 = BboxMap[["xmin"]],
          lat1 = BboxMap[["ymin"]],
          lng2 = BboxMap[["xmax"]],
          lat2 = BboxMap[["ymax"]]
        )

      if (nrow(
        DonneesCarte() %>%
        dplyr::filter(libelle_taxon == input$taxon)
        ) == 0) {
        leaflet::leafletProxy("carte_taxon") %>%
          leaflet::clearMarkers(map = .)
      } else {
        leaflet::leafletProxy("carte_taxon") %>%
          leaflet::clearMarkers(map = .) %>%
          leaflet::addCircleMarkers(
            map = .,
            data = DonneesCarte() %>%
              dplyr::filter(libelle_taxon == input$taxon),
            layerId = ~code_station_hydrobio,
            radius = 7,
            stroke = TRUE,
            color = "black",
            fillColor = "red",
            fillOpacity = 1,
            weight = 2,
            label = ~lapply(hover, shiny::HTML),
            options = pathOptions(pane = "foreground"),
            group = "all_stations"
          )
      }


    })

    repartition <- reactiveValues()

    observe({
      repartition$taxon <- input$taxon
      repartition$station <- input$carte_taxon_marker_click$id
      })

    observeEvent(repartition$station, {
      DonneesStation <- listes %>%
        dplyr::filter(
          libelle_taxon == repartition$taxon,
          code_station_hydrobio == repartition$station
          )

      CoordsStation <- DonneesStation %>%
        sf::st_centroid() %>%
        sf::st_coordinates()

      leaflet::leafletProxy("carte_taxon") %>%
        leaflet::clearGroup(
          group = "station_selected"
        ) %>%
        leaflet::clearGroup(
          group = "all_stations"
        ) %>%
        leaflet::addCircleMarkers(
          map = .,
          data = DonneesCarte() %>%
            dplyr::filter(libelle_taxon == input$taxon),
          layerId = ~code_station_hydrobio,
          radius = 7,
          stroke = TRUE,
          color = "black",
          fillColor = "red",
          fillOpacity = 1,
          weight = 2,
          label = ~lapply(hover, shiny::HTML),
          options = pathOptions(pane = "foreground"),
          group = "all_stations"
        ) %>%
        leaflet::addCircleMarkers(
          data = DonneesStation,
          layerId = ~code_station_hydrobio,
          radius = 7,
          stroke = TRUE,
          color = "black",
          fillColor = c("#6495ED"),
          fillOpacity = 1,
          weight = 2,
          label = ~lapply(hover, shiny::HTML),
          options = pathOptions(pane = "foreground"),
          group = "station_selected"
        ) %>%
        leaflet::setView(
          lng = unname(CoordsStation[,"X"]),
          lat = unname(CoordsStation[,"Y"]),
          zoom = input$carte_taxon_zoom
        )
    })


    return(repartition)

  })
}

## To be copied in the UI
# mod_repartition_taxons_ui("repartition_taxons_1")

## To be copied in the server
# mod_repartition_taxons_server("repartition_taxons_1")
