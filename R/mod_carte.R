#' carte UI Function
#'
#' @description Module Shiny permettant d'afficher une carte interactive des stations de suivi hydrobiologique.
#' La carte permet de visualiser la localisation des stations, leur état écologique et
#' le nombre d'années de suivi. Elle inclut également une fonction de recherche de station.
#'
#' @param id Internal parameter for {shiny}.
#' @param hauteur Hauteur de la carte en pixels.
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom leaflet leafletOutput
mod_carte_ui <- function(id, hauteur){
  ns <- NS(id)

  css <- HTML(
    paste0(
      paste0("#", ns("carte_op"), " {margin-bottom:10px !important;}"),
      ".search-station {
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
        class = "search-station",
        selectizeInput(
          inputId = ns("station"),
          label = "",
          choices = c(
            "Localiser une station" = ""
          ),
          multiple = FALSE
        )
      ),
      leaflet::leafletOutput(
        ns("carte_op"),
        width = '100%',
        height = hauteur
      )
    )

  )
}

#' carte Server Functions
#'
#' @noRd
#' @importFrom dplyr mutate select
#' @importFrom htmltools HTML
#' @importFrom leaflet renderLeaflet leaflet addMapPane addTiles WMSTileOptions providerTileOptions addPolygons pathOptions addPolylines addLayersControl layersControlOptions fitBounds
#' @importFrom leaflet.extras addResetMapButton
#' @importFrom sf st_bbox
#' @importFrom dplyr `%>%`
mod_carte_server <- function(id, donnees_carte, choix_stations){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    radius_pal <- function(x) {
      approx(
        x = sqrt(range(donnees_carte$nb_annees, na.rm = TRUE)),
        y = c(5, 10),
        xout = sqrt(x),
        yleft = 5,
        yright = 10
      )$y
    }

    BboxMap <- sf::st_bbox(donnees_carte)

    couleurs_etat <- c(
      `indéterminé` = "#CDC0B0",
      mauvais = "#EE2C2C",
      `médiocre` = "#FF7F00",
      moyen = "#FFC125",
      bon = "#A2CD5A",
      `très bon` = "#1874CD"
    )

    output$carte_op <- leaflet::renderLeaflet(
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

    DonneesCarte <- reactive({
      req(choix_stations)

      donnees_carte %>%
        dplyr::filter(
          code_station_hydrobio %in% choix_stations()
        ) %>%
        dplyr::group_by(code_station_hydrobio, libelle_station_hydrobio) %>%
        dplyr::summarise(
          derniers_resultats = paste(derniers_resultats, collapse = "<br>"),
          nb_annees = max(nb_annees),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          hover = paste0(
            "<b>", libelle_station_hydrobio, "</b><br><br>",
            derniers_resultats
          )
        )
    })
    observe({
      req(choix_stations)

      updateSelectizeInput(
        session = session,
        inputId = "station",
        choices = c(
          "Localiser une station" = "",
          DonneesCarte()$libelle_station_hydrobio
        ),
        server = TRUE
      )

      BboxMap <- sf::st_bbox(DonneesCarte())

      leaflet::leafletProxy("carte_op") %>%
        leaflet::fitBounds(
          map = .,
          lng1 = BboxMap[["xmin"]],
          lat1 = BboxMap[["ymin"]],
          lng2 = BboxMap[["xmax"]],
          lat2 = BboxMap[["ymax"]]
        )


      if (nrow(DonneesCarte()) == 0) {
        leaflet::leafletProxy("carte_op") %>%
          leaflet::clearMarkers(map = .)
      } else {
        leaflet::leafletProxy("carte_op") %>%
          leaflet::clearMarkers(map = .) %>%
          leaflet::addCircleMarkers(
            map = .,
            data = DonneesCarte(),
            layerId = ~code_station_hydrobio,
            radius = ~radius_pal(nb_annees),
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

      observe({

        if (input$station != "") {

          CoordsStation <- DonneesCarte() %>%
            dplyr::filter(libelle_station_hydrobio == input$station) %>%
            dplyr::summarise() %>%
            sf::st_centroid() %>%
            sf::st_coordinates()

          leaflet::leafletProxy("carte_op") %>%
            leaflet::setView(
              lng = unname(CoordsStation[,"X"]),
              lat = unname(CoordsStation[,"Y"]),
              zoom = 15
            )
        } else {

          leaflet::leafletProxy("carte_op") %>%
            leaflet::fitBounds(
              map = .,
              lng1 = BboxMap[["xmin"]],
              lat1 = BboxMap[["ymin"]],
              lng2 = BboxMap[["xmax"]],
              lat2 = BboxMap[["ymax"]]
            )
        }

      })


    })

    SelectionPoint <- reactiveValues(clickedMarker=NULL)

    # observe the marker click info and print to console when it is changed.
    observeEvent(input$carte_op_marker_click,{
      SelectionPoint$clickedMarker <- input$carte_op_marker_click$id

      DonneesStation <- donnees_carte %>%
        dplyr::filter(
          code_station_hydrobio == SelectionPoint$clickedMarker
        ) %>%
        dplyr::group_by(code_station_hydrobio, libelle_station_hydrobio) %>%
        dplyr::summarise(
          derniers_resultats = paste(derniers_resultats, collapse = "<br>"),
          nb_annees = max(nb_annees),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          hover = paste0(
            "<b>", libelle_station_hydrobio, "</b><br><br>",
            derniers_resultats
          )
        )

      CoordsStation <- DonneesStation %>%
        sf::st_centroid() %>%
        sf::st_coordinates()


      leaflet::leafletProxy("carte_op") %>%
        leaflet::clearGroup(
          group = "station_selected"
          ) %>%
        leaflet::clearGroup(
          group = "all_stations"
        ) %>%
        leaflet::addCircleMarkers(
          map = .,
          data = DonneesCarte(),
          layerId = ~code_station_hydrobio,
          radius = ~radius_pal(nb_annees),
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
          radius = ~radius_pal(nb_annees),
          stroke = TRUE,
          color = "black",
          fillColor = c("#1874CD"),
          fillOpacity = 1,
          weight = 2,
          label = ~lapply(hover, shiny::HTML),
          options = pathOptions(pane = "foreground"),
          group = "station_selected"
        ) %>%
        leaflet::setView(
          lng = unname(CoordsStation[,"X"]),
          lat = unname(CoordsStation[,"Y"]),
          zoom = input$carte_op_zoom
        )
    })

    # POUR UNE RAISON QUE JE NE COMPRENDS PAS
    # CELA NE FONCTIONNE PAS
    # REINITIALISE LA VALEUR AU CLIC MEME SUR MARQUEUR ET
    # PAS QUE SUR FOND DE CARTE
    # observeEvent(input$carte_op_click,{
    #   SelectionPoint$clickedMarker <- NULL
    #   print("reset")
    # })


    reactive(SelectionPoint$clickedMarker)
  })
}

## To be copied in the UI
# mod_carte_ui("carte_1")

## To be copied in the server
# mod_carte_server("carte_1")
