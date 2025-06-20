library(dplyr)
library(sf)
library(leaflet)
# limites_region <-  sf::st_read("C:/Users/edgar.matter/Documents/STAGE/dataOutilValo/regions-20180101-shp/regions-20180101.shp") %>%
#   dplyr::filter(code_insee %in% c('32','11')) %>%
#   sf::st_transform(crs = 4326) %>%
#   rmapshaper::ms_simplify()
#
# # limites_region_l <- limites_region %>%
# #  sf::st_cast(to = "LINESTRING")
#
# limites_region_l <- limites_region$geometry %>%
#   sf::st_cast("MULTILINESTRING") %>%  # Convertir le MULTIPOLYGON en MULTILINESTRING
#   sf::st_cast("LINESTRING")  # Convertir chaque ligne du MULTILINESTRING en LINESTRING
#
# limites_bassin <- sf::st_read("C:/Users/edgar.matter/Documents/STAGE/dataOutilValo/BD_Topage_FXX_2024-shp/BassinHydrographique_FXX-shp/BassinHydrographique_FXX.shp") %>%
#   dplyr::filter(NumCircAdm %in% c("01","02","03")) %>% #Artois-Picardie et Seine Normandie et Rhin-Meuse
#   sf::st_transform(crs = 4326) %>%
#   rmapshaper::ms_simplify()
#
# limites_bassin_union <- sf::st_union(limites_bassin)
#
# limites_bassin_l <- limites_bassin$geometry %>%
#   sf::st_cast("MULTILINESTRING") %>%  # Convertir le MULTIPOLYGON en MULTILINESTRING
#   sf::st_cast("LINESTRING")  # Convertir chaque ligne du MULTILINESTRING en LINESTRING

limites_bassin_utiles <- sf::st_read("C:/Users/edgar.matter/Documents/STAGE/dataOutilValo/AEAP_AESN/Fusion.shp")%>%
  sf::st_transform(crs = 4326) %>%
  rmapshaper::ms_simplify()

limites_bassin_utiles_l <- limites_bassin_utiles$geometry %>%
  sf::st_cast("MULTILINESTRING") %>%  # Convertir le MULTIPOLYGON en MULTILINESTRING
  sf::st_cast("LINESTRING")  # Convertir chaque ligne du MULTILINESTRING en LINESTRING

limites_dep_utiles <- sf::st_read("C:/Users/edgar.matter/Documents/STAGE/dataOutilValo/departements/departements_utiles.shp")%>%
  sf::st_transform(crs = 4326) %>%
  rmapshaper::ms_simplify()

limites_dep_utiles_l <- limites_dep_utiles %>%
  sf::st_cast("MULTILINESTRING") %>%  # Convertir le MULTIPOLYGON en MULTILINESTRING
  sf::st_cast("LINESTRING")  # Convertir chaque ligne du MULTILINESTRING en LINESTRING

centroids <- st_centroid(limites_dep_utiles_l)
# Extraction des coordonnées en matrice
coords <- sf::st_coordinates(centroids)

# Création d'un data.frame simple avec lng, lat et label
labels_df <- data.frame(
  lng = coords[,1],
  lat = coords[,2],
  label = as.character(centroids$code_insee)
)

# limites_bassin <- sf::st_read(
#   dsn = "C:/QGIS-CUSTOM/DATA/VECTEUR/hydrographie/bdtopage_idf.gpkg",
#   layer = "BassinHydrographique"
# ) %>%
#   dplyr::filter(LbBH == "Seine-Normandie") %>%
#   sf::st_transform(crs = 4326) %>%
#   rmapshaper::ms_simplify()
#
# limites_bassin_l <- limites_bassin %>%
#   sf::st_cast(to = "LINESTRING")
#
# masque_metropole <- sf::st_read("C:/Users/edgar.matter/Documents/STAGE/dataOutilValo/EuropeUtile.shp") %>%
#   sf::st_transform(crs = 4326) %>%
#   sf::st_difference(limites_bassin) %>%
#   # dplyr::filter(INSEE_REG != "11") %>%
#   dplyr::summarise() %>%
#   rmapshaper::ms_simplify()

edl <- sf::st_read("C:/Users/edgar.matter/Documents/STAGE/dataOutilValo/edl/edl_ap_sn_utile.gpkg") %>%
  sf::st_transform(crs = 4326) %>%
  dplyr::filter(!sf::st_is_empty(.)) %>%
  # dplyr::mutate(biologique = recode(biologique,
  #                                   `0`='indéterminé',
  #                                   `1`='très bon',
  #                                   `2`='bon',
  #                                   `3`='moyen',
  #                                   `4`='médiocre',
  #                                   `5`='mauvais')
  # )%>%
  dplyr::mutate(
    dplyr::across(
      #c("ETAT.BIOLOGIQUE", "ETAT.ECOLOGIQUE", "ETAT.PHYSICO.CHIMIQUE"),
      c("ETAT.BIOLOGIQUE"),
      function(x) {factor(x, levels = c("très bon", "bon", "moyen", "médiocre", "mauvais", "indéterminé"))}
    ),
    ANNEE = 2022
  ) %>%
  #dplyr::rename(ETAT.BIOLOGIQUE = 'biologique', NOM.MASSE.D.EAU = NOM_MASSE_)%>%
  rmapshaper::ms_simplify()

usethis::use_data(
  limites_bassin_utiles, limites_bassin_utiles_l,
  limites_dep_utiles , limites_dep_utiles_l,
  centroids, labels_df,
  edl,
  internal = TRUE, overwrite = TRUE
)
