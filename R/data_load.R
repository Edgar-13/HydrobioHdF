# Chargement des données au démarrage du package
load_data_hydrobio <- function() {
  data_path <- system.file("data", "data_hydrobio.rda", package = "HydrobioHdF")
  if (file.exists(data_path)) {
    load(data_path, envir = globalenv())  # ou dans un environnement dédié
  } else {
    stop("Le fichier data_hydrobio.rda est introuvable dans le package.")
  }
}
