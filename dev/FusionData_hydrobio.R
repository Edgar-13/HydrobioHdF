library(dplyr)  # pour bind_rows

# Charger les deux fichiers dans des environnements séparés
env1 <- new.env()
load("dev/data_hydrobio.rda", envir = env1)

env2 <- new.env()
load("dev/data_hydrobio2.rda", envir = env2)

# Récupérer la liste des objets (data frames) dans les fichiers
objets_env1 <- ls(env1)
objets_env2 <- ls(env2)

# Objets communs (présents dans les deux fichiers)
objets_communs <- intersect(objets_env1, objets_env2)

# Nouvel environnement pour stocker les objets fusionnés
env_fusion <- new.env()

for(obj in objets_communs) {
  df1 <- get(obj, envir = env1)
  df2 <- get(obj, envir = env2)

  # Vérifier que ce sont bien des data frames (ou tibbles)
  if (is.data.frame(df1) && is.data.frame(df2)) {
    # Fusionner par lignes
    df_fusion <- bind_rows(df1, df2)

    # Assigner dans le nouvel environnement
    assign(obj, df_fusion, envir = env_fusion)
  } else {
    # Si ce ne sont pas des data frames, on peut choisir l'un ou les gérer autrement
    # Ici je prends la version de env1
    assign(obj, df1, envir = env_fusion)
  }
}

# Si il y a des objets présents dans un seul fichier (pas communs), on les ajoute aussi
objets_non_communs_1 <- setdiff(objets_env1, objets_communs)
for(obj in objets_non_communs_1) {
  assign(obj, get(obj, envir = env1), envir = env_fusion)
}

objets_non_communs_2 <- setdiff(objets_env2, objets_communs)
for(obj in objets_non_communs_2) {
  assign(obj, get(obj, envir = env2), envir = env_fusion)
}

# Sauvegarder le tout dans un seul fichier .rda
save(list = ls(env_fusion), envir = env_fusion, file = "dev/data_hydrobio_merged.rda")
