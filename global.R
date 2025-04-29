## Chargement des librairies
library(shiny)
library(dplyr)
library(tidyr)
library(leaflet)
library(data.table)
library(sf)
library(jsonlite)
library(httr)
library(purrr)
library(shinyjs)
library(shinythemes)
library(DT)
library(shinyWidgets)
library(plotly)
library(ggplot2)
library(ggExtra)
library(highcharter)
library(leaflet.extras)
library(leaflet.extras2)
library(readr)
library(shinycssloaders)
library(rsconnect)



################ PARTIE 1 



# --- Chargement TAXVERN (petit fichier) ---
cols_vern_needed <- c("CD_NOM", "LB_VERN", "LANGUE")
taxvern <- fread("www/tax_ref/TAXVERNv18.txt", sep = "\t", encoding = "UTF-8", select = cols_vern_needed)
taxvern_clean <- taxvern[tolower(LANGUE) == "français", .(CD_NOM, LB_VERN)]
rm(taxvern) # On peut supprimer l'objet complet si non utilisé ailleurs

# --- Chargement SÉLECTIF de TAXREF (gros fichier) ---

# colonnes strictement nécessaires pour le mapping des groupes
# necessaire pour limiter la taille et pouvoir le mettre en ligne
cols_ref_needed_for_groups <- c("GROUP2_INPN", "CLASSE", "RANG", "LB_NOM", "CD_REF")

#   SEULEMENT ces colonnes directement dans l'objet 'taxref'
print("Chargement sélectif de TAXREFv18.txt pour les groupes...")
taxref <- fread( #  fread est généralement plus rapide
  "www/tax_ref/TAXREFv18.txt",
  sep = "\t",
  encoding = "UTF-8",
  select = cols_ref_needed_for_groups,
  na.strings = c("", "NA")
)
print("Chargement sélectif terminé.")



############=================================recherche par espèce + statuts


# Fond de carte pour les régions 
regions_fr <- st_read("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/regions-version-simplifiee.geojson", quiet = TRUE)


get_infos_taxref <- function(cd_nom) {
  url <- paste0("https://taxref.mnhn.fr/api/taxa/", cd_nom)
  res <- httr::GET(url, httr::add_headers(Accept = "application/hal+json;version=2"))
  if (httr::status_code(res) == 200) {
    txt <- httr::content(res, as = "text", encoding = "UTF-8")
    result <- jsonlite::fromJSON(txt, flatten = FALSE)
    if (is.list(result)) return(result)
  }
  return(NULL)
}

get_statuts_par_espece <- function(cd_nom) {
  url <- paste0("https://taxref.mnhn.fr/api/taxa/", cd_nom, "/status/lines")
  res <- httr::GET(url, httr::add_headers(Accept = "application/hal+json;version=2"))
  if (httr::status_code(res) == 200) {
    txt <- httr::content(res, as = "text", encoding = "UTF-8")
    parsed <- jsonlite::fromJSON(txt, flatten = TRUE)
    return(parsed$`_embedded`$status)
  }
  return(NULL)
}

####### ============================ visualisation interactive par département


#######=====  chemin du cache
fichier_obs <- "www/data/observations_openobs_par_departement.rds"

#######==== départements (GeoJSON)
departements_geo <- st_read("www/SIG/contour-des-departements.geojson", quiet = TRUE)



##  API pour la carte dynamique
get_observations_by_taxon <- function(taxref_id, dept_code, start, end) {


  url <- paste0("https://openobs.mnhn.fr/api/occurrences/stats/taxon/", taxref_id)

  # Niveaux de validation (chaîne unique)
  les_niveaux_validation_vecteur <- c("CERTAIN", "PROBABLE", "DOUTEUX", "INVALIDE", "NON_REALISABLE", "NON_EVALUE")
  les_niveaux_validation_chaine <- paste(les_niveaux_validation_vecteur, collapse = ",")

  # --- Appel API UNIQUE ---
  res <- httr::GET(url, query = list(
    departmentInseeId = dept_code, # Utilise le code département fourni
    startDate = start,
    endDate = end,
    validationLevelNationale = les_niveaux_validation_chaine
  ))
  # ------------------------

  if (httr::status_code(res) == 200) {
    content <- httr::content(res, as = "parsed", encoding = "UTF-8")
    count <- if (!is.null(content$occurrenceCount)) content$occurrenceCount else 0
    # Retourner un dataframe avec une seule ligne
    return(data.frame(code = dept_code, nb_obs = count))
  } else {
    warning(paste("API Error pour taxon", taxref_id, "dept", dept_code, ":", httr::status_code(res)))
    # Retourner 0 pour ce département en cas d'erreur
    return(data.frame(code = dept_code, nb_obs = 0))
  }
}


##### ==========  Fonction API OpenObs : nombre d'observations par département (pour le cache initial)
get_observations_openobs <- function(dept_code) {
  url <- "https://openobs.mnhn.fr/api/occurrences/stats"
  res <- httr::GET(url, query = list(departmentInseeId = dept_code))
  
  if (httr::status_code(res) == 200) {
    content <- httr::content(res, as = "parsed", encoding = "UTF-8")
    if (!is.null(content$occurrenceCount)) return(as.numeric(content$occurrenceCount))
  }
  return(NA) #  NA ici pour savoir si l'appel initial a échoué
}

# Chargement ou génération des données initiales (cache)
if (!file.exists(fichier_obs)) {
  # print("Téléchargement des observations initiales via OpenObs...")
  codes_dep <- departements_geo$code

  nb_obs <- sapply(codes_dep, function(c) {
    Sys.sleep(0.2) # Pour éviter la surcharge de l'API
    get_observations_openobs(c)
  })
  
  
  df_obs_cache <- data.frame(code = codes_dep, nb_obs = as.numeric(nb_obs)) #  nb_obs doit etre numérique
  df_obs_cache <- df_obs_cache[!is.na(df_obs_cache$nb_obs), ] # Supprime les départements où l'appel API a échoué
  
  saveRDS(df_obs_cache, fichier_obs)
  #print("Données initiales sauvegardées localement")
} else {
  df_obs_cache <- readRDS(fichier_obs)
  #print("Données initiales chargées depuis le cache local")
}





# Identifier les groupes GROUP2_INPN pertinents
#    On exclut NA et "Autres". On pourrait exclure d'autres groupes peu pertinents si besoin
groupes_inpn_valides <- taxref %>%
  filter(!is.na(GROUP2_INPN), GROUP2_INPN != "Autres") %>%
  distinct(GROUP2_INPN) %>%
  pull(GROUP2_INPN) %>%
  sort()

#print(paste("Groupes INPN trouvés:", paste(groupes_inpn_valides, collapse=", ")))

#  mapping : Nom du Groupe INPN -> CD_REF de la CLASSE correspondante
#    On suppose qu'un GROUP2_INPN correspond principalement à une CLASSE pour l'API
map_groupe_inpn_vers_classe_cdref <- list()

for (groupe in groupes_inpn_valides) {
  #  classe la plus fréquente associée à ce groupe INPN
  classe_associee <- taxref %>%
    filter(GROUP2_INPN == groupe, !is.na(CLASSE)) %>%
    count(CLASSE, sort = TRUE) %>%
    slice(1) %>% #  classe la plus fréquente
    pull(CLASSE)
  
  if (length(classe_associee) == 1) {
    # CD_REF de cette classe (au rang CL)
    info_classe <- taxref %>%
      filter(RANG == "CL", LB_NOM == classe_associee) %>%
      distinct(CD_REF) #  distinct pour s'assurer de l'unicité
    
    if (nrow(info_classe) == 1) {
      map_groupe_inpn_vers_classe_cdref[[groupe]] <- info_classe$CD_REF[1]
      #print(paste("Mapping OK:", groupe, "->", classe_associee, "(CD_REF:", info_classe$CD_REF[1], ")"))
    } else {
      warning(paste("CD_REF non unique ou introuvable pour la CLASSE", classe_associee, "associée au groupe", groupe))
    }
  } else {
    warning(paste("Impossible de déterminer une CLASSE principale pour le groupe", groupe))
  }
}

# Filtre les choix pour ne garder que les groupes qu'on a réussi à mapper
choices_groupes_finaux <- setNames(
  names(map_groupe_inpn_vers_classe_cdref), # Les noms des groupes mappés comme valeurs
  names(map_groupe_inpn_vers_classe_cdref)  # Et comme labels affichés
)


# Vérification rapide
#print("Mapping final Groupe INPN -> CD_REF Classe:")
#print(map_groupe_inpn_vers_classe_cdref)
#print("Choix finaux pour l'UI:")
#print(choices_groupes_finaux)

# nettoyage des variables intermédiaires si besoin
# rm(groupes_inpn_valides, classe_associee, info_classe, groupe)

#print("Préparation des groupes terminée")

# 'departements_choices' pour le sélecteur de département
departements_choices <- setNames(departements_geo$code, departements_geo$nom)



########## --- ---- GRAPHIQUES --------




# Fonction pour appeler l’API OpenObs avec plusieurs niveaux de validation
get_stats_by_validation <- function(taxref_id, department_id, start, end, validations) {
  results <- lapply(validations, function(val) {
    #  URL de manière plus robuste
    url <- modify_url("https://openobs.mnhn.fr",
                      path = paste0("/api/occurrences/stats/taxon/", taxref_id),
                      query = list(
                        startDate = start,
                        endDate = end,
                        departmentInseeId = department_id,
                        validationLevelNationale = val
                      ))
    
    res <- httr::GET(url)
    # Géstion de l'erreur API de manière  explicite
    if (httr::status_code(res) != 200) {
      warning(paste("API Error for", val, ":", httr::status_code(res)))
      return(data.frame(level = val, count = 0))
    }
    
    content <- httr::content(res, as = "parsed", encoding = "UTF-8")
    count <- if (!is.null(content$occurrenceCount)) content$occurrenceCount else 0
    data.frame(level = val, count = count)
  })
  
  do.call(rbind, results)
}



### Nouvelles API 



# Fonction pour l'API /occurrences/stats/monthly/taxon/{taxrefId}
get_monthly_obs_for_dept_taxon <- function(taxref_id, dept_code, start, end) {

    url <- paste0("https://openobs.mnhn.fr/api/occurrences/stats/monthly/taxon/", taxref_id)
  # Utilise les mêmes niveaux de validation que pour la carte principale 
  les_niveaux_validation_vecteur <- c("CERTAIN", "PROBABLE", "DOUTEUX", "INVALIDE", "NON_REALISABLE", "NON_EVALUE")
  les_niveaux_validation_chaine <- paste(les_niveaux_validation_vecteur, collapse = ",")
  
  res <- httr::GET(url, query = list(
    departmentInseeId = dept_code,
    startDate = start,
    endDate = end,
    validationLevelNationale = les_niveaux_validation_chaine
  ))
  
  if (httr::status_code(res) == 200) {
    content <- httr::content(res, as = "parsed", encoding = "UTF-8")
    # L'API renvoie une liste de {month: "JANUARY", occurrenceCount: 0}
    if (length(content) > 0) {
      #  en dataframe plus facile à utiliser
      df <- purrr::map_dfr(content, ~ data.frame(month_name = .x$month, count = .x$occurrenceCount))
      # Convertir les noms de mois en numéros pour le tri
      month_levels <- c("JANUARY", "FEBRUARY", "MARCH", "APRIL", "MAY", "JUNE", "JULY", "AUGUST", "SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER")
      df$month_num <- match(df$month_name, month_levels)
      df <- df %>% arrange(month_num)
      return(df)
    } else {
      return(data.frame(month_name=character(), count=numeric(), month_num=numeric())) # Retourne vide si pas de contenu
    }
  } else {
    warning(paste("API Error monthly obs pour taxon", taxref_id, "dept", dept_code, ":", httr::status_code(res)))
    return(NULL) # Retourne NULL en cas d'erreur API
  }
}

# Fonction pour l'API /taxa/stats/habitats
get_dept_habitats <- function(dept_code) {

    url <- "https://openobs.mnhn.fr/api/taxa/stats/habitats"
  res <- httr::GET(url, query = list(departmentInseeId = dept_code))
  
  if (httr::status_code(res) == 200) {
    content <- httr::content(res, as = "parsed", encoding = "UTF-8")
    # Renvoie une liste de {habitat: "string", taxonCount: 0}
    if (length(content) > 0) {
      df <- purrr::map_dfr(content, ~ data.frame(habitat = .x$habitat, count = .x$taxonCount))

      return(df)
    } else {
      return(data.frame(habitat=character(), count=numeric()))
    }
  } else {
    warning(paste("API Error habitats pour dept", dept_code, ":", httr::status_code(res)))
    return(NULL)
  }
}

##### Onglet description des especes 


# Fonction pour obtenir la classification d'un taxon
get_classification_taxon <- function(cd_nom) {
  url <- paste0("https://taxref.mnhn.fr/api/taxa/", cd_nom, "/classification")
  tryCatch({
    res <- httr::GET(url, httr::add_headers(Accept = "application/hal+json;version=1"))
    if (httr::status_code(res) == 200) {
      content <- httr::content(res, as = "text", encoding = "UTF-8")
      parsed <- jsonlite::fromJSON(content, flatten = TRUE)
      # Renvoie la liste des taxons parents (le _embedded.taxa)
      return(parsed$`_embedded`$taxa)
    } else {
      warning(paste("API Error classification pour cd_nom", cd_nom, ":", httr::status_code(res)))
      return(NULL)
    }
  }, error = function(e) {
    warning(paste("Erreur lors de l'appel API classification pour cd_nom", cd_nom, ":", e$message))
    return(NULL)
  })
}

# Fonction pour obtenir la fiche descriptive (portrait)
get_factsheet_taxon <- function(cd_nom) {
  url <- paste0("https://taxref.mnhn.fr/api/taxa/", cd_nom, "/factsheet")
  tryCatch({
    res <- httr::GET(url, httr::add_headers(Accept = "application/hal+json;version=1"))
    # L'API renvoie parfois 404 si pas de fiche, ce qui est normal
    if (httr::status_code(res) == 200) {
      content <- httr::content(res, as = "text", encoding = "UTF-8")
      parsed <- jsonlite::fromJSON(content, flatten = TRUE)
      # Renvoie le contenu de la fiche (_embedded.factSheets peut contenir plusieurs langues)
      # On pourrait filtrer sur la langue ici si nécessaire (parsed$langageId == 'fra')
      return(parsed) # Retourne l'objet complet pour l'instant
    } else if (httr::status_code(res) == 404) {
      return(NULL) # Pas de fiche trouvée
    } else {
      warning(paste("API Error factsheet pour cd_nom", cd_nom, ":", httr::status_code(res)))
      return(NULL)
    }
  }, error = function(e) {
    warning(paste("Erreur lors de l'appel API factsheet pour cd_nom", cd_nom, ":", e$message))
    return(NULL)
  })
}

# Fonction pour obtenir les médias (photos)
get_media_taxon <- function(cd_nom) {
  url <- paste0("https://taxref.mnhn.fr/api/taxa/", cd_nom, "/media")
  tryCatch({
    res <- httr::GET(url, httr::add_headers(Accept = "application/hal+json;version=1"))
    if (httr::status_code(res) == 200) {
      content <- httr::content(res, as = "text", encoding = "UTF-8")
      parsed <- jsonlite::fromJSON(content, flatten = TRUE)
      # Renvoie la liste des médias
      return(parsed$`_embedded`$media)
    } else {
      warning(paste("API Error media pour cd_nom", cd_nom, ":", httr::status_code(res)))
      return(NULL)
    }
  }, error = function(e) {
    warning(paste("Erreur lors de l'appel API media pour cd_nom", cd_nom, ":", e$message))
    return(NULL)
  })
}
