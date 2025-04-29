### PREMIER PARTIE 

# --- Définitions pour le style des graphiques Plotly ---

plot_palette <- RColorBrewer::brewer.pal(8, "Set2") # donne 8 couleurs

# Paramètres de police communs
plotly_font_small <- list(
  family = "sans-serif",
  size = 10,  # Taille pour les axes, légendes, annotations
  color = "#EAEAEA" # Couleur claire pour le thème sombre
)
plotly_title_font_small <- list(
  family = "sans-serif",
  size = 12, # Taille un peu plus grande pour les titres principaux
  color = "#FFFFFF" # Blanc  pour les titres
)
# Paramètres généraux de layout pour thème sombre 
plotly_layout_dark_custom <- list(
  font = plotly_font_small,
  titlefont = plotly_title_font_small,
  paper_bgcolor = 'rgba(0,0,0,0)', # Fond transparent pour laisser passer le thème Shiny
  plot_bgcolor = 'rgba(0,0,0,0)',  # Fond de la zone de tracé transparent
  xaxis = list(
    gridcolor = 'rgb(100, 100, 100)', # Couleur de la grille X plus discrète
    zerolinecolor = 'rgb(150, 150, 150)', # Couleur de la ligne zéro X
    tickfont = plotly_font_small,
    titlefont = plotly_font_small #  même taille que les ticks pour les titres d'axe
  ),
  yaxis = list(
    gridcolor = 'rgb(100, 100, 100)', # Couleur de la grille Y plus discrète
    zerolinecolor = 'rgb(150, 150, 150)', # Couleur de la ligne zéro Y
    tickfont = plotly_font_small,
    titlefont = plotly_font_small #  même taille que les ticks pour les titres d'axe
  ),
  legend = list(font = plotly_font_small) # police pour la légende
)

# --- Fin Définitions Style Plotly ---


shinyServer(function(input, output, session) {
  
  session$onFlushed(function() {
    shinyjs::hide(id = "panel-tableau")
  }, once = TRUE)
  
  observeEvent(input$toggle_tableau, {
    shinyjs::toggle(id = "panel-tableau", anim = TRUE, animType = "fade")
  })
  
  
  # Stocker les données de coordonnées calculées pour la carte
  # afin de pouvoir les réutiliser dans l'observeEvent

  noms_regions_statuts <- reactiveVal(NULL)
  labels_regions_statuts <- reactiveVal(NULL)
  

   # Chargement des noms d'espèces dans le sélecteur
  updateSelectizeInput(session, "choix_espece",
                       choices = setNames(taxvern_clean$CD_NOM, taxvern_clean$LB_VERN),
                       server = TRUE)

  

####### === renderLeaflet ️ Carte de France
  
  output$map_especes <- renderLeaflet({
    regions_affichees <- regions_fr
    
    # --- Définir les couleurs souhaitées ---
    couleur_avec_statut <- plot_palette[1] # Utilise la 1ère couleur de la palette 
    couleur_sans_statut <- "#888888"       # Un gris neutre pour le fond
    
    
    # Coloriage conditionnel
    # Crée un vecteur de couleurs de la même longueur que regions_affichees
    couleur_region <- rep(couleur_sans_statut, nrow(regions_affichees)) # Tout en gris par défaut
    regions_avec_statut <- noms_regions_statuts() # Récupère les noms des régions avec statut
    
    if (!is.null(regions_avec_statut) && length(regions_avec_statut) > 0) {
      # Met la couleur "avec statut" pour les régions correspondantes
      couleur_region[regions_affichees$nom %in% regions_avec_statut] <- couleur_avec_statut
    }
    
###### ------
    
    leaflet(regions_affichees, options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      addPolygons(
        fillColor = couleur_region, #  Utilise le vecteur de couleurs calculé
        color = "white",
        weight = 1,
        fillOpacity = 0.6,
        label = lapply(regions_affichees$nom, function(nom_reg) {
          lbl <- labels_regions_statuts()
          html <- if (!is.null(lbl) && nom_reg %in% names(lbl)) {
            paste0("<strong>", nom_reg, "</strong><br/>", gsub("\n", "<br/>", lbl[[nom_reg]]))
          } else {
            nom_reg
          }
          HTML(html)
        }),
        labelOptions = labelOptions(
          direction = "auto",
          style = list("font-size" = "13px"),
          textsize = "13px",
          html = TRUE
        ),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(position = "topright",
                # mettre à jour les couleurs et labels de la légende 
                colors = c(couleur_avec_statut, couleur_sans_statut),
                labels = c("Région avec statut pour l'espèce", "Autres régions"),
                title = "Statuts de l'espèce",
                opacity = 0.9) %>%
      setView(lng = 2, lat = 47, zoom = 5)
  })
  
  
  

  
###########==== observeEvent choix espece 
  
  observeEvent(input$choix_espece, {
    req(input$choix_espece)
    
    cd_nom <- as.numeric(input$choix_espece)
    ligne <- taxvern_clean[CD_NOM == cd_nom]
    
    if (nrow(ligne) == 0) {
      output$resultat_taxref <- renderText("nom commun introuvable dans le fichier taxref")
      return()
    }
    
    # Infos générales via API
    infos_api <- get_infos_taxref(cd_nom)
    
    if (!is.list(infos_api)) {
      output$resultat_taxref <- renderText("Erreur : format inattendu depuis l'API")
      return()
    }
    
    #  Lien INPN
    inpn_url <- paste0("https://inpn.mnhn.fr/espece/cd_nom/", cd_nom)
    
    #  Affichage des infos générales
    output$resultat_taxref <- renderUI({
      tagList(
        h5(strong("Nom scientifique : "), infos_api$scientificName),
        if (!is.null(infos_api$vernacularGroup2)) h5(strong("Groupe : "), infos_api$vernacularGroup2),
        tags$a(
          href = inpn_url,
          target = "_blank",
          "🔗 Voir la fiche INPN",
          style = "display: inline-block; margin-top: 10px;"
        )
      )
    })
    
    #  Récupération des statuts et transformation en data.frame
    statuts <- get_statuts_par_espece(cd_nom)
    df_statuts <- if (!is.null(statuts)) as.data.frame(statuts) else NULL
    
 
    if (!is.null(df_statuts)) #print(unique(df_statuts$locationAdminLevel))
    
    #  Récupération des régions concernées
    if (!is.null(df_statuts)) {
      noms <- df_statuts$locationName[df_statuts$locationAdminLevel == "Région"]
      noms_regions_statuts(unique(na.omit(noms)))
    } else {
      noms_regions_statuts(NULL)
    }
    
    #  Création des labels pour la carte
    if (!is.null(df_statuts)) {
      df_regions <- df_statuts[
        df_statuts$locationAdminLevel %in% c("Région", "Ancienne région"),
      ]
      #print(unique(df_regions$locationName))
      grouped <- split(df_regions, df_regions$locationName)
      
      # Créer un résumé (ligne par statut)
      label_list_raw <- lapply(names(grouped), function(region) {
        lignes <- grouped[[region]]
        textes <- paste0("• ", lignes$statusTypeName, " – ", lignes$statusName)
        paste(textes, collapse = "\n")
      })
      
      # Donne un nom à chaque élément pour pouvoir l'appeler par région
      label_list <- setNames(label_list_raw, names(grouped))
      
    
      
      #  Stocke dans la variable réactive
      labels_regions_statuts(label_list)
    } else {
      labels_regions_statuts(NULL)
    }

    
    # Tableau dynamique avec tous les statuts (complémentaire à l'affichage résumé)
    output$tableau_statuts <- DT::renderDataTable({
      if (is.null(df_statuts) || nrow(df_statuts) == 0) return(NULL)
      
      DT::datatable(
        df_statuts[, c("statusTypeName", "statusName", "locationName", "statusRemarks")],
        colnames = c("Type", "Nom du statut", "Localité", "Remarques"),
        options = list(pageLength = 8, scrollX = TRUE),
        rownames = FALSE
      )
    })
    
    #  Téléchargement du tableau
    output$telecharger_statuts <- downloadHandler(
      filename = function() {
        paste0("statuts_", cd_nom, ".csv")
      },
      content = function(file) {
        if (!is.null(df_statuts)) {
          write.csv(df_statuts, file, row.names = FALSE, fileEncoding = "UTF-8")
        }
      }
    )
    
    
  })
  
  
  
  
  #  onglet 2
  
  ### renderLeaflet carte 2

  output$map_observations <- renderLeaflet({
    req(input$selected_departement, input$selected_departement != "")
    selected_dept_code <- input$selected_departement
    
    df_obs_dynamic <- observations_dynamique()
    req(df_obs_dynamic, nrow(df_obs_dynamic) > 0)
    
    nb_obs_value <- as.numeric(df_obs_dynamic$nb_obs[1])
    if(is.na(nb_obs_value)) nb_obs_value <- 0
    
    # Filtrer la géométrie
    departement_selected_geo <- departements_geo %>%
      filter(code == selected_dept_code)
    req(nrow(departement_selected_geo) > 0)
    
    #  Ajouter nb_obs à cette variable
    departement_selected_geo <- departement_selected_geo %>%
      mutate(nb_obs = nb_obs_value)
    # Vérifier après ajout colonne
    req(departement_selected_geo, nrow(departement_selected_geo) > 0)
    departement_selected_geo$nb_obs <- as.numeric(departement_selected_geo$nb_obs)
    
    # --- Vérification/Transformation CRS ---
    current_crs_info <- sf::st_crs(departement_selected_geo)
    if (is.na(current_crs_info$epsg) || current_crs_info$epsg != 4326) {
      tryCatch({ departement_selected_geo <- sf::st_transform(departement_selected_geo, crs = 4326) },
               error = function(e){ return(leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(2, 47, 5)) })
    }
    
    # --- Palette ---
    max_obs_overall <- max(df_obs_cache$nb_obs, 1, na.rm = TRUE)
    current_max_obs <- max(departement_selected_geo$nb_obs, 0 , na.rm = TRUE)
    plot_domain <- c(0, max(max_obs_overall, current_max_obs))
    pal <- colorNumeric("YlOrRd", domain = plot_domain)
    
    # --- Calcul du Centre pour setView ---
    center_lng <- 2.35; center_lat <- 47; center_zoom <- 5
    departement_point <- tryCatch(st_point_on_surface(departement_selected_geo), error = function(e) st_centroid(departement_selected_geo))
    if(!is.null(departement_point) && nrow(departement_point) > 0 && !st_is_empty(departement_point)){
      coords_vec <- st_coordinates(departement_point)[1,]
      if(all(is.finite(coords_vec))) {
        center_lng <- coords_vec[1]; center_lat <- coords_vec[2]; center_zoom <- 9
        #print(paste("Zoom sur:", selected_dept_code, "Centre:", round(center_lng,2), round(center_lat,2)))
      } else { warning("Coordonnées calculées invalides pour setView") }
    } else { warning("Point central non calculable pour setView") }
    

 # --- Création Carte ---
    
    # Initialisation avec les données finales établit le contexte pour les ~
    leaflet(data = departement_selected_geo) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      # Utiliser le contexte de données fourni à leaflet()
      addPolygons(fill = TRUE, fillColor = ~pal(nb_obs), fillOpacity = 0.7,
                  color = "#333333", weight = 1.5, opacity = 1,
                  label = ~paste0(nom, ": ", format(nb_obs, big.mark=" ", scientific = FALSE), " obs."), 
                  highlightOptions = highlightOptions(weight = 3, color = "#000000", fillOpacity = 0.9, bringToFront = TRUE),
                  options = pathOptions(pane = "overlayPane"),
                  layerId = ~code 
      ) 

    
  }) 
  
  ### ===============
  
  
  observations_dynamique <- reactive({
    #  input$selected_departement aux dépendances
    req(input$selected_group_inpn, input$date_range_taxon, input$selected_departement, input$selected_departement != "")
    
    selected_group_name <- input$selected_group_inpn
    selected_dept_code <- input$selected_departement #  le code département
    
    # Mapping groupe -> taxref_id 
    taxref_id_to_use <- tryCatch(
      map_groupe_inpn_vers_classe_cdref[[selected_group_name]],
      error = function(e) NULL
    )
    req(taxref_id_to_use) # S'assurer qu'on a l'ID
    
    #print(paste("Appel API pour:", selected_group_name, "(", taxref_id_to_use, ")",
     #           "Dépt:", selected_dept_code, # Log du département
      #          "Dates:", input$date_range_taxon[1], "au", input$date_range_taxon[2]))
    
    # Appel de la fonction ( ne fait qu'un appel)
    df <- get_observations_by_taxon(
      taxref_id = taxref_id_to_use,
      dept_code = selected_dept_code, # Passe le code département sélectionné
      start = input$date_range_taxon[1],
      end = input$date_range_taxon[2]
    )
    
    #print("Résultat API (1 ligne):")
    #print(df)
    return(df) # dataframe avec une seule ligne (ou NULL)
  })
  
  
  
  #### affichage dynamique au-dessus de la carte
  
  output$carte_text <- renderUI({
    req(input$selected_group_inpn, input$date_range_taxon, input$selected_departement, input$selected_departement != "")
    selected_group_name <- input$selected_group_inpn
    
    # Trouver le nom du département (pour affichage)
    selected_dept_name <- departements_geo$nom[departements_geo$code == input$selected_departement]
    # Prévoir un fallback si le nom n'est pas trouvé
    if (length(selected_dept_name) == 0) selected_dept_name <- paste("Dépt.", input$selected_departement)
    
    niveaux_val_str <- "Tous niveaux valides" 
    
    # Texte mis à jour pour refléter le contenu de la carte
    HTML(paste0(
      "<p><strong>Carte :</strong> Observations pour <strong>", selected_group_name, "</strong>",
      " dans le département : <strong>", selected_dept_name, "</strong>",
      "<br>(Validation: ", niveaux_val_str, ", Période: ",
      input$date_range_taxon[1], " au ", input$date_range_taxon[2], ")</p>"
    ))
  })
  
  
  
  ### output$plot_taxon_validation 
  
  
  output$plot_taxon_validation <- renderPlotly({
    req(input$selected_departement, length(input$selected_departement) == 1)
    req(input$selected_group_inpn, input$date_range_taxon)
    
    #print(paste("--- Début RenderPlotly Validation pour Dept:", input$selected_departement))
    
    plot_obj <- tryCatch({
      selected_dept_code <- input$selected_departement
      selected_group_name <- input$selected_group_inpn
      taxref_id_to_use <- map_groupe_inpn_vers_classe_cdref[[selected_group_name]]
      
      if (is.null(taxref_id_to_use)) {
        warning("Plotly: ID Taxon non trouvé.")
        return(plotly::plot_ly() %>%
                 layout(title = list(text = "ID Taxon non trouvé", font = plotly_title_font_small), #  police titre
                        paper_bgcolor = plotly_layout_dark_custom$paper_bgcolor, #  fond
                        plot_bgcolor = plotly_layout_dark_custom$plot_bgcolor,   
                        font = plotly_layout_dark_custom$font))                  #  police défaut
      }
      
      df_stats <- get_stats_by_validation(
        taxref_id = taxref_id_to_use,
        department_id = selected_dept_code,
        start = input$date_range_taxon[1],
        end = input$date_range_taxon[2],
        validations = c("CERTAIN", "PROBABLE", "DOUTEUX", "NON_REALISABLE")
      )
      
      if (is.null(df_stats) || nrow(df_stats) == 0 || sum(df_stats$count, na.rm = TRUE) == 0) {
        #print("Plotly: Aucune donnée de validation trouvée")
        return(plotly::plot_ly() %>%
                 layout(title = list(text = "Aucune donnée de validation", font = plotly_title_font_small),
                        paper_bgcolor = plotly_layout_dark_custom$paper_bgcolor,
                        plot_bgcolor = plotly_layout_dark_custom$plot_bgcolor,
                        font = plotly_layout_dark_custom$font))
      } else {
        #print("Plotly: Création du camembert.")
        # Utilise la palette définie
        couleurs_camembert <- plot_palette[3:nrow(df_stats)] # Prend les N premières couleurs
        
        plot_ly(
          df_stats, labels = ~level, values = ~count, type = "pie",
          textinfo = "label+percent", insidetextorientation = 'radial',
          marker = list(colors = couleurs_camembert, # Utilise la palette
                        line = list(color = '#FFFFFF', width = 1)),
          hoverinfo = 'label+value+percent' # info au survol
        ) %>%
          layout(
            #  paramètres de layout communs
            title = list(text = paste("Niveaux de validation pour", selected_group_name,
                                      "<br>dans le département", selected_dept_code),
                         font = plotly_title_font_small), #  titre
            font = plotly_layout_dark_custom$font,                  # police par défaut
            paper_bgcolor = plotly_layout_dark_custom$paper_bgcolor, # Fond
            plot_bgcolor = plotly_layout_dark_custom$plot_bgcolor,   
            legend = plotly_layout_dark_custom$legend,               # Style légende
            showlegend = TRUE # légende 
            
          )
      }
      
    }, error = function(e) {
      #print(paste("Erreur dans renderPlotly validation chart:", e$message))
      return(plotly::plot_ly() %>%
               layout(title = list(text = "Erreur de chargement", font = plotly_title_font_small),
                      paper_bgcolor = plotly_layout_dark_custom$paper_bgcolor,
                      plot_bgcolor = plotly_layout_dark_custom$plot_bgcolor,
                      font = plotly_layout_dark_custom$font))
    })
    
    if (!inherits(plot_obj, "plotly")) {
      warning("renderPlotly validation n'a pas retourné un objet plotly!")
      return(plotly::plot_ly() %>%
               layout(title = list(text = "Erreur inattendue", font = plotly_title_font_small),
                      paper_bgcolor = plotly_layout_dark_custom$paper_bgcolor,
                      plot_bgcolor = plotly_layout_dark_custom$plot_bgcolor,
                      font = plotly_layout_dark_custom$font))
    }
    
    #print("--- Fin RenderPlotly Validation ---")
    return(plot_obj)
    
  }) 
  
###### Texte 
  output$texte_explicatif_observations <- renderUI({
    div(class = "text-background-box", 
        h5("Comprendre les données affichées"),
        p(HTML("Les <strong>observations</strong> (ou occurrences) sont des signalements de présence d'un groupe d'espèces (ex: Oiseaux, Mammifères...) signalés par des observateurs et centralisés dans des bases de données nationales")),
        p(HTML("La <strong>carte</strong> montre le département sélectionné, coloré selon le nombre total d'observations rapportées pour le groupe et la période choisis")),
        p(HTML("Les autres graphiques fournissent un contexte sur la <strong>saisonnalité</strong> des observations ou sur la <strong>richesse taxonomique</strong> du département pour différents types d'<strong>habitats</strong>"))
    )
  })
  
  
  
#####  graphiques :
  

  # --- Graphique : Tendance Mensuelle ---
  output$plot_monthly_trend <- renderPlotly({
    req(input$selected_group_inpn, input$date_range_taxon, input$selected_departement, input$selected_departement != "")
    
    #print(paste("RenderPlotly: Tendance Mensuelle pour Dept:", input$selected_departement))
    
    selected_group_name <- input$selected_group_inpn
    taxref_id_to_use <- map_groupe_inpn_vers_classe_cdref[[selected_group_name]]
    req(taxref_id_to_use)
    
    df_monthly <- get_monthly_obs_for_dept_taxon(
      taxref_id = taxref_id_to_use,
      dept_code = input$selected_departement,
      start = input$date_range_taxon[1],
      end = input$date_range_taxon[2]
    )
    
    if (is.null(df_monthly) || nrow(df_monthly) == 0 || sum(df_monthly$count) == 0) {
      plot_ly() %>%
        layout(
          title = list(text = paste("Observations Mensuelles (Total",
                                    format(input$date_range_taxon[1], "%Y"), "-",
                                    format(input$date_range_taxon[2], "%Y"), ") pour", selected_group_name), 
                       font = plotly_title_font_small),
          font = plotly_layout_dark_custom$font,
          paper_bgcolor = plotly_layout_dark_custom$paper_bgcolor,
          plot_bgcolor = plotly_layout_dark_custom$plot_bgcolor,
          xaxis = c(plotly_layout_dark_custom$xaxis, list(title = "Mois")),
          yaxis = c(plotly_layout_dark_custom$yaxis, list(title = "Nombre total d'observations")) 
        )
    } else {
      df_monthly$month_name_fr <- factor(month.abb[df_monthly$month_num], levels = month.abb)
      
      plot_ly(df_monthly, x = ~month_name_fr, y = ~count, type = 'bar',
              #  première couleur de la palette
              marker = list(color = plot_palette[1],
                            line = list(color = 'rgba(255,255,255,0.7)', width = 0.5))) %>%
        layout(
          title = list(text = paste("Observations mensuelles (Total",
                                    format(input$date_range_taxon[1], "%Y"), "-",
                                    format(input$date_range_taxon[2], "%Y"), ") pour", selected_group_name), 
                       font = plotly_title_font_small),
          font = plotly_layout_dark_custom$font,
          paper_bgcolor = plotly_layout_dark_custom$paper_bgcolor,
          plot_bgcolor = plotly_layout_dark_custom$plot_bgcolor,
          xaxis = c(plotly_layout_dark_custom$xaxis, list(title = "Mois")),
          yaxis = c(plotly_layout_dark_custom$yaxis, list(title = "Nombre d'observations")) 
        )
    }
  }) 
  
  # --- Graphique : Répartition par habitat ---
  output$plot_dept_habitats <- renderPlotly({
    req(input$selected_departement, input$selected_departement != "")
    selected_dept_code <- input$selected_departement
    #print(paste("RenderPlotly: Habitats pour Dept:", selected_dept_code))
    
    df_habitats <- get_dept_habitats(dept_code = selected_dept_code)
    
    if (is.null(df_habitats) || nrow(df_habitats) == 0) {
      plot_ly() %>%
        layout(title = list(text = "Aucune donnée d'habitat trouvée", font = plotly_title_font_small),
               paper_bgcolor = plotly_layout_dark_custom$paper_bgcolor,
               plot_bgcolor = plotly_layout_dark_custom$plot_bgcolor,
               font = plotly_layout_dark_custom$font,
               xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
    } else {
      df_habitats <- df_habitats %>% filter(count > 0) %>% arrange(desc(count))
      
      plot_ly(df_habitats, x = ~count, y = ~reorder(habitat, count), type = 'bar', orientation = 'h',
              #  deuxième couleur de la palette
              marker = list(color = plot_palette[2],
                            line = list(color = 'rgba(255,255,255,0.7)', width = 0.5))) %>%
        layout(
          #  paramètres de layout communs
          title = list(text = paste("Nombre de taxons par habitat dans le dépt.", selected_dept_code),
                       font = plotly_title_font_small), #  titre
          font = plotly_layout_dark_custom$font,           
          paper_bgcolor = plotly_layout_dark_custom$paper_bgcolor, # Fond
          plot_bgcolor = plotly_layout_dark_custom$plot_bgcolor,   # Fond
          xaxis = c(plotly_layout_dark_custom$xaxis, list(title = "Nombre de taxons")), #  styles axes + titre spécifique
          yaxis = c(plotly_layout_dark_custom$yaxis, list(title = "Habitat", autorange="reversed")), #  styles axes + titre spécifique, inverser l'axe Y pour avoir le + gros en haut
          margin = list(l = 180) # On peut augmenter marge si labels habitats longs
        )
    }
  }) 
  

#####  Onglet Fiche especes 
  

  updateSelectizeInput(session, "choix_espece_fiche",
                       choices = setNames(taxvern_clean$CD_NOM, taxvern_clean$LB_VERN),
                       server = TRUE)
  
  # Réactive pour stocker les données de l'espèce sélectionnée dans cet onglet
  selected_species_data <- reactiveVal(list(info=NULL, classification=NULL, factsheet=NULL, media=NULL))
  
  #  sélection d'espèce dans le nouvel onglet
  observeEvent(input$choix_espece_fiche, {
    req(input$choix_espece_fiche)
    cd_nom <- as.numeric(input$choix_espece_fiche)
    
    #  infos de base (comme dans l'autre onglet)
    infos_api <- get_infos_taxref(cd_nom) #  fonction existante
    ligne_taxvern <- taxvern_clean[CD_NOM == cd_nom]
    
    if (!is.null(infos_api) && nrow(ligne_taxvern) > 0) {
      #  nouvelles fonctions API
      classification_data <- get_classification_taxon(cd_nom)
      factsheet_data <- get_factsheet_taxon(cd_nom)
      media_data <- get_media_taxon(cd_nom)
      
      
      #print("--- DEBUG MEDIA DATA ---")
      #print(str(media_data)) # structure détaillée
      #print("--- FIN DEBUG MEDIA DATA ---")
      
      #  résultats dans la réactive
      selected_species_data(list(
        info = infos_api,
        vernacular = ligne_taxvern$LB_VERN,
        classification = classification_data,
        factsheet = factsheet_data,
        media = media_data
      ))
      
      #  nom commun/scientifique trouvé dans la sidebar
      output$info_espece_fiche <- renderUI({
        tagList(
          h5(strong("Nom commun : "), ligne_taxvern$LB_VERN),
          h5(strong("Nom scientifique : "), tags$em(infos_api$scientificName))
        )
      })
      
      
    } else {
      # Réinitialise si espèce non trouvée
      selected_species_data(list(info=NULL, classification=NULL, factsheet=NULL, media=NULL))
      output$info_espece_fiche <- renderUI({ p("Espèce non trouvée.") })
    }
  })
  
  # --- Rendu des sorties pour la fiche espèce ---
  
  # Classification ( liste HTML)
  output$classification_output <- renderUI({
    data <- selected_species_data()
    # S'assure que data$classification n'est pas NULL et contient des lignes
    req(data$classification, nrow(data$classification) > 0)
    
    # ordre hiérarchique standard des rangs 
    ordre_rangs <- c("KD", "PH", "CL", "OR", "FM", "GN", "ES", "SSES", "VAR", "FO") # Codes des rangs

    # prépare les données
    classif_df <- data$classification %>%
      # Créer un facteur pour le tri basé sur l'ordre défini
      mutate(rankFactor = factor(rankId, levels = ordre_rangs, ordered = TRUE)) %>%
      # Filtre les rangs inconnus dans notre liste 
      filter(!is.na(rankFactor)) %>%
      # Trie selon ce facteur
      arrange(rankFactor)
    
    # Vérifie s'il reste des données après filtrage/tri
    req(nrow(classif_df) > 0)
    
    #  liste HTML
    tags$ul(
      lapply(1:nrow(classif_df), function(i) {
        tags$li(
          #  tagList pour combiner les éléments HTML correctement
          tagList(
            tags$strong(paste0(classif_df$rankName[i], ":")), # Nom du rang en gras
            " ", # Espace
            tags$em(classif_df$scientificName[i]) # Nom scientifique en italique
          )
        )
      })
    )
  })
  
  # Description / Portrait (avec "Lire la suite")
  output$description_output <- renderUI({
    data <- selected_species_data()
    req(data$factsheet)
    desc_text <- "Description non disponible."
    
    fs_data <- data$factsheet
    # ... (même logique d'extraction de desc_text qu'avant) ...
    if (!is.null(fs_data) && !is.null(fs_data$text)) {
      desc_text <- fs_data$text[1]
    } else if (!is.null(fs_data) && !is.null(fs_data$`_embedded`$factSheets$text)) {
      desc_text <- fs_data$`_embedded`$factSheets$text[1]
    }
    
    if (desc_text == "Description non disponible." || is.na(desc_text) || nchar(trimws(desc_text)) == 0) {
      p(em("Aucune description textuelle trouvée."))
    } else {
      # ID unique pour chaque instance 
      div_id <- paste0("desc-", data$info$id)
      link_id <- paste0("link-", data$info$id)
      
      tagList(
        # Le div contenant le texte, initialement limité en hauteur
        div(id = div_id, class = "collapsible-text",
            HTML(desc_text)
        ),
        # Le lien "Lire la suite" / "Réduire"
        a(id = link_id, href = "#", class = "read-more-link",
          onclick = sprintf("toggleReadMore('%s', '%s'); return false;", div_id, link_id),
          "Lire la suite")
      )
    }
  })
  
  # Image (Code  pour extraire l'URL)
  output$image_output <- renderUI({
    data <- selected_species_data()
    # On a besoin des données média et des infos de base (pour alt text)
    req(data$media, data$info)
    
    # pour trouver une image (ex: jpeg, png, gif)
    # La colonne mimeType existe bien d'après le debug
    images <- data$media %>%
      filter(grepl("image", mimeType, ignore.case = TRUE))
    
    if (nrow(images) > 0) {
      # Extraire l'URL depuis la colonne '_links.file.href' identifiée dans le debug
      #  [[...]] pour gérer le nom avec des points
      image_url <- NULL
      col_name <- "_links.file.href" #  nom exact de la colonne
      
      if (col_name %in% names(images)) {
        # première URL trouvée dans cette colonne
        image_url <- images[[col_name]][1]
      } else {
        # Fallback si cette colonne n'existe pas (ne devrait pas arriver vu le debug)
        warning(paste("La colonne", col_name, "n'a pas été trouvée dans les données média"))
      }
      
      
      #  si on a bien une URL non vide
      if(!is.null(image_url) && !is.na(image_url) && nchar(image_url) > 0) {
        #print(paste("URL de l'image trouvée:", image_url)) # Debug
        # image
        tags$img(src = image_url,
                 alt = paste("Image de", data$info$scientificName),
                 # Style pour s'assurer que l'image ne dépasse pas son conteneur
                 style = "max-width: 100%; max-height: 400px; height: auto; width: auto; display: block; margin-left: auto; margin-right: auto; border-radius: 5px;")
      } else {
        p(em("URL d'image valide non trouvée après extraction."))
      }
      
    } else {
      p(em("Aucun média de type image trouvé pour cette espèce via l'API INPN"))
    }
  })
  


  })
