library(shiny)

shinyUI(
  fluidPage(
    theme = shinytheme("darkly"),  #darkly, flatly, cyborg, cerulean, united, superhero
    useShinyjs(),    
    # CSS personnalisé
    tags$head(
      includeCSS("www/styles.css"),

      tags$script(HTML("
    function toggleReadMore(divId, linkId) {
      var div = document.getElementById(divId);
      var link = document.getElementById(linkId);
      if (div.classList.contains('expanded')) {
        // Réduire
        div.classList.remove('expanded');
        link.textContent = 'Lire la suite';
      } else {
        // Développer
        div.classList.add('expanded');
        link.textContent = 'Réduire';
      }
    }
  "))
    ),
    
    
    
    # En-tête principal
    #h3("Espèces protégées en France"),
    
    # Barre de navigation
    navbarPage(
      title = tagList(
        icon("leaf"), # Votre icône
        tags$span(
          class = "navbar-brand-custom", # La classe CSS pour le style
          style = "margin-left: 8px;",    # Ajoute un petit espace après l'icône
          "Espèces protégées en France"  
        )
      ),
             
               # Onglet Fiche Espèce
               
               tabPanel("Fiche espèce",

                          # --- Ligne pour la recherche en haut ---
                          fluidRow(
                            column(width = 8, #  8 colonnes sur 12 à la recherche
                                   h4(" Rechercher une espèce"),
                                   selectizeInput("choix_espece_fiche",
                                                  label = NULL, # label redondant
                                                  choices = NULL,
                                                  selected = "",
                                                  multiple = FALSE,
                                                  options = list(placeholder = 'ex : loup, tyto alba...')
                                   )
                            ),
                            column(width = 4, #  affiche les noms trouvés
                                   br(), 
                                   br(),
                                   uiOutput("info_espece_fiche") #  nom commun/scientifique
                            )
                          ), 
                          
                          hr(), # séparation
                          
                          # ---  Résultats (classification, description, image) ---
                          fluidRow(
                            # Colonne pour classification et portrait
                            column(width = 7,
                                   h4("Classification Taxonomique"),
                                   #  div pour le fond 
                                   div(class = "text-background-box",
                                       shinycssloaders::withSpinner(
                                         uiOutput("classification_output"), 
                                         type = 6, color = "#FFFFFF"
                                       )
                                   ),
                                   hr(),
                                   h4("Portrait / Description"),
                                   #div pour le fond 
                                   div(class = "text-background-box",
                                       shinycssloaders::withSpinner(
                                         uiOutput("description_output"),
                                         type = 6, color = "#FFFFFF"
                                       )
                                   )
                            ),
                            # Colonne pour la Photo
                            column(width = 5,
                                   h4("Photographie"),
                                   shinycssloaders::withSpinner(
                                     uiOutput("image_output", height = "400px"), #hauteur
                                     type = 6, color = "#FFFFFF"
                                   )
                            )
                          ) 
                          
               ),
               

              
               ###########
               
               # Onglet Carte
               tabPanel("Statuts de protection par espèce",
                        div(
                          style = "position: relative; height: calc(100vh - 130px);",
                          leafletOutput("map_especes", height = "100%", width = "100%"),
                          
                          
                          #  Panneau flottant en haut à gauche
                          absolutePanel(
                            id = "panel-flottant", class = "panel panel-default",
                            fixed = TRUE, draggable = TRUE, top = 80, left = 30, width = 350,
                            style = "background-color: rgba(0,0,0,0.85); color: white; padding: 15px; border-radius: 10px; z-index: 500;",
                            
                            h4("🔍 Rechercher une espèce"),
                            selectizeInput("choix_espece",
                                           label = NULL,
                                           choices = NULL,
                                           selected = "", 
                                           multiple = FALSE,
                                           options = list(placeholder = 'ex : loup, hibou, lynx...')
                            ),
                            
                            br(),
                            uiOutput("resultat_taxref"),
                            br(),
                            actionButton("toggle_tableau", "Afficher/Masquer le tableau", class = "btn btn-light"),
                            br(),
                            downloadButton("telecharger_statuts", " Télécharger les statuts")
                          ),
                          
                          # Tableau en bas de la carte
                          absolutePanel(
                            id = "panel-tableau",
                            fixed = TRUE,
                            bottom = 0, left = 0, right = 0,
                            height = "300px",
                            style = "background-color: rgba(255,255,255,0.95); padding: 10px; overflow-y: auto; z-index: 9999;",
                            h5("Détails des statuts"),
                            DT::dataTableOutput("tableau_statuts")
                          )
                        )
               ),

               ### Onglet Observations 
                          
               tabPanel("Observations",
                        #  sidebarLayout pour séparer contrôles et résultats
                        sidebarLayout(
                          # --- Barre latérale avec les contrôles ---
                          sidebarPanel(
                            width = 3, # largeur 
                            h4("Sélection"),
                            selectInput("selected_group_inpn",
                                        "Choisir un Groupe :",
                                        choices = choices_groupes_finaux, # Défini dans global.R
                                        selected = "Oiseaux"),
                            dateRangeInput("date_range_taxon",
                                           "Période d'observation :",
                                           start = "2020-01-01", end = Sys.Date(), # la date actuelle
                                           format = "yyyy-mm-dd", language = "fr", separator = " au "),
                            selectInput("selected_departement",
                                        "Choisir un Département :",
                                        choices = departements_choices, # Défini dans global.R
                                        selected = "44"), # Loire-Atlantique par défaut
                            
                            #  un peu d'espace ou d'aide
                            br(),
                            p(em("Sélectionnez un groupe, une période et un département pour afficher les données correspondantes"))
                          ),
                          
                          # --- Panneau Principal avec les Résultats ---
                          mainPanel(
                            width = 9,
                            # Texte explicatif statique 
                            uiOutput("texte_explicatif_observations"),
                            hr(), # Ligne de séparation
                            
                          
                            # Onglets pour les graphiques et la carte
                            h5("Analyses Détaillées"),
                            tabsetPanel(id = "plotsTabset",
                                        # --- Onglet 1 : Carte et Validation  ---
                                        tabPanel("Carte & Validation",
                                                 #  TEXTE DYNAMIQUE 
                                                 uiOutput("carte_text"),
                                                 # ---------------------------------------------------
                                                 br(), 
                                                 fluidRow(
                                                   column(width = 7, # Colonne Carte
                                                          br(),
                                                          shinycssloaders::withSpinner(
                                                            leafletOutput("map_observations", height = "450px"),
                                                            type = 6, color = "#000000"
                                                          )
                                                   ),
                                                   column(width = 5, # Colonne Camembert
                                                          br(),
                                                          shinycssloaders::withSpinner(
                                                            plotlyOutput("plot_taxon_validation", height = "300px"),
                                                            type = 6, color = "#FFFFFF"
                                                          ),
                                                          br(), 
                                                          p(HTML("Le graphique de <strong>validation</strong> indique la fiabilité estimée de ces observations ('Certain', 'Probable', 'Douteux'...). Seules les observations jugées suffisamment fiables sont généralement utilisées pour les analyses scientifiques"),
                                                            #  un style pour réduire la taille de la police
                                                            style = "font-size: 0.9em; color: #cccccc; margin-top: 10px;"
                                                          )
                                                   )
                                                 ) 
                                        ),
                                        
                                        # --- Onglet 2 : Tendance mensuelle ---
                                        tabPanel("Tendance mensuelle", 
                                                 shinycssloaders::withSpinner(plotlyOutput("plot_monthly_trend"), type = 6, color = "#FFFFFF")
                                        ),
                                        
                                        # --- Onglet 3 : Habitats ---
                                        tabPanel("Habitats (Dépt)",
                                                 shinycssloaders::withSpinner(plotlyOutput("plot_dept_habitats"), type = 6, color = "#FFFFFF")
                                        )
                                        
                                        
                            ) 
                          ) 
                        ) 
               ), 
               
               
               # Onglet Contact

               tabPanel("À propos & contact",
                        fluidRow(
                          column(12,
                                 h4("À Propos de cette application"),
                                 p("Cette application web interactive a été développée dans le cadre du cours de R Shiny."),
                                 p("Ce cours fait partie du ",
                                   strong("Master 2 Économétrie et Statistiques Appliquées (ECAP)"),
                                   " de l'",
                                   # Lien vers le site de l'IAE Nantes
                                   tags$a(href="https://iae.univ-nantes.fr/", target="_blank", "IAE Nantes - Économie & Management"),
                                   ", année universitaire 2024-2025." 
                                 ),
                                 p("L'objectif principal était d'explorer les capacités de Shiny pour interagir avec des API publiques (ici, celles de l'",
                                   tags$a(href="https://inpn.mnhn.fr/", target="_blank", "INPN - Inventaire National du Patrimoine Naturel"),
                                   " et ",
                                   tags$a(href="https://openobs.mnhn.fr/", target="_blank", "OpenObs"), ") afin de visualiser des données sur la biodiversité française"
                                 ),
                                 
                                 hr(), 
                                 
                                 h4("Motivation"),
                                 p("Le choix du thème des espèces protégées découle d'un intérêt personnel pour la conservation de la faune et de la flore, illustré ici par la Tortue d'Hermann (*Testudo hermanni*), une espèce particulièrement fascinante et malheureusement menacée en France"),
                                 
                                 h5("La Tortue d'Hermann"),
                                 div(style="text-align: center;", 
                                     tags$img(
                                       src = "images/tortue-hermann.png",
                                       alt = "Tortue d'Hermann (Testudo hermanni)",
                                       style = "max-width: 250px; height: auto; margin-top: 10px; margin-bottom: 20px; border-radius: 5px;"
                                     )
                                 ),

                                 hr(),
                                 
                                 h4("Contact"),
                                 p(
                                   "Application réalisée par : ",
                                   strong("Gloria Isabel PALACIO")

                                 ),
                                 p(
                                   "Contact (Université de Nantes) : ",
                                   tags$a(href = "mailto:gloria.palacio-barco@etu.univ-nantes.fr", "gloria.palacio-barco@etu.univ-nantes.fr")
                                 ),
                                 
                                 hr(), 
                                 
                                 h4("Perspectives Futures"),
                                 p("Cette application est une première version qui pourra être enrichie et améliorée dans le futur, par exemple en ajoutant de nouvelles fonctionnalités, en intégrant d'autres sources de données ou en affinant les visualisations")
                                 
                          ) 
                        ) 
               ) 
               
    ),
    
    # Footer commun
    div(
      class = "footer",
      tags$a(
        href = "https://www.economiecirculaire.org/data/sources/users/3390/logoiaehorizontal.jpg",
        target = "_blank",
        tags$img(
          src = "images/logoiaehorizontal.jpg",
          alt = "Logo IAE",
          height = "40px",
          style = "margin-right: 15px;"
        )
      ),
      tags$a(
        href = "https://sites.google.com/view/master-ecap/accueil",
        target = "_blank",
        style = "color: #ccc; text-decoration: none;",
        "Master Économétrie, Statistique - Parcours Économétrie Appliquée"
      ),
      tags$span(
        style = "float: right; font-size: 10px; color: #999;",
        paste("Mis à jour le", format(Sys.Date(), "%d/%m/%Y"))
      )
    )
  )
  
)
