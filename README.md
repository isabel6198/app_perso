
## Fonctionnement de l'application

Cette application  R Shiny permet d'explorer les statuts de protection et les observations d'espèces en France métropolitaine. Elle s'appuie sur des données publiques et des API fournies par l'Inventaire National du Patrimoine Naturel (INPN)

### Sources de données utilisées

* **Données taxonomiques (TAXREF) :** Les informations sur les noms d'espèces (communs et scientifiques), les rangs, la classification et les groupes proviennent du référentiel taxonomique national TAXREF (version 18.0). Les fichiers `TAXVERNv18.txt` (noms vernaculaires) et `TAXREFv18.txt` (référentiel principal) sont nécessaires
    * *Note :* Le fichier `TAXREFv18.txt` étant volumineux, il n'est pas inclus dans ce dépôt Git. Veuillez le télécharger depuis [https://inpn.mnhn.fr/telechargement/referentielEspece/taxref/18.0/menu] et le placer dans le dossier `www/tax_ref/`. Le fichier `TAXVERNv18.txt` doit aussi être téléchargé depuis lien.
* **Contours géographiques :**
    * Les contours des régions françaises simplifiés sont chargés depuis le dépôt GitHub de `gregoiredavid/france-geojson`.
    * Les contours des départements français proviennent d'un fichier GeoJSON local (`www/SIG/contour-des-departements.geojson`).
* **Cache d'observations :** Un fichier cache (`www/data/observations_openobs_par_departement.rds`) stocke le nombre total d'observations par département pour accélérer le chargement initial. Il est créé automatiquement au premier lancement si absent. (Inclus dans le dépôt Git)

---
### APIs Interrogées

L'application interroge en temps réel deux API principales de l'INPN :

1.  **API INPN - TAXREF (`taxref.mnhn.fr/api/`)**
    * Utilisée pour obtenir des informations détaillées sur les taxons
    * **Points d'accès clés et Fonctions R associées (`global.R`) :**
        * `/taxa/{cd_nom}` (`get_infos_taxref`) : Récupère les informations de base (nom scientifique, rang, parent...) d'un taxon via son code `cd_nom`. Utilisé dans les onglets "Statuts..." et "Fiche Espèce"
        * `/taxa/{cd_nom}/status/lines` (`get_statuts_par_espece`) : Obtient la liste des statuts légaux ou de conservation (protection, liste rouge...) pour un taxon. Utilisé dans l'onglet "Statuts..."
        * `/taxa/{cd_nom}/classification` (`get_classification_taxon`) : Récupère la lignée taxonomique complète (Règne, Phylum, Classe...) d'un taxon. Utilisé dans l'onglet "Fiche Espèce"
        * `/taxa/{cd_nom}/factsheet` (`get_factsheet_taxon`) : Obtient la fiche descriptive textuelle (si disponible) pour un taxon. Utilisé dans l'onglet "Fiche Espèce"
        * `/taxa/{cd_nom}/media` (`get_media_taxon`) : Récupère la liste des médias (photos, sons...) associés à un taxon (si disponibles). Utilisé pour afficher l'image dans l'onglet "Fiche Espèce"

2.  **API INPN - OpenObs (`openobs.mnhn.fr/api/`)**
    * Utilisée pour obtenir des statistiques agrégées sur les observations d'espèces
    * **Points d'accès clés et Fonctions R associées (`global.R`) :**
        * `/api/occurrences/stats` (`get_observations_openobs`) : Obtient le **nombre total d'observations** (tous taxons confondus) pour un département donné. Utilisé uniquement pour créer le fichier cache au premier lancement
        * `/api/occurrences/stats/taxon/{taxrefId}` (`get_observations_by_taxon`, `get_stats_by_validation`) : Point d'accès central pour les statistiques d'observation d'un *groupe taxonomique* donné (`taxrefId`, correspondant à une Classe dans notre cas) :
            * Appelé une fois par département/période/groupe pour obtenir le nombre total d'observations (`get_observations_by_taxon`). Utilisé pour la carte de l'onglet "Observations"
            * Appelé plusieurs fois (filtré par niveau de validation) pour obtenir la répartition par statut de validation (`get_stats_by_validation`). Utilisé pour le camembert de validation
        * `/api/occurrences/stats/monthly/taxon/{taxrefId}` (`get_monthly_obs_for_dept_taxon`) : Obtient le **nombre total d'observations agrégées par mois** sur la période sélectionnée pour un groupe taxonomique. Utilisé pour le graphique de saisonnalité
        * `/api/taxa/stats/habitats` (`get_dept_habitats`) : Obtient le **nombre de taxons recensés par grand type d'habitat** dans un département. Utilisé pour le graphique des habitats


-----
### Fonctions principales (`global.R`)

Le fichier `global.R` contient :

* Le chargement des librairies R nécessaires
* Le chargement des données locales (TAXREF/TAXVERN, GeoJSON)
* La logique de mise en cache pour les observations initiales
* La préparation des listes de choix pour l'interface utilisateur (groupes taxonomiques, départements)
* Les fonctions personnalisées (commençant par `get_...`) qui encapsulent les appels aux API INPN (Taxref et OpenObs). Ces fonctions gèrent la construction de l'URL, l'appel HTTP (`httr`), l'interprétation de la réponse JSON (`jsonlite`), et le retour des données dans un format structuré (`list` ou `data.frame`), tout en incluant une gestion simple des erreurs ou des réponses vides


---

### Préparation des données au lancement

Au démarrage de l'application, plusieurs étapes de préparation sont effectuées (`global.R`) :

* **Chargement des données locales :**
    * Lecture du fichier des noms vernaculaires français (`www/tax_ref/TAXVERNv18.txt` via `data.table::fread`)
    *Nécessite le téléchargement préalable de ce fichier (voir section Sources de Données)*
    * Lecture des contours géographiques des régions (via URL) et des départements (fichier local `www/SIG/...`)
* **Mise en cache des observations initiales :**
    * Pour accélérer l'affichage initial de la carte dans l'onglet "Observations", l'application vérifie si un fichier cache (`www/data/observations_openobs_par_departement.rds`) existe
    * Si le fichier n'existe pas (premier lancement ou cache supprimé), l'application interroge l'API OpenObs (`/api/occurrences/stats` pour chaque département) afin d'obtenir le nombre total d'observations par département. Ces requêtes sont espacées pour respecter l'API. Le résultat est ensuite sauvegardé dans le fichier `.rds`
    * Lors des lancements suivants, l'application charge directement ce fichier cache, évitant de multiples appels API
* **Préparation des groupes taxonomiques pour l'UI :**
    * L'onglet "Observations" permet de filtrer par grand groupe taxonomique (Oiseaux, Mammifères, etc.)
    * Pour créer cette liste déroulante simplifiée, le code analyse le fichier TAXREF/TAXVERN :
        1.  Il identifie les groupes pertinents définis dans la colonne `GROUP2_INPN`.
        2.  Pour chaque groupe (ex: "Oiseaux"), il détermine la classe taxonomique (`CLASSE`) la plus représentative (ex: "Aves")
        3.  Il récupère l'identifiant numérique (`CD_REF`) de cette classe
    * Ce mapping permet à l'utilisateur de choisir un groupe simple, tout en fournissant à l'application un `CD_REF` précis (celui de la classe) pour interroger plus efficacement les API d'observations (OpenObs). La liste de ces groupes est ensuite utilisée pour peupler le sélecteur dans l'interface utilisateur
* **Préparation des choix de départements :** La liste des départements français (noms et codes INSEE) est préparée pour être utilisée dans le sélecteur de département de l'onglet "Observations"


---

### Logique côté serveur (`server.R`)

Le fichier `server.R` contient la logique réactive de l'application, définissant comment les sorties (cartes, graphiques, tableaux, textes) sont générées et mises à jour en fonction des actions de l'utilisateur (entrées via les widgets)

#### Initialisation et styles communs

* Au début du fichier, des définitions de style (palette de couleurs `plot_palette`, tailles de police, fonds transparents) sont créées pour assurer une apparence visuelle cohérente à tous les graphiques générés avec la librairie `plotly`
* La fonction `shinyServer` initialise la session utilisateur
* Du code `shinyjs` est utilisé pour gérer l'affichage initial et la bascule (afficher/masquer) du panneau contenant le tableau détaillé des statuts dans le premier onglet



#### Onglet "Statuts de protection par espèce"

Cet onglet permet à l'utilisateur de sélectionner une espèce et de visualiser les régions où elle bénéficie d'un statut de protection ou de menace, ainsi que le détail de ces statuts.

* **Entrée Utilisateur :**
     Un sélecteur (`selectizeInput` - `input$choix_espece`) permet de rechercher et choisir une espèce par son nom commun. La liste est alimentée par le fichier TAXREF/TAXVERN chargé dans `global.R`
* **Réactivité Principale (`observeEvent(input$choix_espece, ...)`):**

Lorsqu'une espèce est sélectionnée, cet `observeEvent` déclenche une série d'actions :
    
        1.  Le code `cd_nom` de l'espèce est identifié
        
        2.  L'API TAXREF (`get_infos_taxref`) est appelée pour récupérer le nom scientifique et le groupe taxonomique, qui sont affichés sous le sélecteur (`output$resultat_taxref`) avec un lien vers la fiche INPN de l'espèce
        
        3.  L'API TAXREF (`get_statuts_par_espece`) est appelée pour obtenir la liste complète des statuts associés à cette espèce (type de statut, nom, localité...)
        
        4.  Ces données de statuts sont traitées pour :
        
            * Identifier les noms des régions administratives concernées. Cette liste est stockée dans une variable réactive (`noms_regions_statuts`)
            * Générer des étiquettes textuelles formatées (en HTML) résumant les statuts pour chaque région concernée. Ces étiquettes sont stockées dans une autre variable réactive (`labels_regions_statuts`)
            * Afficher la liste détaillée des statuts dans un tableau interactif (`output$tableau_statuts`) en utilisant le package `DT`
            * Permettre le téléchargement de ce tableau au format CSV (`output$telecharger_statuts`)
* **Carte (`output$map_especes` - `renderLeaflet`) :**

     Cette fonction génère la carte de France (contours des régions)
    * Elle lit les variables réactives `noms_regions_statuts()` et `labels_regions_statuts()`.
    * Les régions sont colorées différemment (en utilisant la palette de couleurs définie) selon qu'elles possèdent ou non un statut pour l'espèce sélectionnée
    * Au survol d'une région avec la souris, les étiquettes formatées des statuts correspondants sont affichées
    * Une légende explique le code couleur


#### Onglet "Observations"

Cet onglet est dédié à la visualisation des données d'observations (signalements de présence) pour un groupe taxonomique, une période et un département donnés. Il s'appuie principalement sur l'API OpenObs

* **Entrées Utilisateur :**
    * `selectInput("selected_group_inpn", ...)` : Choix du groupe taxonomique (préparé dans `global.R`)
    * `dateRangeInput("date_range_taxon", ...)` : Sélection de la période d'observation
    * `selectInput("selected_departement", ...)` : Choix du département (liste préparée dans `global.R`)
* **Textes d'Information :**
    * Bloc de texte statique (`output$texte_explicatif_observations`) explique les données affichées dans l'onglet, en utilisant un style visuel distinct (`text-background-box`)
    * Un texte dynamique (`output$carte_text`) situé au-dessus des visualisations rappelle les sélections actuelles de l'utilisateur (groupe, département, période)
* **Récupération des données d'observation (`observations_dynamique`) :**
    * Une expression réactive centrale (`reactive({...})`) est déclenchée par tout changement dans les sélections de l'utilisateur
    * Elle détermine l'ID TaxRef (Classe) correspondant au groupe choisi
    * Elle appelle la fonction `get_observations_by_taxon` (qui interroge l'API OpenObs `/occurrences/stats/taxon/{taxrefId}`) pour obtenir le **nombre total** d'observations correspondant aux critères
    * Le résultat (un dataframe simple avec le code département et le nombre d'observations) est utilisé par la carte
* **Carte des observations (`output$map_observations` - `renderLeaflet`) :**
    * Affiche la carte de France centrée sur le département sélectionné.
    * Le polygone du département est coloré en fonction du nombre total d'observations (obtenu via `observations_dynamique`) en utilisant une échelle de couleurs ("YlOrRd")
    * Les informations de base (nom du département, nombre d'observations) sont affichées au survol
* **Graphiques d'analyse (`tabsetPanel`) :**
    * **Carte & Validation :**
        * La carte décrite ci-dessus est affichée
        * À côté, un graphique camembert (`output$plot_taxon_validation` - `renderPlotly`) montre la **répartition des observations selon leur niveau de validation** (Certain, Probable, Douteux...). Les données sont obtenues via `get_stats_by_validation` (qui appelle l'API OpenObs `/occurrences/stats/taxon/{taxrefId}` pour chaque niveau). Un texte explicatif est placé sous ce graphique (directement dans `ui.R`)
    * **Tendance mensuelle :**
        * Un graphique en barres (`output$plot_monthly_trend` - `renderPlotly`) illustre la **saisonnalité** des observations. Il montre le **nombre total d'observations pour chaque mois**, agrégé sur toute la période sélectionnée. Les données proviennent de `get_monthly_obs_for_dept_taxon` (API OpenObs `/occurrences/stats/monthly/taxon/{taxrefId}`)
    * **Habitats (Dépt) :**
        * Un graphique en barres horizontales (`output$plot_dept_habitats` - `renderPlotly`) montre le **nombre de taxons** (et non d'observations) recensés dans les différents grands types d'habitats pour le département sélectionné. Les données sont issues de `get_dept_habitats` (API OpenObs `/taxa/stats/habitats`)
    * Tous les graphiques Plotly utilisent les définitions de style personnalisées (polices, couleurs, fonds) définies au début de `server.R` pour une apparence cohérente avec le thème sombre

----

#### Onglet "Fiche Espèce"

Cet onglet permet d'obtenir des informations détaillées sur une espèce spécifique sélectionnée par l'utilisateur.

* **Entrée utilisateur :**
    * Un sélecteur (`selectizeInput` - `input$choix_espece_fiche`) permet de rechercher et choisir une espèce par son nom commun
* **Réactivité principale (`observeEvent(input$choix_espece_fiche, ...)`):**
    * Lorsqu'une espèce est sélectionnée :
        1.  Les informations de base (nom scientifique, groupe...) sont récupérées via `get_infos_taxref` (API Taxref) et affichées en haut (`output$info_espece_fiche`)
        2.  La **classification taxonomique** complète (Règne, Phylum, Classe, Ordre, Famille, Genre, Espèce...) est récupérée via `get_classification_taxon` (API Taxref)
        3.  La **fiche descriptive** textuelle (si elle existe dans la base de connaissance de l'INPN) est récupérée via `get_factsheet_taxon` (API Taxref).
        4.  La liste des **médias** (photos, sons...) associés à l'espèce est récupérée via `get_media_taxon` (API Taxref)
        5.  Toutes ces informations sont stockées dans une variable réactive (`selected_species_data`) pour être utilisées par les éléments d'affichage
* **Affichage des résultats :**
    * **Classification (`output$classification_output`) :** La hiérarchie taxonomique est présentée sous forme d'une liste formatée (HTML), triée du rang le plus élevé au plus bas
    * **Description/Portrait (`output$description_output`) :** Le texte descriptif issu de la fiche INPN est affiché. Pour les textes longs, un mécanisme "Lire la suite / Réduire" (implémenté avec CSS et JavaScript) permet de ne montrer initialement qu'un extrait
    * **Image (`output$image_output`) :** L'application tente d'afficher la première image disponible trouvée via l'API média. Si aucune image n'est trouvée ou si le lien fourni par l'API est invalide pour une espèce donnée, un message l'indique


----
### Interface Utilisateur (`ui.R`)

L'interface utilisateur de l'application est construite avec le package Shiny et structurée à l'aide de `shiny::fluidPage` et `shiny::navbarPage` pour la navigation principale par onglets. Le thème visuel sombre "darkly" (`shinythemes::shinytheme("darkly")`) est appliqué globalement, complété par des styles personnalisés définis dans `www/styles.css`

L'application est divisée en plusieurs onglets :

1.  **Fiche Espèce :** Permet d'obtenir des informations détaillées sur une espèce
    * Une barre de recherche (`selectizeInput`) en haut permet de sélectionner l'espèce. Le nom commun et scientifique sont affichés à côté une fois l'espèce trouvée
    * Les résultats sont présentés en dessous :
        * La classification taxonomique (`uiOutput`) est affichée dans une boîte de texte 
        * Le portrait/description (`uiOutput`) est affiché dans une boîte similaire, avec une fonctionnalité "Lire la suite / Réduire" (via JavaScript) pour les textes longs
        * Une photographie (`uiOutput`) de l'espèce est affichée (si disponible via l'API)
    * Des indicateurs de chargement (`shinycssloaders`) sont présents sur les éléments dont la génération peut prendre du temps
2.  **Statuts de protection par espèce :** Fournit une vue cartographique des statuts
    * L'élément principal est une carte interactive (`leafletOutput`) affichant les régions de France
    * Un panneau de contrôle flottant (`absolutePanel`) contient un sélecteur d'espèces (`selectizeInput`), l'affichage des informations de base de l'espèce (`uiOutput`), un bouton pour afficher/masquer le tableau (`actionButton` géré par `shinyjs`), et un bouton de téléchargement (`downloadButton`)
    * Un second panneau (`absolutePanel`) apparaît en bas de la carte pour afficher un tableau détaillé (`DT::dataTableOutput`) des statuts de l'espèce sélectionnée
3.  **Observations :** Permet d'explorer les données d'observations.
    * Utilise une mise en page avec une barre latérale (`sidebarLayout`)
    * La barre latérale (`sidebarPanel`) contient les contrôles : sélection du groupe taxonomique (`selectInput`), sélection de la période (`dateRangeInput`), sélection du département (`selectInput`)
    * Le panneau principal (`mainPanel`) affiche :
        * Un texte explicatif statique et un texte dynamique résumant les sélections
        * Un ensemble d'onglets (`tabsetPanel`) pour visualiser :
            * La carte du département sélectionné, colorée par le nombre d'observations (`leafletOutput`), et le graphique camembert des niveaux de validation (`plotlyOutput`)
            * Le graphique de la tendance mensuelle des observations (`plotlyOutput`)
            * Le graphique de la répartition des taxons par habitat (`plotlyOutput`)
        * Des indicateurs de chargement (`shinycssloaders`) sont utilisés pour la carte et les graphiques
4.  **À Propos & Contact :** Contient des informations sur le projet, son contexte et les sources de données

L'application utilise également `shinyjs` pour des interactions dynamiques (comme afficher/masquer le tableau) et potentiellement du JavaScript personnalisé pour des fonctionnalités spécifiques comme le "Lire la suite". Le pied de page contient des liens et logos pertinents

---

### Style Visuel (`www/styles.css`)

L'apparence de l'application est basée sur le thème Shiny "darkly" (`shinythemes::shinytheme("darkly")`). Des personnalisations supplémentaires sont apportées via un fichier CSS dédié (`www/styles.css`) pour améliorer l'intégration visuelle et l'ergonomie :

* **Thème Sombre Général :** Définition d'un fond très sombre, d'une couleur de texte claire par défaut
* **Titres et Textes :** Styles personnalisés pour les titres (`h3`, `h4`, `h5`) et les blocs de texte explicatifs ou descriptifs (`.text-background-box`) avec des fonds légèrement contrastés et des bordures discrètes
* **Éléments d'Interface :**
    * Styles spécifiques pour les cartes Leaflet (`.leaflet`), les panneaux flottants (`#panel-flottant`), le tableau de données (`#panel-tableau table.dataTable`), la barre de navigation et les onglets (`.navbar-*`, `.nav-tabs > ...`), les étiquettes de contrôle (`.control-label`), et le pied de page (`.footer`)
* **Fonctionnalité "Lire la suite" :** Les règles CSS (`.collapsible-text`, `.read-more-link`, etc.) permettent de masquer une partie des descriptions longues et de les afficher/masquer via un lien, améliorant la lisibilité dans l'onglet "Fiche Espèce"

