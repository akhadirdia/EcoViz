library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(DT)
library(dplyr)
library(readxl)
library(readr)
library(sf)
library(mapview)
library(leaflet)
library(tidyverse)
library(pheatmap)
library(readxl)
library(readr)

df_emissions  = read_csv("C:/Users/diaak/Hackathon OSDP/registre_ges.csv")
datamap1 <- st_read("C:/Users/diaak/Hackathon OSDP/Quebec data/Bdga1m/SHP/regio_s.shp", quiet = T)
# Aggrégation des données par annee et region pour obtenir les totaux d'emissions
df_emissions_totales <- df_emissions %>%
  group_by(Annee, Region) %>%
  summarise(emissions_ges_totales = sum(Emissions_totales, na.rm = TRUE)) %>%
  ungroup()  # Retirer le regroupement pour éviter les comportements inattendus par la suite

#### Donnees des contaminants #########
df_contam  = read_csv("C:/Users/diaak/Hackathon OSDP/registre_contaminant.csv")
df_contam_filtre <- df_contam %>%
  group_by(Annee, Region) %>%
  summarise(Emissions_totales = sum(Emissions, na.rm = TRUE)) %>%
  ungroup()
df_contam_filtre$id <- as.integer(factor(df_contam_filtre$Region, levels = unique(df_contam_filtre$Region)))
######################################

##########Faune exotique ############
faune <- read_excel("C:/Users/diaak/Hackathon OSDP/BD_EAE_faunique_Quebec.xlsx")
df_count <- df %>% 
  count(region, groupe)

points_sf_faune <- st_as_sf(faune, coords = c("longitude", "latitude"), crs = st_crs(datamap1))
##################

############ Normales Mensuelles ##############
norm_mens  = read_csv("C:/Users/diaak/Hackathon OSDP/normales_mens.csv")
points_sf2_clim <- st_as_sf(norm_mens, coords = c("LONGITUDE", "LATITUDE"), crs = st_crs(datamap1))


accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

df_emissions_totales <- df_emissions_totales %>% accumulate_by(~Annee)

#####Import data for map ########


df1 <- df_emissions[!is.na(df_emissions$Longitude), ]
points_sf <- st_as_sf(df1, coords = c("Longitude", "Latitude"), crs = st_crs(datamap1))

###### QUALITE DE L'EAU ####################
qual_eau <- read_csv("C:/Users/diaak/Hackathon OSDP/Water_quality.csv")
qual_eau <- qual_eau %>% 
  filter(Province =='Quebec')
points_sf2 <- st_as_sf(qual_eau, coords = c("Longitude", "Latitude"), crs = st_crs(datamap1))
# Définir une palette de couleurs pour les différentes qualités d'eau
colors <- c("fair" = "blue", "excellent" = "green", "good" = "yellow", "marginal" = "orange", "poor" = "red")

# Définir l'interface utilisateur
ui <- dashboardPage(
  dashboardHeader(title = "ECOVIZ"),
  dashboardSidebar(
    sidebarMenu(
      id="tabs",
      menuItem("Accueil", tabName = "home", icon = icon("home")),
      menuItem("Emission GES", tabName = "emissions", icon = icon("cloud")),
      menuItem("Qualite de l'eau", tabName = "qual_eau", icon = icon("water")),
      menuItem("Emissions des contaminants", tabName = "contamin", icon = icon("cloud")),
      menuItem("Faune Exotique", tabName = "faune_exotic", icon = icon("fish")),
      menuItem("Normales Climatiques", tabName = "norm_clim", icon = icon("snowflake"))
     

      # menuItem("Accueil", tabName = "home"),
      # menuItem("Emission GES", tabName = "emissions")
      # Ajoutez plus de menus ici
    )
  ),
  dashboardBody(
    tabItems(
      # Premier onglet
      tabItem(tabName = "home",
              fluidRow(
                tabBox(
                  title = "Emissions de GES au Quebec",height = "560px",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1", 
                  tabPanel("GES PLOT", selectInput("regionInput_1", "Choisissez une ou plusieurs régions:",
                  choices = unique(df_emissions_totales$Region), multiple = TRUE, selected = c("Estrie", "Mauricie")),
                  plotlyOutput("plot2")),
                  
                  tabPanel("Emissions de GES ?", "Qu'est ce que les Emissions de GES ?", helpText("Les émissions de gaz à effet de serre (GES) sont des composés chimiques présents naturellement dans l'atmosphère, tels que le dioxyde de carbone (CO2), le méthane (CH4) et l'oxyde nitreux (N2O). Ils jouent un rôle crucial dans le maintien de la température de la Terre en emprisonnant la chaleur du soleil, un phénomène connu sous le nom d'effet de serre. Cependant, les activités humaines ont entraîné une augmentation significative des concentrations de GES dans l'atmosphère, provoquant un réchauffement climatique et des changements climatiques à l'échelle mondiale.

Les principales sources d'émissions de GES sont liées aux activités humaines, notamment la combustion de combustibles fossiles pour la production d'énergie et le transport, l'agriculture (notamment l'élevage de bétail et la gestion des déchets agricoles), la déforestation et la production industrielle.

Les conséquences du changement climatique dû aux émissions de GES sont multiples et variées. Elles incluent l'élévation du niveau des mers due à la fonte des glaces polaires et des glaciers, l'augmentation de la fréquence et de l'intensité des phénomènes météorologiques extrêmes tels que les tempêtes, les sécheresses et les inondations, la perturbation des écosystèmes et des cycles naturels, ainsi que les effets sur la santé humaine tels que les maladies respiratoires et les maladies vectorielles."))
                ),
                tabBox(
                  selected = "GES MAP", height = "550px",
                  tabPanel("GES MAP", "Emissions de GES totales par annee a Quebec", selectInput("annee_input", "Choisissez une annee a afficher:",
                  choices = unique(points_sf$Annee), selected = 2012), leafletOutput("map1")),
                  tabPanel("Qualité de l'eau", "Qualite de l'eau de la province de Quebec", leafletOutput("map2"))
                )
              ),
              
              fluidRow(
                tabBox(
                 height = "550px",
                selected = "Emissions de Contaminants",
                tabPanel("Emissions de Contaminants", "Emissions totales des contaminants en tonnes", selectInput("annee_input_1", "Choisissez une annee a afficher:",
                        choices = unique(df_contam_filtre$Annee), selected = 2012), plotOutput("plot3")),
                tabPanel("Emissions de Contaminants ?", "Qu'est ce que les Emissions de Contaminants", helpText("Les émissions de contaminants, qu'ils soient d'origine naturelle ou anthropique, ont un impact significatif sur notre environnement et notre santé. Ces substances nocives peuvent prendre différentes formes, telles que les polluants atmosphériques, les produits chimiques toxiques, les déchets industriels et les polluants de l'eau et du sol.

Les causes des émissions de contaminants sont multiples et diverses. Les activités industrielles, l'agriculture intensive, les transports, la production d'énergie, les déchets et la combustion de combustibles fossiles sont parmi les principales sources de contaminants dans l'environnement. Les rejets non contrôlés de substances toxiques peuvent entraîner une pollution généralisée et des risques pour la santé humaine et les écosystèmes.

Les conséquences de la pollution par les contaminants sont graves et variées. Ils peuvent entraîner une dégradation de la qualité de l'air, de l'eau et du sol, menacer la biodiversité, contaminer les ressources en eau potable, et causer des maladies respiratoires, des troubles neurologiques, des cancers et d'autres problèmes de santé chez les êtres humains et les animaux."))
              ),
              tabBox(
                height = "550px",
                selected = "Faune Exotique",
                tabPanel("Faune Exotique", "Faune Exotique",selectInput("region_input2", "Choisissez une region a etudier:",
                choices = unique(faune$region), selected = 'Bas-Saint-Laurent'), plotOutput("plot4")),
                tabPanel("Info", "Qu'est ce que la faune exotique", helpText("Une espèce aquatique envahissante est un végétal, un animal ou un micro-organisme (virus, bactérie ou champignon) qui est introduit hors de son aire de répartition naturelle, qui colonise de nouveaux sites ou de nouvelles régions à un rythme rapide et qui peut former des populations dominantes. Son établissement ou sa propagation peuvent constituer une menace pour l’environnement, l’économie ou la société. ###Les observations proviennent de sources variées telles que : * les échantillonnages scientifiques effectués par le MELCCFP et par d’autres organismes gouvernementaux ou à but non lucratif; * les mentions de citoyens ayant communiqué directement avec le service à la clientèle du MELCCFP ou au moyen d’outils comme Sentinelle et iNaturalist; * les captures accidentelles de pêcheurs commerciaux membres du Réseau de détection précoce des espèces aquatiques exotiques envahissantes du Saint Laurent."))
              ))


      ),
      # Deuxième onglet
      tabItem(tabName = "emissions",
              fluidRow(
                box(title = "Emission de GES par Secteur", status = "primary", solidHeader = TRUE, width = 12, 
                selectInput("regionInput", "Choisissez une ou plusieurs régions et cliquer sur play pour suivre l'evolution des emissions de GES",
                choices = unique(df_emissions_totales$Region), multiple = TRUE, selected = c("Estrie", "Laval", "Mauricie")),
                 plotlyOutput("plot1")),
                # Ajouter d'autres éléments ici
              ),
              fluidRow(
                box(title = "Tableau des Emission de GES du Quebec", status = "primary", solidHeader = TRUE, width = 12, 
                    dataTableOutput("tab1")),
              )
      ),
      tabItem(tabName = "qual_eau",
              fluidRow(
                box(title = "Qualite de l'eau", status = "primary", solidHeader = TRUE, width = 12, leafletOutput("map3")),
              ),
              fluidRow(
                box(title = "Tableau de la Qualite de l'eau des Provinces du Canada", status = "primary", solidHeader = TRUE, width = 12, 
                    dataTableOutput("tab2")),
              )
              ),
      tabItem(tabName = "contamin",
              fluidRow(
                box(title = "Emissions des contaminants", status = "primary", solidHeader = TRUE, width = 12, selectInput("annee_input_2", "Choisissez une annee a afficher:",
                choices = unique(df_contam_filtre$Annee), selected = 2012), plotOutput("plot5")),
              )
      ),
      tabItem(tabName = "faune_exotic",
              fluidRow(
                box(title = "Faune Exotique", status = "primary", solidHeader = TRUE, width = 12, selectInput("groupe", "Choisissez uen categorie de faune:",
                choices = unique(points_sf_faune$groupe), selected = 'poisson'), leafletOutput("map4")),
              )
      ),
      tabItem(tabName = "norm_clim",
              fluidRow(
                box(title = "Normles Climatiques Mensuelles", status = "primary", solidHeader = TRUE, width = 12, 
                    helpText("Selon les normes de l’OMM, une normale est la moyenne arithmétique calculée pour chaque mois de l’année à partir des données climatiques enregistrées quotidiennement sur une période de 30 ans. La moyenne ou le total de ces normales mensuelles, selon la donnée climatique considérée, constitue la normale annuelle1. En plus des normales mensuelles et annuelles, certaines normales quotidiennes s’avèrent utiles et sont donc calculées.
                   Les périodes climatologiques de 30 ans, définies pour le calcul des normales, débutent le 1er janvier au début d’une décennie et se terminent le 31 décembre, 30 ans plus tard1. Par exemple, les normales de la période 1981-2010 ont été établies à partir des données mesurées du 1er janvier 1981 au 31 décembre 2010. La mise à jour des normales devrait donc avoir lieu toutes les décennies (voir la figure 1).

                    Des cartes représentant les normales mensuelles et annuelles de la période 1981-2010 pour la température de l’air (minimale, maximale et moyenne) et la précipitation (pluie, neige et précipitation totale) sont disponibles sur notre site Web. D’autre part, vous pouvez également consulter la liste détaillée (PDF, 37 ko) de toutes les données disponibles sous forme de normales."),
                selectInput("normale", "Selectionner la variable a etudier:", choices = c('NEIGE_NORMALE', 'PLUIE_NORMALE', 'PREC_NORMALE', 'TMAX_NORMALE', 'TMIN_NORMALE', 'TMOY_NORMALE'), selected = 'NEIGE_NORMALE'), 
                selectInput("mois", "Selectionner le mois",
                choices = unique(norm_mens$MOIS), selected = 1), leafletOutput("map5")),
              )
      )
      
      # Ajouter d'autres onglets ici
    )
  )
)

# Définir la logique côté serveur
server <- function(input, output) {
  output$plot1 <- renderPlotly({
    # Filtrer les données basées sur la sélection de l'utilisateur
    filtered_data <- df_emissions_totales %>% 
      filter(Region %in% input$regionInput) %>% 
      arrange(Region, Annee)
    
    # Créer le graphique Plotly
    fig <- filtered_data %>%
      plot_ly(
        x = ~Annee, 
        y = ~emissions_ges_totales,
        split = ~Region,
        frame = ~frame, 
        type = 'scatter',
        mode = 'lines', 
        line = list(simplyfy = F)
      )
    fig <- fig %>% layout(
      xaxis = list(
        title = "Annee",
        zeroline = F
      ),
      yaxis = list(
        title = "emissions_ges_totales",
        zeroline = F
      )
    ) 
    fig <- fig %>% animation_opts(
      frame = 500, 
      transition = 0, 
      redraw = TRUE
    )
    fig <- fig %>% animation_slider(
      hide = T
    )
    fig <- fig %>% animation_button(
      x = 1, xanchor = "right", y = 0, yanchor = "bottom"
    )
    
    fig
  })
  
  
  output$plot2 <- renderPlotly({
    # Filtrer les données basées sur la sélection de l'utilisateur
    filtered_data <- df_emissions_totales %>% 
      filter(Region %in% input$regionInput_1) %>% 
      arrange(Region, Annee)
    
    # Créer le graphique Plotly
    p <- plot_ly(filtered_data, x = ~Annee, y = ~emissions_ges_totales, color = ~Region, 
                 type = 'scatter', mode = 'lines+markers', 
                 marker = list(size = 10), line = list(simplify = FALSE)) %>%
      layout(title = "Évolution des émissions de GES par région",
             xaxis = list(title = "Année"),
             yaxis = list(title = "Émissions de GES"))
    p
  })
  
  
  output$map1<-renderLeaflet({
    points_sf_filtre <- points_sf %>% 
      filter(Annee==input$annee_input) 
    map <- mapview(datamap1, zcol = "RES_NM_REG", legend = FALSE)@map
    # Ajoutez les points, ajustez la taille des points selon la population
    # Utilisez la fonction log10 pour une meilleure répartition des tailles des points si les populations varient grandement
    points_map <- mapview(points_sf, zcol = "Emissions_totales", cex = 'Emissions_totales', legend = TRUE)@map
    #points_map
    # Superposez les points sur la carte des régions
    #final_map <- map@map + points_map@map
    # # Affichez la carte finale
    #final_map@map
    (mapview(datamap1, zcol = "RES_NM_REG", legend = FALSE) + mapview(points_sf_filtre, zcol = "Emissions_totales", cex = 'Emissions_totales', legend = TRUE))@map

  })
  
  output$map2<-renderLeaflet({
    
    (mapview(datamap1, zcol = "RES_NM_REG", legend = FALSE) + mapview(points_sf2, zcol = "Category", col.regions = colors, legend = TRUE))@map
    
  })
  
  output$map3<-renderLeaflet({
    
    (mapview(datamap1, zcol = "RES_NM_REG", legend = FALSE) + mapview(points_sf2, zcol = "Category", col.regions = colors, legend = TRUE))@map
    
  })
  
  output$map4<-renderLeaflet({
    points_sf_filtre4 <- points_sf_faune %>% 
      filter(groupe==input$groupe) 
    
    (mapview(datamap1, zcol = "RES_NM_REG", legend = FALSE) + mapview(points_sf_filtre4, zcol = "groupe", col.regions = colors, legend = TRUE))@map
    
  })
  
  output$map5<-renderLeaflet({
    points_sf_filtre5 <- points_sf2_clim %>% 
      filter(MOIS==input$mois) 
    (mapview(datamap1, zcol = "RES_NM_REG", legend = FALSE) + mapview(points_sf_filtre5, zcol = input$normale, cex = input$normale, legend = TRUE))@map
    
  })

  output$plot3 <-renderPlot({
    df_contam_filtre2 <- df_contam_filtre %>% 
      filter(Annee ==input$annee_input_1)
    number_of_bar <- nrow(df_contam_filtre2)
    angle <-  90 - 360 * (df_contam_filtre2$id-0.5) /number_of_bar
    # calculate the alignment of labels: right or left
    # If I am on the left part of the plot, my labels have currently an angle < -90
    df_contam_filtre2$hjust<-ifelse( angle < -90, 1, 0)
        # flip angle BY to make them readable
    df_contam_filtre2$angle<-ifelse(angle < -90, angle+180, angle)
        # Start the plot
    p <- ggplot(df_contam_filtre2, aes(x=as.factor(id), y=Emissions_totales)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
            # This add the bars with a blue color
      geom_bar(stat="identity", fill=alpha("skyblue", 0.7)) +
            # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
      ylim(-40000,330000) +
            # Custom the theme: no axis title and no cartesian grid
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4), "cm")      # Adjust the margin to make in sort labels are not truncated!
      ) +
            # This makes the coordinate polar instead of cartesian.
      coord_polar(start = 0) +
            # Add the labels, using the label_data dataframe that we have created before
      geom_text(data=df_contam_filtre2, aes(x=id, y=Emissions_totales+10, label=Region, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= df_contam_filtre2$angle, inherit.aes = FALSE ) 
      p
  })
  
  output$plot4 <- renderPlot({
    filtered_data <- df_count %>% 
      filter(region %in% input$region_input2)
    data <-filtered_data
    # Compute percentages
    data$fraction <- data$n / sum(data$n)
    
    # Compute the cumulative percentages (top of each rectangle)
    data$ymax <- cumsum(data$fraction)
    
    # Compute the bottom of each rectangle
    data$ymin <- c(0, head(data$ymax, n=-1))
    
    # Compute label position
    data$labelPosition <- (data$ymax + data$ymin) / 2
    
    # Compute a good label
    data$label <- paste0(data$groupe, "\n value: ", data$n)
    
    # Make the plot
    ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=groupe)) +
      geom_rect() +
      geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
      scale_fill_brewer(palette=4) +
      coord_polar(theta="y") +
      xlim(c(2, 4)) +
      theme_void() +
      theme(legend.position = "none")
  })
  
  output$plot5 <-renderPlot({
    df_contam_filtre2 <- df_contam_filtre %>% 
      filter(Annee ==input$annee_input_2)
    number_of_bar <- nrow(df_contam_filtre2)
    angle <-  90 - 360 * (df_contam_filtre2$id-0.5) /number_of_bar
    # calculate the alignment of labels: right or left
    # If I am on the left part of the plot, my labels have currently an angle < -90
    df_contam_filtre2$hjust<-ifelse( angle < -90, 1, 0)
    # flip angle BY to make them readable
    df_contam_filtre2$angle<-ifelse(angle < -90, angle+180, angle)
    # Start the plot
    p <- ggplot(df_contam_filtre2, aes(x=as.factor(id), y=Emissions_totales)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
      # This add the bars with a blue color
      geom_bar(stat="identity", fill=alpha("skyblue", 0.7)) +
      # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
      ylim(-100000,330000) +
      # Custom the theme: no axis title and no cartesian grid
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4), "cm")      # Adjust the margin to make in sort labels are not truncated!
      ) +
      # This makes the coordinate polar instead of cartesian.
      coord_polar(start = 0) +
      # Add the labels, using the label_data dataframe that we have created before
      geom_text(data=df_contam_filtre2, aes(x=id, y=Emissions_totales+10, label=Region, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= df_contam_filtre2$angle, inherit.aes = FALSE ) 
    p
  })
  
  output$tab1 = renderDataTable({
  
    datatable (df_emissions,  class = "hover cell-border compact", options = list(
      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json'), scrollX = TRUE,                # Activation du défilement horizontal
      pageLength = 3, autoWidth = TRUE),
      caption = "Tableau des emissions de GES du Quebec") %>%
      formatStyle(               # Fonction pour changer le style d'une colonne
        #c("Identifiant",	"Annee",	"Entreprise",	"Etablissement",	"Adresse",	"Region",	"Mun",	"Emissions_totales",	"Emissions_bio_combustion",	"Emissions_bio_autres",	"Emissions_totales_sans_bio"),                 # Nom de la colonne à modifier
        colnames(df_emissions),
        color = 'green',         # Couleur du texte dans la colonne
        backgroundColor = 'white', # Couleur des cases de la colonne
        fontWeight = 'bold'      # Affiche les caractères en gras
      )
      }
  )
  
  output$tab2 = renderDataTable({
    
    datatable (qual_eau,  class = "hover cell-border compact", options = list(
      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json'), scrollX = TRUE,                # Activation du défilement horizontal
      pageLength = 3, autoWidth = TRUE),
      caption = "Tableau des emissions de GES du Quebec") %>%
      formatStyle(
        colnames(qual_eau),
        color = 'green',         # Couleur du texte dans la colonne
        backgroundColor = 'white', # Couleur des cases de la colonne
        fontWeight = 'bold'      # Affiche les caractères en gras
      )
  }
  )

}

# Exécuter l'application
shinyApp(ui, server)





