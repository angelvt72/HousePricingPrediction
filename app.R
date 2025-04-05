# Load required libraries
library(dplyr)
library(shiny)
library(shinydashboard)
library(leaflet)
library(rnaturalearthdata)
library(rnaturalearth) # world map using rnaturalearth
library(rmapshaper) # for performance using ms_simplify
library(sf)
library(ggplot2)
library(plotly)
library(shinycssloaders)
library(shinydashboardPlus)
library(htmltools)
library(scales)


library(tidyverse)  # For data manipulation
library(caret)      # For modeling and cross-validation
library(glmnet)     # For Ridge regression
library(MLmetrics)  # For evaluation metrics
library(e1071)      # For data preprocessing
library(readr)      # For reading CSV files


library(car)

library(shinyWidgets)
options(warn=-1)


calculate_profitability <- function(purchase_price, rent_price) {
  
  # Calculate annual gross profitability
  
  annual_rent_price <- rent_price * 12
  gross_profitability <- (annual_rent_price / purchase_price) * 100
  
  return(gross_profitability)
}

predict_price <- function(localizacion ,  ascensor, baños, trastero, piso, 
                          habitaciones, metros_reales, condicion, 
                          armarios_empotrados, terraza, garaje, 
                          aire_acondicionado, calefaccion, balcon,
                          df_casas,
                          model) {
  
  # Create a list with input parameters
  data <- list(
    ascensor = ascensor,
    baños = baños,
    trastero = trastero,
    piso = piso,
    habitaciones = habitaciones,
    metros_reales = metros_reales,
    condicion = condicion,
    armarios_empotrados = armarios_empotrados,
    terraza = terraza,
    garaje = garaje,
    aire_acondicionado = aire_acondicionado,
    calefaccion = calefaccion,
    balcon = balcon
  )
 
  # Convert the list to a data frame
  data <- as.data.frame(data)
  
  # Convert factors to match levels in df_casas

  data$piso <- factor(data$piso, levels = levels(df_casas$piso))
  data$condicion <- factor(data$condicion, levels = levels(df_casas$condicion))
  
  # One-hot encoding
  testData_encoded <- dummyVars(~ ., data = data) %>%
    predict(newdata = data) %>%
    as.data.frame()
  
  

  # Predict using the ridge model
  prediction_compra <- as.integer(predict(model, newx = as.matrix(testData_encoded)))
 
  return(prediction_compra)
}


location_options <- list(
  "Cartagena" = list(
    "Cartagena" = c(
      "Alameda", 
      "Casco Antiguo",
      "Ciudad Jardín",
      "Ensanche", 
      "La Concepción", 
      "Los Barreros-Cuatro Santos",
      "Peral-La Vereda",
      "San Antón", 
      "San Ginés-Virgen de la Caridad", 
      "Santa Lucía"
    ),
    "El Algar" = c("El Algar"),
    "Pozo Estrecho" = c("Pozo Estrecho"),
    "La Azohía" = c("La Azohía"),
    "Santa Ana-Miranda" = c("Santa Ana-Miranda"),
    "Canteras" = c("Canteras"),
    "La Aljorra" = c("La Aljorra"),
    "La Magdalena" = c("La Magdalena"),
    "Lentiscar" = c("Lentiscar"),
    "Albujón" = c("Albujón"),
    "El Beal-Estrecho de San Ginés" = c("El Beal-Estrecho de San Ginés"),
    "Alumbres-Escombreras" = c("Alumbres-Escombreras"),
    "Hondón-Torreciega" = c("Hondón-Torreciega"),
    "El Plan" = c(
      "Los Dolores-Los Gabatos",
      "El Plan-Polígono de Santa Ana",
      "Hispanoamérica-Universidad",
      "San Cristóbal"
    ),
    "Mar de Cristal-Cabo de Palos" = c(
      "Playa Honda-Playa Paraíso",
      "Islas Menores-Mar de Cristal",
      "Los Nietos",
      "La Manga Club-Atamaría",
      "Cabo de Palos",
      "Los Belones"
    ),
    "Perín" = c(
      "Galifa",
      "Cuesta Blanca",
      "Centro",
      "La Azohía"
    )
  ),
  "Fuente Álamo" = list(
    "Fuente Álamo" = c(
      "Hacienda del Álamo Golf", 
      "La Pinilla - Las Palas", 
      "Centro", 
      "Cánovas - Cuevas de Reyllo", 
      "Balsapintada - El Estrecho", 
      "Los Almagros - Los Paganes - El Escobar"
    )
  ),
  "La Unión" = list(
    "La Unión" = c(
      "La Unión",
      "Portmán",
      "Roche"
    )
  ),
  "Mar Menor" = list(
    "Torre-Pacheco" = c(
      "Torre-Pacheco", 
      "Roldán", 
      "Dolores de Pacheco-Santa Rosalía", 
      "Mar Menor Golf Resort", 
      "Balsicas", 
      "San Cayetano", 
      "El Jimenado"
    ),
    "Los Alcázares" = c(
      "Los Alcázares", 
      "Los Narejos-Punta Calera", 
      "Las Lomas del Rame-Bahía Bella"
    ),
    "San Javier" = c(
      "Santiago de la Ribera", 
      "San Javier", 
      "Roda", 
      "El Mirador-Pozo Aledo"
    ),
    "San Pedro del Pinatar" = c(
      "San Pedro del Pinatar", 
      "Lo Pagán", 
      "Los Cuarteros", 
      "Los Peñascos-El Salero-Los Imbernones", 
      "El Mojón-Las Salinas"
    )
  ),
  "La Manga del Mar Menor" = list(
    "La Manga del Mar Menor" = c(
      "Playa de las Gaviotas-El Pedrucho", 
      "Playa del Esparto-Veneziola", 
      "Playa del Galán", 
      "Zona Entremares", 
      "Zona Galúa-Calnegre"
    )
  ),
  "Bajo Guadalentín" = list(
    "Librilla" = c("Librilla"),
    "Aledo" = c("Aledo"),
    "Totana" = c("Totana"),
    "Alhama de Murcia" = c(
      "Alhama de Murcia", 
      "Condado de Alhama", 
      "Sierra de Carrascoy"
    ),
    "Mazarrón" = c("Mazarrón", "Bolnuevo"),
    "Puerto de Mazarrón" = c(
      "Bahía", 
      "Cuatro Plumas-La Cumbre", 
      "El Alamillo", 
      "Playa Grande-Castellar", 
      "Playa Sol", 
      "Puerto"
    )
  ),
  "Alto Guadalentín" = list(
    "Puerto Lumbreras" = c("Puerto Lumbreras"),
    "Águilas" = c(
      "Calabardina-Cope", 
      "Centro", 
      "Hornillo", 
      "Isla del Fraile", 
      "Las Delicias", 
      "Las Lomas-Las Yucas", 
      "Las Majadas-Las Molinetas-Labradorcico", 
      "Los Collados-Los Geráneos", 
      "Tébar - Los Arejos"
    ),
    "Lorca" = c(
      "La Hoya-Almendricos-Purias", 
      "La Viña-San José", 
      "San Diego-Los Ángeles-San Cristóbal", 
      "Zarcilla de Ramos-Doña Inés", 
      "Zona Centro-Corredera"
    )
  ),
  "Noroeste" = list(
    "Bullas" = c("Bullas"),
    "Calasparra" = c("Calasparra"),
    "Caravaca de la Cruz" = c("Caravaca de la Cruz"),
    "Cehegín" = c("Cehegín"),
    "Moratalla" = c("Moratalla")
  ),
  "Valle de Ricote" = list(
    "Archena" = c("Archena"),
    "Villanueva del Rio Segura" = c("Villanueva del Rio Segura"),
    "Ricote" = c("Ricote"),
    "Ulea" = c("Ulea"),
    "Ojós" = c("Ojós")
  ),
  "Río Mula" = list(
    "Mula" = c("Mula"),
    "Pliego" = c("Pliego"),
    "Campos del Río" = c("Campos del Río"),
    "Albudeite" = c("Albudeite")
  ),
  "Vega Alta" = list(
    "Cieza" = c("Cieza"),
    "Abarán" = c("Abarán"),
    "Blanca" = c("Blanca")
  ),
  "Altiplano Oriental" = list(
    "Yecla" = c("Yecla"),
    "Fortuna" = c("Fortuna"),
    "Jumilla" = c("Jumilla"),
    "Abanilla" = c("Abanilla")
  ),
  "Vega Media" = list(
    "Las Torres de Cotillas" = c("Las Torres de Cotillas"),
    "Alguazas" = c("Alguazas"),
    "Ceutí" = c("Ceutí"),
    "Lorquí" = c("Lorquí"),
    "Molina de Segura" = c(
      "Centro", 
      "El Castillo-San Roque-El Carmen", 
      "El Llano", 
      "Fátima-El Panderón", 
      "Los Vientos-Casa Ros", 
      "Sagrado Corazón", 
      "San Antonio-San José-Los Ángeles", 
      "San Miguel", 
      "Torrealta-Ribera de Molina"
    ),
    "El Romeral" = c(
      "Altorreal-El Chorrico", 
      "La Alcayna", 
      "Los Conejos", 
      "La Quinta", 
      "El Pino"
    ),
    "Campotéjar-Los Valientes" = c("Campotéjar-Los Valientes")
  ),
  "Murcia" = list(
    "Norte" = c("Juan de Borbón", "Juan Carlos I", "Santa María de Gracia", "El Ranero-San Basilio", "Vista Alegre", "La Flota"),
    "Pedanías Este" = c("Algezares", "Alquerías", "Beniaján", "Casillas", "El Raal", "Garres y Lages", "La Alberca", "Llano de Brujas", "Los Dolores", "Los Ramos", "Puente Tocinos", "San Benito-Patiño", "San Benito-Progreso", "San José de la Vega", "Santa Cruz", "Santo Ángel", "Torreagüera", "Zarandona", "Zeneta"),
    "Pedanías Oeste" = c("El Palmar", "Sangonera la Verde", "Guadalupe", "Aljucer", "Sangonera la Seca", "La Ñora", "Javalí Nuevo", "Puebla de Soto", "Era Alta", "La Raya", "San Ginés", "Nonduermas", "Javalí Viejo", "La Albatalía", "Rincón de Seca", "La Arboleja", "Rincón de Beniscornia"),
    "Centro" = c("San Miguel", "La Paz", "La Fama", "San Andrés-San Antolín", "Santa Catalina-San Bartolomé", "San Lorenzo", "San Antón", "Santa Eulalia", "San Juan", "San Nicolás", "Catedral", "Vistabella", "San Pedro"),
    "Campo de Murcia" = c("Gea y Truyols", "Sucina", "Baños y Mendigo", "Jerónimo y Avileses", "Lobosillo", "Corvera", "Cañadas de San Pedro", "Los Martínez del Puerto", "Valladolises y Lo Jurado"),
    "Pedanías Norte" = c("Espinardo", "Cabezo de Torres", "El Esparragal", "Churra", "Cobatillas", "El Puntal", "Monteagudo"),
    "Sur" = c("El Carmen", "La Purísima-Barriomar", "Infante Juan Manuel", "Santiago el Mayor", "Ronda Sur", "San Pío X")
  )
)

all_locations <- list()

# Loop through each city
for (city in names(location_options)) {
  # Loop through each region within the city
  for (region in names(location_options[[city]])) {
    # Add locations from the current region to the all_locations list
    all_locations <- c(all_locations, location_options[[city]][[region]])
  }
}
locations <- unlist(all_locations)

# buy/sale
df_casas_compra = read.csv("data/MURCIA_FILTRADO_Modified.csv", encoding='utf-8', sep=';')
df_casas_compra$localizacion = df_casas_compra$newLocation



# df_casas_compra$titulo = as.factor(df_casas_compra$titulo)
df_casas_compra$localizacion = as.factor(df_casas_compra$localizacion)
df_casas_compra$piso = as.factor(df_casas_compra$piso)
df_casas_compra$condicion = as.factor(df_casas_compra$condicion)

compra_model <- readRDS(file = "Model/compra_ridge.rds")






# rent
df_casas_alquiler = read.csv("data/alquiler_MURCIA_FILTRADO_Modified.csv", encoding='utf-8', sep=';')
df_casas_alquiler$localizacion = df_casas_alquiler$newLocation

# df_casas_alquiler$titulo = as.factor(df_casas_alquiler$titulo)
df_casas_alquiler$localizacion = as.factor(df_casas_alquiler$localizacion)
df_casas_alquiler$piso = as.factor(df_casas_alquiler$piso)
df_casas_alquiler$condicion = as.factor(df_casas_alquiler$condicion)
alquiler_model <- readRDS(file = "Model/alquiler_ridge.rds")


# predicted_prices <- apply(df_casas_alquiler, 1, function(row) {
#   predict_price(
#     
#     ascensor = as.integer(row["ascensor"]),                     # Convert to integer
#     baños = as.integer(row["baños"]),                           # Convert to integer
#     trastero = as.integer(row["trastero"]),                     # Convert to integer
#     piso = as.character(row["piso"]),                           # Convert to character
#     habitaciones = as.integer(row["habitaciones"]),              # Convert to integer
#     metros_reales = as.numeric(row["metros_reales"]),           # Convert to numeric
#     condicion = as.character(row["condicion"]),                 # Convert to character
#     armarios_empotrados = as.integer(row["armarios_empotrados"]), # Convert to integer
#     terraza = as.integer(row["terraza"]),                        # Convert to integer
#     garaje = as.integer(row["garaje"]),                          # Convert to integer
#     aire_acondicionado = as.integer(row["aire_acondicionado"]),  # Convert to integer
#     calefaccion = as.integer(row["calefaccion"]),                # Convert to integer
#     balcon = as.integer(row["balcon"]),       
#     df_casas_compra,
#     compra_model
#   )
# })
# 
# df_casas_alquiler$buy_sell_price <- predicted_prices
# 
# df_casas_alquiler$gross_profitability <- apply(df_casas_alquiler, 1, function(row) {
#   calculate_profitability(as.integer( row['buy_sell_price']), as.integer( row['precio']))
# })



## profitability for buy_sell


# predicted_prices <- apply(df_casas_compra, 1, function(row) {
#   predict_price(
#     
#     ascensor = as.integer(row["ascensor"]),                     # Convert to integer
#     baños = as.integer(row["baños"]),                           # Convert to integer
#     trastero = as.integer(row["trastero"]),                     # Convert to integer
#     piso = as.character(row["piso"]),                           # Convert to character
#     habitaciones = as.integer(row["habitaciones"]),              # Convert to integer
#     metros_reales = as.numeric(row["metros_reales"]),           # Convert to numeric
#     condicion = as.character(row["condicion"]),                 # Convert to character
#     armarios_empotrados = as.integer(row["armarios_empotrados"]), # Convert to integer
#     terraza = as.integer(row["terraza"]),                        # Convert to integer
#     garaje = as.integer(row["garaje"]),                          # Convert to integer
#     aire_acondicionado = as.integer(row["aire_acondicionado"]),  # Convert to integer
#     calefaccion = as.integer(row["calefaccion"]),                # Convert to integer
#     balcon = as.integer(row["balcon"]),       
#     df_casas_alquiler,
#     alquiler_model
#   )
# })
# 
# df_casas_compra$rent_price <- predicted_prices
# 
# 
# df_casas_compra$gross_profitability <- apply(df_casas_compra, 1, function(row) {
#   calculate_profitability(as.integer( row['rent_price']), as.integer( row['precio']))
# })

# print(colnames(df_casas_alquiler))
# 
# print(colnames(df_casas_compra))

cities_sf <- st_read("shapeFile/MUNICIPIOS__MU_2014.shp")

yes_no_options <- c('Yeah' = 1, 'No' = 0)
# yes_no_options = {0: "No", 1: "Sí"}

floor_options <- c(
  "Primeros pisos" = "Primero",
  "Últimos pisos" = "Ultimos_pisos",
  "Bajo" = "Bajo",
  "Muchas plantas" = "Muchas_plantas" ,
  "Intermedios" = "Intermedios" ,
  "Otro" = "Otro"
)

condition_options <- c(
  "Segunda mano / en buen estado" = "Segunda mano/buen estado" ,
  "Segunda mano / para reformar" = "Segunda mano/para reformar",
  "Promoción de obra nueva" = "Promoción de obra nueva" 
)




# # city -> areas,...
# location_options <- list(
  # "Norte" = c("Juan de Borbón", "Juan Carlos I", "Santa María de Gracia", "El Ranero-San Basilio", "Vista Alegre", "La Flota"),
  # "Pedanías Este" = c("Algezares", "Alquerías", "Beniaján", "Casillas", "El Raal", "Garres y Lages", "La Alberca", "Llano de Brujas", "Los Dolores", "Los Ramos", "Puente Tocinos", "San Benito-Patiño", "San Benito-Progreso", "San José de la Vega", "Santa Cruz", "Santo Ángel", "Torreagüera", "Zarandona", "Zeneta"),
  # "Pedanías Oeste" = c("El Palmar", "Sangonera la Verde", "Guadalupe", "Aljucer", "Sangonera la Seca", "La Ñora", "Javalí Nuevo", "Puebla de Soto", "Era Alta", "La Raya", "San Ginés", "Nonduermas", "Javalí Viejo", "La Albatalía", "Rincón de Seca", "La Arboleja", "Rincón de Beniscornia"),
  # "Centro" = c("San Miguel", "La Paz", "La Fama", "San Andrés-San Antolín", "Santa Catalina-San Bartolomé", "San Lorenzo", "San Antón", "Santa Eulalia", "San Juan", "San Nicolás", "Catedral", "Vistabella", "San Pedro"),
  # "Campo de Murcia" = c("Gea y Truyols", "Sucina", "Baños y Mendigo", "Jerónimo y Avileses", "Lobosillo", "Corvera", "Cañadas de San Pedro", "Los Martínez del Puerto", "Valladolises y Lo Jurado"),
  # "Pedanías Norte" = c("Espinardo", "Cabezo de Torres", "El Esparragal", "Churra", "Cobatillas", "El Puntal", "Monteagudo"),
  # "Sur" = c("El Carmen", "La Purísima-Barriomar", "Infante Juan Manuel", "Santiago el Mayor", "Ronda Sur", "San Pío X")
# )
# 
# 
# # areas -> locations,....
# city_options <- list(
#   "Juan de Borbón" = c("Juan de Borbón", "Juan Carlos I", "Santa María de Gracia", "El Ranero-San Basilio", "Vista Alegre", "La Flota"),
#   "Juan Carlos I" = c("Algezares", "Alquerías", "Beniaján", "Casillas", "El Raal", "Garres y Lages", "La Alberca", "Llano de Brujas", "Los Dolores", "Los Ramos", "Puente Tocinos", "San Benito-Patiño", "San Benito-Progreso", "San José de la Vega", "Santa Cruz", "Santo Ángel", "Torreagüera", "Zarandona", "Zeneta"),
#   "Santa María de Gracia" = c("El Palmar", "Sangonera la Verde", "Guadalupe", "Aljucer", "Sangonera la Seca", "La Ñora", "Javalí Nuevo", "Puebla de Soto", "Era Alta", "La Raya", "San Ginés", "Nonduermas", "Javalí Viejo", "La Albatalía", "Rincón de Seca", "La Arboleja", "Rincón de Beniscornia"),
#   "Centro" = c("San Miguel", "La Paz", "La Fama", "San Andrés-San Antolín", "Santa Catalina-San Bartolomé", "San Lorenzo", "San Antón", "Santa Eulalia", "San Juan", "San Nicolás", "Catedral", "Vistabella", "San Pedro"),
#   "Campo de Murcia" = c("Gea y Truyols", "Sucina", "Baños y Mendigo", "Jerónimo y Avileses", "Lobosillo", "Corvera", "Cañadas de San Pedro", "Los Martínez del Puerto", "Valladolises y Lo Jurado"),
#   "Pedanías Norte" = c("Espinardo", "Cabezo de Torres", "El Esparragal", "Churra", "Cobatillas", "El Puntal", "Monteagudo"),
#   "Sur" = c("El Carmen", "La Purísima-Barriomar", "Infante Juan Manuel", "Santiago el Mayor", "Ronda Sur", "San Pío X")
# )




# cites
# names(location_options)
# regions
# names(location_options[["Cartagena"]])
# locations
# location_options[["Cartagena"]][["Cartagena"]]

##################
# USER INTERFACE #
##################

ui <- dashboardPage(
  options = list(sidebarExpandOnHover = FALSE),
  dashboardHeader(title = tagList(
    span(class = "logo-lg", "House Price"), 
    img(
      src = "icon.png",
      style = "width: 35px"
    )
  ),titleWidth = 350),#"House Price",titleWidth = 350),
  dashboardSidebar(
    collapsed = TRUE,
    sidebarMenu(
      id = "tabs",
      menuItem("Trends",icon = icon("globe"), tabName = "trends"),
      menuItem("Area-wise Analysis",icon =  icon("flag"), tabName = "area_analysis"),
      menuItem("Comparative Analysis",icon =  icon("arrows-h"), tabName = "Comparative"),
      menuItem("House Price Prediction",icon =  icon("home"), tabName = "prediction")
      
    )
  ),
  dashboardBody(
    # CSS styling for the dashboard
    tags$head(
      tags$style(
        HTML(
          '
          .skin-blue .main-header .logo {
            background-color : #222831;
          }
   
          .skin-blue .main-header .navbar {
          background-color : #222831;
          }
          
          /*Tittle Left Side hover affect*/
                 .skin-blue .main-header .logo:hover {
    background-color: #9D8654;
}
          
          
       
          /*Toggle sidebar button hover affect*/
          .skin-blue .main-header .navbar .sidebar-toggle:hover {
    background-color: #9D8654;
}
       
        /*Toggle sidebar button color */  
.main-header .sidebar-toggle:before {

    color: #F5D68A;
}
 /*sidebar left border */ 
.skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li:hover>a {
              border-left-color: #F5D68A;
              
}
          
          /*Background Color Change*/
          .content {
            background-color:#383D46;
          }
          
          /*Box Background Color*/
          .box{
            background-color:#222831;
            color : white;
                border-top: 3px solid #F5D68A;
          }
          
          /*SliderInput color*/ 
          .irs--shiny .irs-bar {
            background:#F5D68A;
          }
          /*Change map background color*/ 
          #map {
            background-color: #222831;
          }
          /*sliderInput tooltip*/
          .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
          color: black;
              background-color: #F5D68A;
          }
          
          /* change leaflet/map legend  background color*/
          
          .leaflet .info {
            background : #222831;
            border: solid double white;
            border: 2px solid white;
            color : white;
          }
          
          .leaflet .legend svg text {
              fill : white;
          
          
          }
          
          body{
            background-color:#383D46;
          }
          .content-wrapper {
          background-color:#383D46;
          }
          /*Adjust Table Width*/
          .table {
          width:100% !important; 
      
          }
/*.irs-grid-text { font-size: 10pt; }*/
          '
        )
      )
    ),
    tabItems(
      # Tab 1: Overview
      tabItem(
        tabName = "trends",
        
        
        fluidPage( 
          fluidRow(
            
            
            column(4,
                   # Box with year slider and summary statistics
                   box(width = 12,
                       
                       
                       
                       
                       h3("Summary Statistics"),
                       withSpinner(
                         tagList(
                           # uiOutput("accessToElectricityUI"),
                           # uiOutput("renewableEnergyShareUI"),
                           # uiOutput("financialFlowsUI")
                           uiOutput("homeForRentUI"),
                           uiOutput("homeForSaleUI"),
                           uiOutput("mostProfitable"),
                           uiOutput("lessProfitable")
                           
                         ),type = 4)
                       
                   )
                   
            ),
            column(8,
                   
                   # Box with leaflet map
                   withSpinner(leafletOutput("map",height = 535),type = 4)
                   
            )
            
          ),
          br(),
          fluidRow(
            column(12,
                   
                   #  line chart
                   plotlyOutput("trends_chart")
                   # withSpinner(,type = 4)
            )
            
            
            
            
            
          )
        )
        
        
      ),
      
      # Tab 2: Area-wise Analysis
      tabItem(
        tabName = "area_analysis",
        fluidPage(
          fluidRow(
            
            column(4,
                   # Box with country selection and summary statistics
                   box(width = 12,
                       radioGroupButtons(
                         inputId = "house_type_area_wise",
                         label = "",
                         choices = c("Houses for Sale/Buy", 
                                     "Houses for Rent"),
                         justified = TRUE
                       ),
                       # cites
                       # names(location_options)
                       # regions
                       # names(location_options[["Cartagena"]])
                       # locations
                       # location_options[["Cartagena"]][["Cartagena"]]
                       
                       # city
                       selectInput("city_select", "Select City",
                                   choices = names(location_options),
                                   selected = names(location_options)[1]),
                       # region (area_select)
                       selectInput("region_select", "Select Region",
                                   choices = NULL),
                       
                       
                       # location
                       selectInput("location_select", "Select Location",
                                   choices = NULL),
                     
                       
                       h3("Summary Statistics"),
                       withSpinner(uiOutput("area_summary_stats"),4)
                       
                      
                   )
            ),
            column(8,
                   #  Pie chart
                   box(width = 12,
                     selectInput("area_features", "Select Feature",
                                 choices = c("House conditions",
                                             "Number of bathrooms",
                                             "Number of bedrooms",
                                             "Type of housing"
                                             
                                 ),
                                 selected = "House conditions"
                     ),
                     withSpinner(plotlyOutput("condition_distribution",height = 445),4)
                   ),
                   
                   
            )
            
            
            
          ),
          br(),
          
          fluidRow(
            column(12,
                   # Time series plot
                   box(width = 12,
                       sliderInput("n_ticks_histogram", "Number of intervals:", 
                                   min = 2, max = 20, value = 10, step = 1),
                       withSpinner(plotlyOutput("price_distribution_histogram"),4)
                       )
                   
            )
          )
        )
        
      ),
      # Tab 3: Comparative Analysis
      tabItem(
        tabName = "Comparative",
        fluidPage(
          fluidRow(  # First row containing three columns
            column(12,
                   box(width = 12,
                         radioGroupButtons(
                           inputId = "house_type_comparative_analysis",
                           label = "",
                           choices = c("Houses for Sale/Buy", 
                                       "Houses for Rent"),
                           justified = TRUE
                         )
                       )
                   ),
            column(4,
                   box(width = 12,
                       # cites
                       # names(location_options)
                       # regions
                       # names(location_options[["Cartagena"]])
                       # locations
                       # location_options[["Cartagena"]][["Cartagena"]]
                         selectInput("City2",
                                     label = ('Select City'),
                                     choices = names(location_options),
                                     selected = names(location_options)[1]
                         ),
                         selectInput("Region2",
                                     label = ('Select Region'),
                                     choices = NULL
                                     ),
                         selectInput("Location2",
                                     label = ('Select Location'),
                                     choices = NULL
                         )
                       )
            ),
            column(4,
                   box(width = 12,
                         selectInput("City3",
                                     label = ('Select City'),
                                     choices = names(location_options),
                                     selected = names(location_options)[1]
                         ),
                         selectInput("Region3",
                                     label = ('Select Region'),
                                     choices = NULL
                         ),
                         selectInput("Location3",
                                     label = ('Select Location'),
                                     choices = NULL
                         )
                       )
            ),
            column(4,
                   box(width = 12,
                       selectInput("Compare_variable",
                                   label = ('Compare'),
                                   choices = c(
                                     "Price",
                                     "Size",
                                    "Number of rooms",
                                    "profitability (%)"
                                    
                                   ),
                                   selected = "Price"
                            ),
                       selectInput("Compare_variable1",
                                   label = (''),
                                   choices = c(
                                     "Price",
                                     "Size",
                                     "Number of rooms",
                                     "profitability (%)"
                                   ),
                                   selected = "Size"
                                )
                       )
            )
          ),  # End of the first row
          fluidRow(  # Second row containing the plot
            column(12,
                   
                   box(width = 12,
                       sliderInput("n_ticks", "Number of intervals:", 
                                   min = 2, max = 20, value = 10, step = 1),
                       withSpinner(plotlyOutput("mypointline"),6)
                       ),
                   
                   
            )
          )  # End of the second row
        )
      ),
      tabItem(
        tabName = "prediction",
        fluidPage(
      
          fluidRow(
            
         
              box( 
                   
                   h3("Características de la Vivienda", class = "section-title"),
                   fluidRow(
                     column(6,
                            selectInput("city_prediction", "City", 
                                        choices = names(location_options),
                                        selected = names(location_options)[1]
                                          ),
                            selectInput("region_prediction", "region", 
                                        choices = NULL),
                            
                            selectInput("localizacion", "Localización", choices = NULL),
                            selectInput("ascensor", "Ascensor", choices = yes_no_options),
                            sliderInput("baños", "Baños", min = 0, max = 7, value = 1),
                            selectInput("trastero", "Trastero", choices = yes_no_options),
                            selectInput("piso", "Piso", choices = floor_options),
                            # selectInput("jardin", "Jardín", choices = yes_no_options),
                            selectInput("balcon", "Balcón", choices = yes_no_options)
                     ),
                     column(6,
                            sliderInput("habitaciones", "Habitaciones", min = 0, max = 7, value = 1),
                            numericInput("metros_reales", "Metros construidos", value = 100),
                            selectInput("condicion", "Condición", choices = condition_options),
                            selectInput("armarios", "Armarios empotrados", choices = yes_no_options),
                            selectInput("terraza", "Terraza", choices = yes_no_options),
                            selectInput("garaje", "Garaje", choices = yes_no_options),
                            selectInput("aire_acondicionado", "Aire acondicionado", choices = yes_no_options),
                            selectInput("calefaccion", "Calefacción", choices = yes_no_options)
                     )
                   ),
                   actionButton("btn_calculate", "Calcular Precio", class = "btn-primary btn-lg btn-block mt-4")
              ),
          
            
               box(
                 
                 h3("Predicción de Precio de la Vivienda", class = "section-title"),
                 
                 uiOutput("House_Prediction_Result")
                 
                   


                 
               )

          )
          
        )
        
        )
      
    )
    
    
    
  )
)

################
# SHINY SERVER #
################

server <- function(input, output,session) {
  
  

  
  ##########################################################################
  ############################ 1st Tab (Trends) ################
  ##########################################################################
  

  observeEvent(input$map_shape_click, { 
    location <- input$map_shape_click$id  # typo was on this line
    # print(location)
    filtered = df_casas_alquiler %>%
      filter(region == location)
    
    
    output$trends_chart<- renderLeaflet({

      # filtered$metros_normalized <- 1 + (filtered$metros_reales - min(filtered$metros_reales)) /
      #   (max(filtered$metros_reales) - min(filtered$metros_reales)) * 100
     
      plot <- plot_ly(
        data = filtered, 
        x = ~titulo, 
        y = ~precio, 
        type = 'scatter', 
        mode = 'markers',
        marker = list(
          size = ~metros_reales, 
          color = ~precio, 
          colorscale = 'Viridis'
        ),
        text = ~paste(
                      "<b>title:</b> ", titulo,
                      "<br><b>Price:</b> ", precio,
                      "<br><b>Meter Gauge:</b> ", metros_reales, "sqm",
                      "<br><b>condition:</b> ", condicion,
                      "<br><b>elevator:</b> ", ifelse(ascensor == 0 , "No","Yes"),
                      "<br><b>bathrooms:</b> ", baños,
                      "<br><b>storage room:</b> ",  ifelse(trastero == 0 , "No","Yes"),
                      "<br><b>floor:</b> ", piso,
                      "<br><b>bedrooms:</b> ", habitaciones,
                      "<br><b>terrace:</b> ",  ifelse(terraza == 0 , "No","Yes"),
                      "<br><b>air_conditioning:</b> ",  ifelse(aire_acondicionado == 0 , "No","Yes")
                      
                      ),
        hoverinfo = 'text'
      )%>% 
      layout(
            height = 600,
            title = "Avaliable Houses",
             xaxis = list(title = "Title"),
             yaxis = list(title = "price"),
             paper_bgcolor = "#222831",  # Background color
             plot_bgcolor ="#222831",
             font = list(color = "white"))
      
      plot
  

    })
    
  })
  # Map
  
  output$map <- renderLeaflet({
   
    cities_sf <- st_transform(cities_sf, crs = 4326)
    df_casas_alquiler_temp = df_casas_alquiler
    colnames(df_casas_alquiler_temp)[colnames(df_casas_alquiler_temp) == "region"] <- "NOMBRE"
    
    df_avg_profitability <- df_casas_alquiler_temp %>%
      group_by(NOMBRE) %>%
      summarise(avg_gross_profitability = mean(gross_profitability, na.rm = TRUE))
    
    
    merged_data <- cities_sf %>%
      left_join(df_avg_profitability, by = "NOMBRE")
   
    pal <- colorNumeric(
      palette = "YlOrRd",  # Yellow to Red color scheme
      domain = merged_data$avg_gross_profitability
    )
    leaflet(data = merged_data) %>%
      #addTiles() %>%  # Add default OpenStreetMap tiles
      addPolygons(
        layerId = ~NOMBRE,
        fillColor = ~pal(avg_gross_profitability),  # Set fill color based on gross_profitability
        weight = 1,  # Line weight for borders
        opacity = 1,  # Line opacity
        color = "black",,  # Border color
        fillOpacity = 0.8,  # Fill opacity
        label = ~paste("<b>Region:</b>", NOMBRE, "<br>",
                       "<b>Average Profitability:</b>",round(  avg_gross_profitability,2 ) , "%" )%>%
          lapply(htmltools::HTML),
        labelOptions = labelOptions(
          direction = "auto"
        ),
        highlightOptions = highlightOptions(
          color = "red",  # Border color when hovering
          weight = 3,     # Border weight when hovering
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = pal, 
        values =   ~merged_data$avg_gross_profitability[!is.na(merged_data$avg_gross_profitability)] ,
        title = "Gross Profitability",
        position = "bottomright",
        labFormat = labelFormat(suffix = "%")
        
      ) %>%
      # Add title to the map
      addControl(
        html = "<h3 style='text-align: center;'>Gross Profitability by Regions</h3>", 
        position = "topright"  # You can change the position to "topleft", "bottomleft", etc.
      )
    # df_unique <- df_casas_alquiler %>%
    #   distinct(lat, long, .keep_all = TRUE)
    # 
    # #palette <- colorNumeric(palette = "YlOrRd", domain = df_casas_alquiler$gross_profitability)
    # 
    # 
    # getColor <- function(profitability) {
    #   sapply(profitability, function(gp) {
    #     if (gp <= 4) {
    #       "green"        # Low profitability
    #     } else if (gp <= 8) {
    #       "orange"       # Medium profitability
    #     } else {
    #       "red"          # High profitability
    #     }
    #   })
    # }
    # 
    # # Create the icons using the getColor function
    # icons <- awesomeIcons(
    #   icon = 'info-sign',
    #   iconColor = 'white',
    #   library = 'glyphicon',
    #   markerColor = getColor(df_unique$gross_profitability)
    # )
    # 
    # cities_sf_wgs84 <- st_transform(cities_sf, crs = 4326)
    # leaflet(df_unique) %>%
    #   addTiles() %>% 
    #     
    #   addAwesomeMarkers(
    #     layerId = ~localizacion,
    #     lng = ~long,
    #     lat = ~lat,
    #     icon = icons,
    #     label =  ~paste(
    #       "Location: ","<b>", localizacion, "</b><br>",
    #       
    #       "Profitability: ","<b>", round(gross_profitability, 2), "</b>"
    #       
    #     )%>%
    #       lapply(htmltools::HTML)
    #   ) %>%
    #   addLegend(
    #     "bottomright", 
    #     colors = c("green", "orange", "red"),
    #     labels = c("Low (0-4%)", "Medium (4-8%)", "High (8%+)"),
    #     title = "Gross Profitability",
    #     opacity = 1
    #   )
    
 
    
  })
  


  # Summary Statistics
  
  # Render the UI for "Access to Electricity" summary
  output$homeForSaleUI <- renderUI({
    
    
     df_casas_alquiler_filtered <- df_casas_alquiler 
     #%>%
    #   filter(localizacion %in% locations)
    
    
    val = nrow(df_casas_alquiler_filtered)
    div(style = "text-align: center; border: 1px solid #ccc; padding: 20px; margin: 10px;",
        h1(paste0(round(val, 2)), style = "margin: 0;"),
        p("Total homes for Rent", style = "margin: 0;")
      )
  })
  # Render the UI for "Renewable Energy Share" summary
  output$homeForRentUI <- renderUI({
    
    df_casas_compra_filtered <- df_casas_compra 
    # %>%
    #                     filter(localizacion %in% locations)
    
    
    val = nrow(df_casas_compra_filtered)
    div(style = "text-align: center; border: 1px solid #ccc; padding: 20px; margin: 10px;",
        h1(paste0(round(val, 2)), style = "margin: 0;"),
        p("Total Homes for Sale ", style = "margin: 0;")
      )
  })
  output$mostProfitable <- renderUI({
    
    avg_profitability <- df_casas_alquiler %>%
      group_by(localizacion) %>%
      summarise(average_gross_profitability = mean(gross_profitability, na.rm = TRUE))
    
    max_localizacion <- avg_profitability[which.max(avg_profitability$average_gross_profitability), ]
    
    
    
    div(style = "text-align: center; border: 1px solid #ccc; padding: 20px; margin: 10px;",
        h1(max_localizacion$localizacion , style = "margin: 0;"),
        p("Most Profitable location for Sale ", style = "margin: 0;")
    )
  })
  output$lessProfitable <- renderUI({
    avg_profitability <- df_casas_alquiler %>%
      group_by(localizacion) %>%
      summarise(average_gross_profitability = mean(gross_profitability, na.rm = TRUE))
    
    min_localizacion <- avg_profitability[which.min(avg_profitability$average_gross_profitability), ]
    
    div(style = "text-align: center; border: 1px solid #ccc; padding: 20px; margin: 10px;",
        h1(min_localizacion$localizacion , style = "margin: 0;"),
        p("Less Profitable location for Sale ", style = "margin: 0;")
    )
  })
 
  
  ##########################################################################
  ############################ 2nd Tab (Area wise Analysis) ################
  ##########################################################################
  # cites
  # names(location_options)
  # regions
  # names(location_options[["Cartagena"]])
  # locations
  # location_options[["Cartagena"]][["Cartagena"]]
  
  observeEvent(input$city_select, {
    updateSelectInput(session, "region_select", choices = names( location_options[[input$city_select]]) )
  })
  
  observeEvent(input$region_select, {
    req(input$city_select)
    updateSelectInput(session, "location_select", choices = location_options[[input$city_select]][[input$region_select]] )
  })
  # Summary Stats for Selected Aread
  
  format_number <- function(x) {
    if (is.na(x)) return("N/A")  # Handle NA values
    if (abs(x) < 1e3) {
      return(as.character(round( x)))  # No suffix for values less than 1000
    } else if (abs(x) < 1e6) {
      return(paste0(round(x / 1e3, 1), "k"))  # Format for thousands
    } else if (abs(x) < 1e9) {
      return(paste0(round(x / 1e6, 1), "M"))  # Format for millions
    } else {
      return(paste0(round(x / 1e9, 1), "B"))  # Format for billions
    }
  }
  
  output$area_summary_stats <- renderUI({
    # Ensure a location is selected
    req(input$location_select)
    req(input$house_type_area_wise)
    
    selected_location <- input$location_select
    
    selected_location <- tolower(iconv(selected_location, from = "UTF-8", to = "ASCII//TRANSLIT"))
    
    # Filter data for the selected location
    if (input$house_type_area_wise == "Houses for Sale/Buy") {
      location_data <- filter(df_casas_compra, localizacion == selected_location)
    } else {
      location_data <- filter(df_casas_alquiler, localizacion == selected_location)
    }
    
    # Calculate summary statistics
    summary_stats <- location_data %>%
      summarise(
        Number_of_Homes = n(),
        Mean_Price = mean(precio, na.rm = TRUE),  
        Median_Price = median(precio, na.rm = TRUE),
        Min_Price = min(precio, na.rm = TRUE),  
        Max_Price = max(precio, na.rm = TRUE),
        Price_Range = (max(precio, na.rm = TRUE) - min(precio, na.rm = TRUE)), 
        Price_per_Sq_Meter = mean(precio / metros_reales, na.rm = TRUE)
      )
    
    # Add additional statistics for "Houses for Rent"
    if (input$house_type_area_wise == "Houses for Rent") {
      Mean_buy_sell_price <- mean(location_data$buy_sell_price, na.rm = TRUE)
      Average_profitability <- ((summary_stats$Mean_Price * 12) / Mean_buy_sell_price) * 100
      
      summary_stats <- summary_stats %>%
        mutate(
          Mean_buy_sell_price = Mean_buy_sell_price,
          Average_profitability = Average_profitability
        )
    } else {
      Mean_rent_price <- mean(location_data$rent_price, na.rm = TRUE)
      Average_profitability <- (( Mean_rent_price * 12) / summary_stats$Mean_Price )  * 100
      
      summary_stats <- summary_stats %>%
        mutate(
          rent_price = Mean_rent_price,
          Average_profitability = Average_profitability
        )
    }
    
    # Create a table with formatted summary statistics
    result_table <- data.frame(
      Metrics = c("Number of Homes", "Average Price", "Median Price", "Minimum Price", "Maximum Price", "Price Range", "Price per Sq. Meter"),
      Value = c(
        summary_stats$Number_of_Homes,
        format_number(summary_stats$Mean_Price),  
        format_number(summary_stats$Median_Price),
        format_number(summary_stats$Min_Price),
        format_number(summary_stats$Max_Price),
        format_number(summary_stats$Price_Range),
        format_number(summary_stats$Price_per_Sq_Meter)
      )
    )
    
    # Add average profitability to the result table if relevant
    #if (input$house_type_area_wise == "Houses for Rent") {
      result_table <- rbind(result_table, 
                            data.frame(
                              Metrics = "Average Profitability (%)",
                              Value =   paste0( round (summary_stats$Average_profitability, 2), "%")
                            )
      )
    #} 
    
    renderTable(result_table, rownames = FALSE)
  })
  
  output$condition_distribution <- renderPlotly({
    req(input$location_select)
    req(input$area_features)
    
 
    
    selected_location <- input$location_select
    selected_location <- tolower(iconv(selected_location, from = "UTF-8", to = "ASCII//TRANSLIT"))
    
    if (input$house_type_area_wise == "Houses for Sale/Buy") {
      location_data <- filter(df_casas_compra, localizacion == selected_location)
    } else {
      location_data <- filter(df_casas_alquiler, localizacion == selected_location)
    }
    
    
    
    if (input$area_features == "House conditions" ) {
        location_data$labels = location_data$condicion
        legend_text = "House Conditions"
        
    } else if (input$area_features == "Number of bathrooms") {
      location_data$labels = location_data$baños
      legend_text = "Number of bathrooms"
    } else if (input$area_features == "Number of bedrooms") {
      location_data$labels = location_data$habitaciones
      legend_text = "Number of bedrooms"
    } else if (input$area_features == "Type of housing") {
      location_data$labels = location_data$piso
      legend_text = "Type of housing"
    }
    
    condition_counts <- location_data %>%
      group_by(labels) %>%
      summarise(Count = n()) %>%
      
      ungroup()
    
    # Calculate percentages
    condition_counts <- condition_counts %>%
      mutate(Percentage = Count / sum(Count) * 100)
 
    
    plot_ly(condition_counts, labels = ~labels, values = ~Percentage, type = "pie",
            textinfo = "label+percent") %>%
      layout(title =list(text = paste("<b>Distribution of ",legend_text,"\n", "in ", selected_location), x = 0.04, y = 0.85),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             paper_bgcolor = "#222831",  # Set the background color
             font = list(color = "white"),  # Set text color)
             legend = list(
               
               title = list(text = legend_text),  # Add a title to the legend
               bgcolor = "#222831",  # Set the legend's background color
               orientation = "v" ,
               x = 0.8, 
               y = 0.5,
               
               borderwidth = 1,
               bordercolor = "#FFFFFF"
             )
             ###
             
      ) %>%
      add_trace(type = "pie", text  = paste0( condition_counts$labels,"<br>", round( condition_counts$Percentage ,2) , "%"),hoverinfo = 'text',marker = list(
        colors = c("#7F7F7F", "lightpink", "#00AF50")  # Set the colors for each portion
      ))
  })
  
  
  output$price_distribution_histogram <- renderPlotly({
    # Ensure a location is selected
    req(input$location_select)
    selected_location <- input$location_select
    selected_location <- tolower(iconv(selected_location, from = "UTF-8", to = "ASCII//TRANSLIT"))
    
    # Filter data for the selected location
    
    if (input$house_type_area_wise == "Houses for Sale/Buy") {
      location_data <- filter(df_casas_compra, localizacion == selected_location) 
    } else {
      location_data <- filter(df_casas_alquiler, localizacion == selected_location) 
    }
    
    x_min <- min(location_data$precio)
    x_max <- max(location_data$precio)
    x_range <- x_max - x_min
    
    n_ticks <- input$n_ticks_histogram
    tick_interval <- x_range / (n_ticks - 1)
    
    
    # Create a histogram for the price distribution
    plot_ly(location_data, 
            x = ~precio, 
            type = 'histogram', 
            marker = list(color = '#DDC5A6', line = list(width = 1, color = 'black')),
            hoverinfo = 'text',
            text = ~paste("Title: ", titulo,
                          "<br>Price: ", precio,
                          "<br>condition: ", condicion,
                          "<br>Square Meterage: ", metros_reales,
                          "<br>floor: ", piso
                          ),
            textposition = "none"
       
            ) %>%
      layout(title = paste("Price Distribution for Homes in", selected_location),
              
             xaxis = list(title = "Price",
                          tickmode = "linear",
                          tick0 = x_min,
                          dtick = tick_interval,
                          tickformat = "~s"),
             yaxis = list(title = "Frequency"),
             paper_bgcolor = "#222831",
             plot_bgcolor ="#222831",
             font = list(color = "white")
            
             )%>% rangeslider(start=x_min, end=x_max)
   
  })
  
  ####################################################################
  ##################### 3rd Tab Comparative Analysis #################
  ####################################################################
  # cites
  # names(location_options)
  # regions
  # names(location_options[["Cartagena"]])
  # locations
  # location_options[["Cartagena"]][["Cartagena"]]
  
  observeEvent(input$City2, {
    updateSelectInput(session, "Region2", 
                      choices = names(location_options[[input$City2]]),
                      selected = names(location_options[[input$City2]])[1] )
  })
  
  observeEvent(input$Region2, {
    req(input$City2)
    updateSelectInput(session, "Location2", 
                      choices = location_options[[input$City2]][[input$Region2]],
                      selected = location_options[[input$City2]][[input$Region2]][1] )
  })
  
  
  
  observeEvent(input$City3, {
    updateSelectInput(session, "Region3", 
                      choices = names(location_options[[input$City3]]),
                      selected = names(location_options[[input$City3]])[1] )
  })
  
  observeEvent(input$Region3, {
    req(input$City3)
    updateSelectInput(session, "Location3", 
                      choices = location_options[[input$City3]][[input$Region3]],
                      selected = location_options[[input$City3]][[input$Region3]][2] )
  })
  
  
# 
#   observe({
#     if (input$house_type_comparative_analysis == "Houses for Sale/Buy") {
#       
#       
#           updateSelectInput(session, 
#                             inputId = "Compare_variable", 
#                            
#                             choices = c(
#                               "Price",
#                               "Size",
#                               "Number of rooms"
#                             )
#           )
#           updateSelectInput(session, 
#                           inputId = "Compare_variable1", 
#                          
#                           choices = c(
#                             "Price",
#                             "Size",
#                             "Number of rooms"
#                           ),
#                           selected = "Size"
#         )
#       
#       
#     } else {
#       updateSelectInput(session, 
#                         inputId = "Compare_variable", 
#                         
#                         choices = c(
#                           "Price",
#                           "Size",
#                           "Number of rooms",
#                           "profitability (%)"
#                         )
#       )
#       updateSelectInput(session, 
#                         inputId = "Compare_variable1", 
#                         
#                         choices = c(
#                           "Price",
#                           "Size",
#                           "Number of rooms",
#                           "profitability (%)"
#                         ),
#                         selected = "Size"
#       )
#     }
#   })
  
  
  
  output$mypointline <- renderPlotly({
    req(input$Location2)
    req(input$Location3)
    
    # Filter data for the selected locations
    
    input_Location2 <- tolower(iconv(input$Location2, from = "UTF-8", to = "ASCII//TRANSLIT"))
    input_Location3 <- tolower(iconv(input$Location3, from = "UTF-8", to = "ASCII//TRANSLIT"))
    
    
    if (input$house_type_comparative_analysis == "Houses for Sale/Buy") {
      comparison_data <- df_casas_compra %>%
        filter(localizacion %in% c(input_Location2, input_Location3))
    } else {
      comparison_data <- df_casas_alquiler %>%
        filter(localizacion %in% c(input_Location2, input_Location3))
    }
    
    
    
    
    
    if (input$Compare_variable == "Price") {
      comparison_data$x = comparison_data$precio
      x_axis_label = "Price"
      
    } else if (input$Compare_variable == "Size") {
      comparison_data$x = comparison_data$metros_reales
      x_axis_label = "Size (Square Meters)"
    } else if (input$Compare_variable == "Number of rooms") {
      comparison_data$x = comparison_data$habitaciones
      x_axis_label = "Number of rooms"
    } else if (input$Compare_variable == "profitability (%)") {
      comparison_data$x = comparison_data$gross_profitability
      x_axis_label = "profitability (%)"
    }
    
    if (input$Compare_variable1 == "Price") {
      comparison_data$y = comparison_data$precio
      y_axis_label = "Price"
    
    } else if (input$Compare_variable1 == "Size")  {
      
      comparison_data$y = comparison_data$metros_reales
      y_axis_label = "Size (Square Meters)"
    } else if (input$Compare_variable1 == "Number of rooms")  {
      
      comparison_data$y = comparison_data$habitaciones
      y_axis_label = "Number of rooms"
    } else if (input$Compare_variable1 == "profitability (%)") {
      comparison_data$y = comparison_data$gross_profitability
      y_axis_label = "profitability (%)"
    }
    x_min <- min(comparison_data$x)
    x_max <- max(comparison_data$x)
    x_range <- x_max - x_min
    
    # Calculate the interval based on the number of ticks
    n_ticks <- input$n_ticks
    tick_interval <- x_range / (n_ticks - 1)
    
    # x_min <- min(comparison_data$x)
    # x_max <- max(comparison_data$x)
    # x_range <- x_max - x_min
    # 
    # # Set a default interval to divide the range into 10 ticks
    # default_interval <- x_range / 10
    # 
    # # Update the slider input with the calculated interval
    # updateSliderInput(session, "x_tick_interval_comperative", 
    #                   min = x_min, 
    #                   max = x_max, 
    #                   value = default_interval,
    #                   step = default_interval / 5)  # Adjust the step for finer control
    
    # plot_ly() %>%
    #   add_trace(data = comparison_data %>% filter(localizacion == input$Location2),
    #             x = ~x,
    #             y = ~y,
    #             type = 'scatter',
    #             mode = 'lines',
    #             name = input$Location2,
    #             hoverinfo = 'text',
    #             text = ~paste(
    #               "Area: ", localizacion, "<br>",
    #               "Title: ", titulo, "<br>",
    #               "Price: ", round(precio / 1000, 2), "k<br>",
    #               "Square Meterage: ", metros_reales
    #             ),
    #             line = list(color = 'blue')) %>%
    #   add_trace(data = comparison_data %>% filter(localizacion == input$Location3),
    #             x = ~x,
    #             y = ~y,
    #             type = 'scatter',
    #             mode = 'lines',
    #             name = input$Location3,
    #             hoverinfo = 'text',
    #             text = ~paste(
    #               "Area: ", localizacion, "<br>",
    #               "Title: ", titulo, "<br>",
    #               "Price: ", round(precio / 1000, 2), "k<br>",
    #               "Square Meterage: ", metros_reales
    #             ),
    #             line = list(color = 'red')) %>%
    #   
    #   layout(title = paste0(x_axis_label, " vs. ", y_axis_label, " in Selected Locations"),
    #          xaxis = list(title = x_axis_label),
    #          yaxis = list(title = y_axis_label),
    #          paper_bgcolor = "#222831",
    #          plot_bgcolor = "#222831",
    #          font = list(color = "white"),
    #          rangeslider = list(start = min(comparison_data$x), end = max(comparison_data$x))
    #   )
    label_format <- if (max(comparison_data$x) >= 1e6) {
      label_number(scale = 1e-6, suffix = "m")
    } else if (max(comparison_data$x) >= 1e3) {
      label_number(scale = 1e-3, suffix = "k")
    } else {
      label_number(scale = 1)
    }
  
    gg <- ggplot(comparison_data, aes(x = x, y = y, group = localizacion, color = localizacion,
                                      text = paste(
                                        "Area: ", localizacion, "<br>",
                                        "Title: ", titulo, "<br>",
                                        "Price: ", round(precio / 1000, 2), "k<br>",
                                        "Square Meterage: ", metros_reales
                                      )
    )) +
      geom_line() +
      geom_point() +  # Adds points to the line
      labs(title =  paste0(x_axis_label," vs. ",y_axis_label," in Selected Locations"),
           x = x_axis_label,
           y = y_axis_label) +
      scale_x_continuous(transform = scales::transform_reciprocal()) + 
      scale_x_continuous(breaks = seq(x_min, x_max, by = tick_interval),
                         labels = label_format) + 
      # scale_x_continuous(limits = c(min(comparison_data$x), max(comparison_data$x)),
      #                     # breaks = seq(
      #                     #     min(comparison_data$x), 
      #                     #     max(comparison_data$x), 
      #                     #     by = input$x_tick_interval_comperative
      #                     # ),
      #                    labels = label_format) +  # Apply the custom label format
      theme_dark() +
      theme(axis.text = element_text(size = 12, color = "white"),
            axis.title = element_text(size = 14, color = scales::alpha("white", alpha = 0.3)),
            plot.title = element_text(size = 16, hjust = 0.5, face = "bold", color = "white"),
            plot.subtitle = element_text(size = 12, hjust = 0.5, color = "white"),
            plot.caption = element_text(color = "white"),
            legend.position = "right",
            plot.background = element_rect(fill = "#222831", color = "white"),
            panel.background = element_rect(fill = "#424B55", size = 0.5, linetype = "solid"),
            legend.background = element_rect(fill = "transparent", colour = "white"),
            legend.key = element_rect(fill = "transparent", colour = "transparent"),
            legend.text = element_text(colour = "white"),
            legend.title = element_text(colour = "white")
      )

    ggplotly(gg, tooltip = "text", height = 530) %>% 
      rangeslider(start = min(comparison_data$x), end = max(comparison_data$x)) %>% 
      layout( margin = list(
      l = 25,
      r = 25,
      b = 100,
      t = 100
    )

    )

    
  })
  
  ########################################################
  #########################Tab 4 Prediction #############
  ######################################################
  

  observeEvent(input$city_prediction, {
    updateSelectInput(session, "region_prediction", 
                      choices = names(location_options[[input$city_prediction]]),
                      selected = names(location_options[[input$city_prediction]])[1] )
  })
  
  observeEvent(input$region_prediction, {
    req(input$city_prediction)
    updateSelectInput(session, "localizacion", 
                      choices = location_options[[input$city_prediction]][[input$region_prediction]],
                      selected = location_options[[input$city_prediction]][[input$region_prediction]][1] )
  })
  
  
  
  observeEvent(input$btn_calculate,{

    req(input$localizacion)
    req(input$ascensor)
    req(input$baños)
    req(input$trastero)
    req(input$piso)
    req(input$habitaciones)
    req(input$metros_reales)
    req(input$condicion)
    req(input$armarios)
    req(input$terraza)
    req(input$garaje)
    req(input$aire_acondicionado)
    req(input$calefaccion)
    req(input$balcon)
    
   
    # Filter data according to selected location
    localizacion_seleccionada <- input$localizacion
    localizacion_seleccionada <- tolower(iconv(input$localizacion, from = "UTF-8", to = "ASCII//TRANSLIT"))
    # Count records for the selected location
    num_registros_venta <- nrow(df_casas_compra[df_casas_compra$localizacion == localizacion_seleccionada,])
    num_registros_alquiler <- nrow(df_casas_alquiler[df_casas_alquiler$localizacion == localizacion_seleccionada,])
    
    # Calculate total number of homes analyzed
    total_viviendas_compra <- nrow(df_casas_compra)
    total_viviendas_alquiler <- nrow(df_casas_alquiler)

    
    # Predictions from the models (assuming the models are loaded)
    prediction_compra <- as.integer(predict_price(
                localizacion = as.character(input$localizacion),
                ascensor = as.integer(input$ascensor),                     # Convert to integer
                baños = as.integer(input$baños),                           # Convert to integer
                trastero = as.integer(input$trastero),                     # Convert to integer
                piso = as.character(input$piso),                           # Convert to character
                habitaciones = as.integer(input$habitaciones),              # Convert to integer
                metros_reales = as.integer(input$metros_reales),           # Convert to numeric
                condicion = as.character(input$condicion),                 # Convert to character
                armarios_empotrados = as.integer(input$armarios), # Convert to integer
                terraza = as.integer(input$terraza),                        # Convert to integer
                garaje = as.integer(input$garaje),                          # Convert to integer
                aire_acondicionado = as.integer(input$aire_acondicionado),  # Convert to integer
                calefaccion = as.integer(input$calefaccion),                # Convert to integer
                balcon = as.integer(input$balcon),       
                df_casas_compra,
                compra_model
              ))
    
    # print(prediction_compra)
    #prediction_alquiler <- as.integer(predict(model_alquiler, newdata = data))
    # Trastero
    # Garaje
    prediction_alquiler <- as.integer(predict_price(
      localizacion = as.character(input$localizacion),
      ascensor = as.integer(input$ascensor),                     # Convert to integer
      baños = as.integer(input$baños),                           # Convert to integer
      trastero = ifelse( as.integer(input$trastero) == 0 , 1,0),                     # Convert to integer
      piso = as.character(input$piso),                           # Convert to character
      habitaciones = as.integer(input$habitaciones),              # Convert to integer
      metros_reales = as.integer(input$metros_reales),           # Convert to numeric
      condicion = as.character(input$condicion),                 # Convert to character
      armarios_empotrados = as.integer(input$armarios), # Convert to integer
      terraza = as.integer(input$terraza),                        # Convert to integer
      garaje = ifelse( as.integer(input$garaje) == 0 , 1,0) ,                         # Convert to integer
      aire_acondicionado = as.integer(input$aire_acondicionado),  # Convert to integer
      calefaccion = as.integer(input$calefaccion),                # Convert to integer
      balcon = as.integer(input$balcon),       
      df_casas_alquiler,
      alquiler_model
    ))
    
    # Define the margin according to the price
    margen_alquiler <- if (prediction_alquiler < 900) 0.08 else 0.10
    margen_compra <- if (prediction_compra < 250000) 0.08 else 0.10
    
    # Calculate the margin for rent
    alquiler_min <- as.integer(prediction_alquiler * (1 - margen_alquiler))
    alquiler_max <- as.integer(prediction_alquiler * (1 + margen_alquiler))
    
    # Calculate the margin for purchase
    compra_min <- as.integer(prediction_compra * (1 - margen_compra))
    compra_max <- as.integer(prediction_compra * (1 + margen_compra))
    
    # Calculate annual gross profitability
    precio_anual_alquiler <- prediction_alquiler * 12
    rentabilidad_bruta_anual <- round((precio_anual_alquiler / prediction_compra) * 100, 2)
    
    # Calculate years to recover the investment
    anos_recuperacion_inversion <- round(prediction_compra / precio_anual_alquiler, 2)
    
    output$House_Prediction_Result <- renderUI({
      tagList(
        strong(p("Prediction of the Purchase/Sale Price:")),
        div(paste0("€", format(compra_min, big.mark = ","), " - €", format(compra_max, big.mark = ","))),
        
        strong(p("Monthly Rental Price Prediction:")),
        div(paste0("€", format(alquiler_min, big.mark = ","), " - €", format(alquiler_max, big.mark = ","))),
        
        strong(p("Annual Gross Profitability:")),
        div(paste0(rentabilidad_bruta_anual, "%")),
        
        strong(p("Years to recover investment:")),
        div(paste0(anos_recuperacion_inversion, " years")),
        
        strong(paste0("Number of homes analyzed in ", localizacion_seleccionada, ":")),
        div(paste0("For sale: ", num_registros_venta)),
        div(paste0("For rent: ", num_registros_alquiler)),
        
        strong(p("Total number of homes analyzed in Murcia:")),
        div(paste0("For sale: ", total_viviendas_compra)),
        div(paste0("For rent: ", total_viviendas_alquiler))
      )
      
    })
    
  })
}


#############
# Run Shiny #
#############

shinyApp(ui, server)