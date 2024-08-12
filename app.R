###############################################################################
######################### VISOR GEOGRÁFICO DE LA DAP ########################## 
###############################################################################

######## Recursos ######## 
# Libreria Microsoft365R: https://cran.r-project.org/web/packages/Microsoft365R/Microsoft365R.pdf
# Iconos 1: https://fontawesome.com/search?q=place&o=r
# Iconos 2: https://www.freepik.es/search?format=search&last_filter=type&last_value=icon&query=reservorio&type=icon
# Libreria shinydashboard: https://rstudio.github.io/shinydashboard/appearance.html
# Recursos de apoyo: https://www.reddit.com/r/rstats/comments/vhjt3g/how_to_hide_legends_for_layers_that_are_initially/
#                   https://bookdown.org/loankimrobinson/rshinybook/stock-back-individual.html#plotly-package
#                   https://plotly.com/r/bar-charts/
# RDS DATA: https://blog.enterprisedna.co/how-to-save-load-an-rds-file-in-r/#:~:text=RDS%20(R%20Data%20Serialization)%20files,your%20work%20for%20later%20use.
# DataTable: https://shiny.posit.co/r/gallery/widgets/datatables-demo/
#           https://rstudio.github.io/DT/shiny.html
#           https://laustep.github.io/stlahblog/posts/DTcallbacks.html
#           https://shiny.posit.co/r/reference/shiny/0.12.0/renderdatatable
##########################

######## Importando librerias ######## 
library(leaflet.extras) # Para el boton de busqueda en el mapa
#library(shinyjs)
library(shinydashboard)
library(shiny)
library(sf)
# library(dplyr)
library(leaflet)
# library(geojsonsf)
# library(lattice)
# library(leafem)
library(plotly)
library(DT)
library(openxlsx)

######## Cargando capas ########


url_depa <- "https://raw.githubusercontent.com/HermanMoreno98/DATA_DASH/main/depa.json"
url_prov <- "https://raw.githubusercontent.com/HermanMoreno98/DATA_DASH/main/prov.json"
url_dist <- "https://raw.githubusercontent.com/HermanMoreno98/DATA_DASH/main/dist.json"
url_res <- "https://raw.githubusercontent.com/HermanMoreno98/DATA_DASH/main/res.json"
url_capta <- "https://raw.githubusercontent.com/HermanMoreno98/DATA_DASH/main/capta.json"
url_ptap <- "https://raw.githubusercontent.com/HermanMoreno98/DATA_DASH/main/ptap.json"
url_ptar <- "https://raw.githubusercontent.com/HermanMoreno98/DATA_DASH/main/ptar.json"
url_eps <- "https://raw.githubusercontent.com/HermanMoreno98/DATA_DASH/main/eps.json"
#url_capa_pc_ccpp <- "https://raw.githubusercontent.com/HermanMoreno98/DATA_DASH/main/capa_pc_ccpp.json"
url_capa_pc_ccpp <- "https://raw.githubusercontent.com/HermanMoreno98/DATA_DASH/main/data_pc.json"
url_capa_pc_ccpp_general <- "https://raw.githubusercontent.com/HermanMoreno98/DATA_DASH/main/data_ccpp_general.json"
# url_capa_pc_ccpp_general <- "https://raw.githubusercontent.com/HermanMoreno98/DATA_DASH/main/capa_pc_ccpp_general.json"
url_prestador <- "https://raw.githubusercontent.com/HermanMoreno98/DATA_DASH/main/pequena_ciudad_prest.xlsx"
url_expo_inund_grd <- "https://raw.githubusercontent.com/HermanMoreno98/DATA_DASH/main/expo_inund_grd.json"
url_sectores_op <- "https://raw.githubusercontent.com/HermanMoreno98/DATA_DASH/main/sectores_op.json"
url_colegio_p1 <- "https://raw.githubusercontent.com/HermanMoreno98/DATA_DASH/main/colegio_parte1.json"
url_colegio_p2 <- "https://raw.githubusercontent.com/HermanMoreno98/DATA_DASH/main/colegio_parte2.json"

depa <- st_read(url_depa)
prov <- st_read(url_prov)
dist <- st_read(url_dist)
res <- st_read(url_res)
capta <- st_read(url_capta)
ptap <- st_read(url_ptap)
ptar <- st_read(url_ptar)
eps <- st_read(url_eps)
capa_pc_ccpp <- st_read(url_capa_pc_ccpp)
capa_pc_ccpp_general <- st_read(url_capa_pc_ccpp_general)
capa_pc_ccpp_general_rural <- capa_pc_ccpp_general[capa_pc_ccpp_general$ambito == "Rural", ]
capa_pc_ccpp_general_pc <- capa_pc_ccpp_general[capa_pc_ccpp_general$ambito == "PC", ]
capa_pc_ccpp_general_urbano <- capa_pc_ccpp_general[capa_pc_ccpp_general$ambito == "Urbano no EPS", ]
pequena_ciudad_prest <- read.xlsx(url_prestador)
expo_inund_grd <- st_read(url_expo_inund_grd)
sectores_op <- st_read(url_sectores_op)
colegio_p1 <- st_read(url_colegio_p1)
colegio_p2 <- st_read(url_colegio_p2)
colegio <- rbind(colegio_p1, colegio_p2)


# Desarrollo de dashboard
header <- dashboardHeader(
  title = "Geovisor de la Dirección de Ámbito de la Prestación",
  titleWidth = "100%"
)

body <- dashboardBody(
  tags$head(tags$style(HTML('
        .skin-blue .main-header .logo {
          background-color: #12239E;
          font-weight: bold;
          font-size: 24px;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #12239E;
          font-weight: bold;
          font-size: 24px;
        }
        .popup-table {
        border-collapse: collapse;
        width: 100%;
        border: 1px solid black;
        }
        .popup-table th, .popup-table td {
        border: 1px solid black;
        padding: 8px;
        text-align: left;
        }
        .popup-table th {
        background-color: #f2f2f2;
        }
      '))),
  fluidRow(
    column(width = 3,
           box(width = NULL, status = "primary",
               selectInput("departamento", "Selecciona un departamento",
                           choices = c("Seleccionar todos los departamentos", sort(depa$nomdep)),
                           selected = "Seleccionar todos los departamentos"),
               selectInput("provincia", "Selecciona una provincia",
                           choices = NULL,
                           selected = NULL),
               selectInput("distrito", "Selecciona un distrito",
                           choices = NULL,
                           selected = NULL)
           )
           # box(width = NULL, status="primary",
           #     tags$div(
           #       style = "text-align: center;",
           #       tags$h4(
           #         style = "font-weight: bold;",
           #         "Leyenda"
           #       ),
           #       tags$br(),
           #       tags$img(src = "https://cdn-icons-png.flaticon.com/512/1843/1843893.png", width = 30, height = 30),
           #       "Reservorio",
           #       tags$br(),
           #       tags$br(),
           #       tags$img(src = "https://cdn-icons-png.flaticon.com/512/5371/5371132.png", width = 30, height = 30),
           #       "Captación",
           #       tags$br(),
           #       tags$br(),
           #       tags$img(src = "https://cdn-icons-png.flaticon.com/512/10708/10708424.png", width = 30, height = 30),
           #       "PTAR",
           #       tags$br(),
           #       tags$br(),
           #       tags$img(src = "https://cdn-icons-png.flaticon.com/512/8846/8846576.png", width = 30, height = 30),
           #       "PTAP",
           #       tags$br(),
           #       tags$br(),
           #       tags$img(src="https://cdn-icons-png.flaticon.com/512/616/616546.png",width=30,height=30),
           #       "EPS",
           #       tags$br(),
           #       tags$br(),
           #       tags$img(src="https://cdn-icons-png.flaticon.com/512/1080/1080985.png", width=30, heiht=30),
           #       "Colegios")
           # )
    ),
    column(width = 9,
           box(width = NULL, solidHeader = TRUE,
               leafletOutput("map", height = 600)
           )
    )
  ),
  fluidRow(
    column(width = 12,
           box(title="Indicadores Relevantes",width = NULL, solidHeader = TRUE,
               collapsible = TRUE,
               
               valueBoxOutput("progressBox"),
               
               valueBoxOutput("numPrest"),
               
               valueBoxOutput("numCCPP")
               
           ))
  ),
  fluidRow(
    column(width = 6,
           box(width = NULL, solidHeader = TRUE,
               plotlyOutput("plot_out")
           )),
    column(width = 6,
           box(width = NULL, solidHeader = TRUE,
               plotlyOutput("plot_pc")
           ))
  ),
  fluidRow(
    column(width = 12,
           box(width = NULL, solidHeader = TRUE,
               DT::dataTableOutput("table_ccpp_pc")
           ))
  ),
  # Agrega la siguiente línea de código
  tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/2.0.8/clipboard.min.js"),
    tags$script(
      HTML(
        'function copyToClipboard(text) {
           var dummy = document.createElement("textarea");
           document.body.appendChild(dummy);
           dummy.value = text;
           dummy.select();
           document.execCommand("copy");
           document.body.removeChild(dummy);
         }
         Shiny.addCustomMessageHandler("copyToClipboard", copyToClipboard);'
      )
    )
  ),
  tags$br(),
  tags$footer(
    #position: fixed;
    tags$style(
      HTML(
        "
      footer {

        bottom: 0;
        width: 100%;
        text-align: center;
        padding: 20px;
        background-color: #333;
        color: #fff;
      }
      .footer-content {
        display: flex;
        justify-content: space-between;
        align-items: center;
      }
      .footer-section {
        flex: 1;
        text-align: left;
      }
      "
      )
    ),
    div(
      class = "footer-content",
      div(
        class = "footer-section",
        h4("Desarrollado por"),
        p("Dirección de Ámbito de la Prestación")
      ),
      div(
        class = "footer-section",
        h4("Contacto"),
        p("Correo: alazaro@sunass.gob.pe")
      ),
      div(
        class = "footer-section",
        h4("Enlaces de interés"),
        p(
          a("Sunass", href = "https://www.sunass.gob.pe/sunass/quienes-somos/", target = "_blank"),
          " | ",
          a("Escale - Minedu", href = "https://escale.minedu.gob.pe/", target = "_blank")
        )
      )
    )
  )
)

ui <- dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)


server <- function(input, output, session) {
  output$map <- renderLeaflet({
    # Filtrar las provincias relacionadas con el departamento seleccionado
    selected_prov <- prov[prov$nomdep == input$departamento, ]
    
    # Filtrar los puntos de reservorio relacionados con el departamento seleccionado
    selected_res <- res[res$nomdep == input$departamento, ]
    
    selected_capta <- capta[capta$nomdep == input$departamento, ]
    
    selected_ptap <- ptap[ptap$nomdep == input$departamento, ]
    
    selected_ptar <- ptar[ptar$nomdep == input$departamento, ]
    
    selected_eps <- eps[eps$NOMDEP == input$departamento, ]
    
    selected_expo_inund_grd <- expo_inund_grd[expo_inund_grd$nomdep == input$departamento, ]
    
    selected_sectores_op <- sectores_op[sectores_op$nomdep == input$departamento, ]
    
    selected_capa_pc_ccpp <- capa_pc_ccpp[capa_pc_ccpp$nomdep == input$departamento, ]
    
    selected_capa_pc_ccpp_general_rural <- capa_pc_ccpp_general_rural[capa_pc_ccpp_general_rural$NOMDEP == input$departamento, ]
    
    selected_capa_pc_ccpp_general_pc <- capa_pc_ccpp_general_pc[capa_pc_ccpp_general_pc$NOMDEP == input$departamento, ]
    
    selected_capa_pc_ccpp_general_urbano <- capa_pc_ccpp_general_urbano[capa_pc_ccpp_general_urbano$NOMDEP == input$departamento, ]
    
    selected_colegio <- colegio[colegio$nomdep == input$departamento, ]
    
    reservoir_icon <- makeIcon(
      iconUrl = "https://cdn-icons-png.flaticon.com/512/1843/1843893.png",  # Replace with the path to your reservoir icon
      iconWidth = 30, iconHeight = 30
    )
    
    capta_icon <- makeIcon(
      iconUrl = "https://cdn-icons-png.flaticon.com/512/5371/5371132.png",  # Replace with the path to your reservoir icon
      iconWidth = 30, iconHeight = 30
    )
    
    ptar_icon <- makeIcon(
      iconUrl = "https://cdn-icons-png.flaticon.com/512/10708/10708424.png",  # Replace with the path to your reservoir icon
      iconWidth = 30, iconHeight = 30
    )
    
    ptap_icon <- makeIcon(
      iconUrl = "https://cdn-icons-png.flaticon.com/512/8846/8846576.png",  # Replace with the path to your reservoir icon
      iconWidth = 30, iconHeight = 30
    )
    
    eps_icon <- makeIcon(
      iconUrl = "https://cdn-icons-png.flaticon.com/512/616/616546.png",
      iconWidth = 30, iconHeight = 30
    )
    
    
    pc_icon <- makeIcon(
      iconUrl = "https://cdn-icons-png.flaticon.com/512/565/565665.png",
      iconWidth = 20, iconHeight = 20
    )
    
    rural_icon <- makeIcon(
      iconUrl = "https://cdn-icons-png.flaticon.com/512/4344/4344781.png",
      iconWidth = 20, iconHeight = 20
    )
    
    urbano_icon = makeIcon(
      iconUrl = "https://cdn-icons-png.flaticon.com/512/4274/4274096.png",
      iconWidth = 20, iconHeight = 20
    )
    
    colegios_icon <- makeIcon(
      iconUrl = "https://cdn-icons-png.flaticon.com/512/1080/1080985.png",
      iconWidth = 30, iconHeight = 30
    )
    
    hospital_icon <- makeIcon(
      iconUrl = "https://cdn-icons-png.flaticon.com/512/3606/3606564.png",
      iconWidth = 30, iconHeight = 30
    )
    
    leaflet() %>%
      #addProviderTiles("Esri.WorldTopoMap") %>%
      #addProviderTiles("WorldTerrain", group = "TopoMap") %>%
      addProviderTiles("Esri.WorldTopoMap", group = "Esri") %>%
      addTiles(urlTemplate = "http://www.google.cn/maps/vt?lyrs=s@189&gl=cn&x={x}&y={y}&z={z}", group = "Terrain") %>%
      # addProviderTiles("Esri.WorldImagery", group = "Terrain") %>% 
      addSearchOSM() %>%
      setView(lng = -76.5, lat = -9, zoom = 5.4) %>% 
      #addRasterImage(mi_raster,opacity = 0.7, colors = raster_pal, group="GRD") %>% 
      # addMarkers(data = selected_res,
      #            lng = ~COORDX,
      #            lat = ~COORDY,
      #            icon = reservoir_icon,
      #            popup = ~paste(
      #              "<table class='popup-table'>",
      #              "<tr><th>Nombre de la EPS</th><td>", NOMEPS, "</td></tr>",
      #              "<tr><th>Estado operativo</th><td>", ESTADOOP, "</td></tr>",
      #              "</table>"
      #            ),
      #            group = "Reservorio<img src='https://cdn-icons-png.flaticon.com/512/1843/1843893.png' width='20' height='20'>") %>%
      # addMarkers(data = selected_capta,
      #            lng = ~X,
      #            lat = ~Y,
      #            icon = capta_icon,
      #            popup = ~paste(
      #              "<table class='popup-table'>",
      #              "<tr><th>Nombre de la captacion</th><td>", Nombre_BD, "</td></tr>",
      #              "<tr><th>Tipo de captacion</th><td>", Tipo_cap, "</td></tr>",
      #              "</table>"
      #            ),
      #            group = "Captacion<img src='https://cdn-icons-png.flaticon.com/512/5371/5371132.png' width='20' height='20'>") %>%
      # addMarkers(data = selected_ptap,
      #            lng = ~COORDX,
      #            lat = ~COORDY,
      #            icon = ptap_icon,
      #            popup = ~paste(
      #              "<table class='popup-table'>",
      #              "<tr><th>Nombre de la PTAP</th><td>", NOMPTAP, "</td></tr>",
      #              "<tr><th>Nombre de localidad</th><td>", NOMLOCALID, "</td></tr>",
      #              "</table>"
      #            ),
      #            group = "PTAP <img src='https://cdn-icons-png.flaticon.com/512/8846/8846576.png' width='20' height='20'>") %>%
      # addMarkers(data = selected_ptar,
      #            lng = ~X,
      #            lat = ~Y,
      #            icon = ptar_icon,
      #            popup = ~paste(
      #              "<table class='popup-table'>",
      #              "<tr><th>Nombre de la PTAR</th><td>", NOMPTAR, "</td></tr>",
      #              "<tr><th>Nombre de localidad</th><td>", NOMLOCALID, "</td></tr>",
      #              "</table>"
      #            ),
      #            group = "PTAR <img src='https://cdn-icons-png.flaticon.com/512/10708/10708424.png' width='20' height='20'><hr><strong>Servicios de saneamiento:</strong><br>") %>%
      # addMarkers(data = selected_eps,
      #            lng = ~X,
      #            lat = ~Y,
      #            icon = eps_icon,
      #            popup = ~paste(
      #              "<table class='popup-table'>",
      #              "<tr><th>Nombre de la EPS</th><td>", NOMEPS, "</td></tr>",
      #              "<tr><th>Poblacion Ambito</th><td>", POBAMBEPS, "</td></tr>",
      #              "</table>"
      #            ),
      #            group = "EPS<img src='https://cdn-icons-png.flaticon.com/512/616/616546.png' width='20' height='20'>") %>%
      # addMarkers(data = selected_capa_pc_ccpp_general_pc,
      #            lng = ~X,
      #            lat = ~Y,
      #            icon = pc_icon,
      #            popup = ~paste(
      #              "<table class='popup-table'>",
      #              "<tr><th>Nombre del centro poblado</th><td>", NOM_CCPP, "</td></tr>",
      #              "<tr><th>Poblacion</th><td>", POBTOTAL, "</td></tr>",
      #              "<tr><th>Vivienda</th><td>", VIVTOTAL, "</td></tr>",
      #              "<tr><th>Continuidad horas</th><td>", conti_avenida_horas, "</td></tr>",
      #              "<tr><th>Continuidad días</th><td>", conti_avenida_dias, "</td></tr>",
      #              "<tr><th>Conexiones totales de agua</th><td>", Conex_tot_agua, "</td></tr>",
      #              "<tr><th>Conexiones totales de alcantarillado</th><td>", Conex_tot_alca, "</td></tr>",
      #              "</table>"
      #            ),
      #            clusterOptions = markerClusterOptions(),
      #            group = "CCPP Pequeña Ciudad <img src='https://cdn-icons-png.flaticon.com/512/565/565665.png' width='20' height='20'>") %>%
      # addMarkers(data = selected_capa_pc_ccpp_general_rural,
      #            lng = ~X,
      #            lat = ~Y,
      #            icon = rural_icon,
      #            popup = ~paste(
      #              "<table class='popup-table'>",
      #              "<tr><th>Nombre del centro poblado</th><td>", NOM_CCPP, "</td></tr>",
      #              "<tr><th>Poblacion</th><td>", POBTOTAL, "</td></tr>",
      #              "<tr><th>Vivienda</th><td>", VIVTOTAL, "</td></tr>",
      #              "<tr><th>Continuidad horas</th><td>", conti_avenida_horas, "</td></tr>",
      #              "<tr><th>Continuidad días</th><td>", conti_avenida_dias, "</td></tr>",
      #              "<tr><th>Conexiones totales de agua</th><td>", Conex_tot_agua, "</td></tr>",
      #              "<tr><th>Conexiones totales de alcantarillado</th><td>", Conex_tot_alca, "</td></tr>",
      #              "</table>"
      #            ),
      #            clusterOptions = markerClusterOptions(),
      #            group = "CCPP Rural <img src='https://cdn-icons-png.flaticon.com/512/4344/4344781.png' width='20' height='20'>") %>%
      # addMarkers(data = selected_capa_pc_ccpp_general_urbano,
      #            lng = ~X,
      #            lat = ~Y,
      #            icon = urbano_icon,
      #            popup = ~paste(
      #              "<table class='popup-table'>",
      #              "<tr><th>Nombre del centro poblado</th><td>", NOM_CCPP, "</td></tr>",
      #              "<tr><th>Poblacion</th><td>", POBTOTAL, "</td></tr>",
      #              "<tr><th>Vivienda</th><td>", VIVTOTAL, "</td></tr>",
      #              "<tr><th>Continuidad horas</th><td>", conti_avenida_horas, "</td></tr>",
      #              "<tr><th>Continuidad días</th><td>", conti_avenida_dias, "</td></tr>",
      #              "<tr><th>Conexiones totales de agua</th><td>", Conex_tot_agua, "</td></tr>",
      #              "<tr><th>Conexiones totales de alcantarillado</th><td>", Conex_tot_alca, "</td></tr>",
      #              "</table>"
      #            ),
      #            clusterOptions = markerClusterOptions(),
      #            group = "CCPP Pequeña Ciudad Tipo II <img src='https://cdn-icons-png.flaticon.com/512/4274/4274096.png' width='20' height='20'>") %>%
      addPolygons(data=selected_sectores_op,
                  fillColor = "green",
                  color = "green",
                  weight = 2,
                  popup = ~paste(
                    "<table class='popup-table'>",
                    "<tr><th>Nombre del sector</th><td>", SECTOR, "</td></tr>",
                    "<tr><th>Nombre de la EPS</th><td>", NOMEP, "</td></tr>",
                    "</table>"
                  ),
                  highlightOptions = highlightOptions(color = "white", weight = 4, bringToFront = TRUE),
                  group = "Sectores Operacionales") %>%
      # addMarkers(data = selected_colegio,
      #            lng = ~X,
      #            lat = ~Y,
      #            icon = colegios_icon,
      #            popup = ~paste(
      #              "<table class='popup-table'>",
      #              "<tr><th>Nombre del colegio</th><td>", CEN_EDU_L, "</td></tr>",
      #              "<tr><th>¿Riesgo de inundación?</th><td>", dentro_poligono, "</td></tr>",
      #              "</table>"
      #            ),
      #            clusterOptions = markerClusterOptions(),
      #            group = "Colegio <img src='https://cdn-icons-png.flaticon.com/512/1080/1080985.png' width='20' height='20'>") %>%
      #addLayersControl(overlayGroups = c("Departamento", "Provincia","Distrito","Reservorio","Captacion","PTAP","PTAR","EPS","ADP","CategoriasDistrito", "Colegio", "Exposicion de Inundacion - GRD","Sectores Operacionales","Prestador PC")) %>% 
      addLayersControl(baseGroups = c("Esri","Terrain"), 
                       overlayGroups = c("Departamento", "Provincia","Distrito<hr><strong>Infraestructura de saneamiento:</strong>",
                                         "Reservorio<img src='https://cdn-icons-png.flaticon.com/512/1843/1843893.png' width='20' height='20'>",
                                         "Captacion<img src='https://cdn-icons-png.flaticon.com/512/5371/5371132.png' width='20' height='20'>",
                                         "PTAP <img src='https://cdn-icons-png.flaticon.com/512/8846/8846576.png' width='20' height='20'>",
                                         "PTAR <img src='https://cdn-icons-png.flaticon.com/512/10708/10708424.png' width='20' height='20'><hr><strong>Servicios de saneamiento:</strong><br>",
                                         "Sectores Operacionales","EPS<img src='https://cdn-icons-png.flaticon.com/512/616/616546.png' width='20' height='20'>",
                                         "CCPP Pequeña Ciudad <img src='https://cdn-icons-png.flaticon.com/512/565/565665.png' width='20' height='20'>",
                                         "CCPP Rural <img src='https://cdn-icons-png.flaticon.com/512/4344/4344781.png' width='20' height='20'>",
                                         "CCPP Pequeña Ciudad Tipo II <img src='https://cdn-icons-png.flaticon.com/512/4274/4274096.png' width='20' height='20'>","Exposicion de Inundacion - GRD<hr><strong>Información secundaria:</strong>",
                                         "Colegio <img src='https://cdn-icons-png.flaticon.com/512/1080/1080985.png' width='20' height='20'>"
                       )
      ) %>% 
      addMeasure(primaryLengthUnit = "meters") %>% 
      leafem::addMouseCoordinates()
    
    
    
  })
  
  ############### INDICADORES ###############
  output$progressBox <- renderValueBox({
    if (input$departamento == "Seleccionar todos los departamentos") {
      valueBox(
        "Seleccione", "",
        color = "orange"
      )
    } else {
      selected_colegio <- colegio[colegio$nomdep == input$departamento, ]
      selected_colegios_within_polygon <- selected_colegio[selected_colegio$dentro_poligono == "Si", ]
      valueBox(
        nrow(selected_colegios_within_polygon), "Número de colegios con riesgo de inundación", icon = icon("school"),
        color = "orange"
      )
    }
  })
  
  output$numPrest <- renderValueBox({
    if (input$departamento == "Seleccionar todos los departamentos") {
      valueBox(
        "Seleccione", "", 
        color = "blue"
      )
    } else {
      selected_pc_prest <- pequena_ciudad_prest[pequena_ciudad_prest$nomdep == input$departamento, ]
      valueBox(
        nrow(selected_pc_prest), "Número de prestadores de pequeña ciudad", icon = icon("droplet"),
        color = "blue"
      )
    }
  })
  
  output$numCCPP <- renderValueBox({
    if (input$departamento == "Seleccionar todos los departamentos") {
      valueBox(
        "Seleccione", "",
        color = "purple"
      )
    } else {
      selected_pc_ccpp <- capa_pc_ccpp_general_pc[capa_pc_ccpp_general_pc$NOMDEP == input$departamento, ]
      valueBox(
        nrow(selected_pc_ccpp), "Número de pequeñas ciudades", icon = icon("location-dot"),
        color = "purple"
      )
    }
  })
  
  output$plot_out <- renderPlotly({
    if (input$departamento == "Seleccionar todos los departamentos") {
      # Puedes renderizar un mensaje o un gráfico vacío
      plot_ly(labels = c("Seleccione un departamento"), values = c(1), type = "pie") %>%
        layout(title = "Seleccione un departamento")
    } else {
      selected_colegio <- colegio[colegio$nomdep == input$departamento, ]
      selected_colegios_within_polygon <- selected_colegio[selected_colegio$dentro_poligono == "Si", ]
      selected_colegios_without_polygon <- selected_colegio[selected_colegio$dentro_poligono == "No", ]
      
      # Renderiza tu gráfico de pie con los datos específicos
      plot_ly(labels = c("Si", "No"),
              values = c(nrow(selected_colegios_within_polygon), nrow(selected_colegios_without_polygon)), type = "pie",
              textposition = 'inside',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              marker = list(colors = c('#12239E', '#118DFF'),
                            line = list(color = '#FFFFFF', width = 1)),
              #The 'pull' attribute can also be used to create space between the sectors
              showlegend = FALSE) %>%
        layout(title = "# Colegios con riesgo de inundación ",
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
  })
  
  output$plot_pc <- renderPlotly({
    if (input$departamento == "Seleccionar todos los departamentos") {
      # Puedes renderizar un mensaje o un gráfico vacío
      plot_ly(labels = c("Seleccione un departamento"), values = c(1), type = "pie") %>%
        layout(title = "Seleccione un departamento")
    } else {
      selected_pc_ccpp <- capa_pc_ccpp[capa_pc_ccpp$nomdep == input$departamento, ]
      selected_pc_ccpp_within_polygon <- selected_pc_ccpp[selected_pc_ccpp$dentro_poligono == "Si", ]
      selected_pc_ccpp_without_polygon <- selected_pc_ccpp[selected_pc_ccpp$dentro_poligono == "No", ]
      
      # Renderiza tu gráfico de pie con los datos específicos
      plot_ly(labels = c("Si", "No"),
              values = c(nrow(selected_pc_ccpp_within_polygon), nrow(selected_pc_ccpp_without_polygon)), type = "pie",
              textposition = 'inside',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              marker = list(colors = c('#12239E', '#118DFF'),
                            line = list(color = '#FFFFFF', width = 1)),
              #The 'pull' attribute can also be used to create space between the sectors
              showlegend = FALSE) %>%
        layout(title = "# PC con riesgo de inundación",
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
  })
  
  output$table_ccpp_pc <- renderDT({
    data <- data.frame(capa_pc_ccpp_general) %>% 
      select(c("ubigeo_censo_17","NOMDEP","NOMPROV","NOMDIST","NOM_CCPP","POBTOTAL","conti_avenida_horas","Ubic_APC_EPES",
               "conti_avenida_dias","Conex_tot_agua","Conex_tot_alca","cloro_residual_reservorio",
               "cloro_residual_uv","cobra_cuota","nomprest")) %>% 
      rename(
        "Ubigeo" = "ubigeo_censo_17",
        "Departamento" = "NOMDEP",
        "Provincia" = "NOMPROV",
        "Distrito" = "NOMDIST",
        "Centro Poblado" = "NOM_CCPP",
        "Tipo de CCPP" = "Ubic_APC_EPES",
        "Poblacion" = "POBTOTAL",
        "Continuidad (horas/día)" = "conti_avenida_horas",
        "Continuidad (días/semana)" = "conti_avenida_dias",
        "Conex. Tot. Agua" = "Conex_tot_agua",
        "Conex. Tot. Alca" = "Conex_tot_alca",
        "Cloro Residual Reservorio" = "cloro_residual_reservorio",
        "Cloro Residual Casa más lejana" = "cloro_residual_uv",
        "¿Cobra cuota?" = "cobra_cuota",
        "Prestador" = "nomprest"
      )
    
    data_filtered <- data
    
    if (input$departamento != "Seleccionar todos los departamentos"){
      data_filtered <- data_filtered[data_filtered$Departamento == input$departamento,]
    }
    
    if (!is.null(input$provincia) && input$provincia != "Seleccionar todas las provincias" && input$departamento != "Seleccionar todos los departamentos") {
      data_filtered <- data_filtered[data_filtered$Provincia == input$provincia,]
    }

    if (!is.null(input$distrito) && input$distrito != "Seleccionar todos los distritos" && input$provincia != "Seleccionar todas las provincias" && input$departamento != "Seleccionar todos los departamentos") {
      data_filtered <- data_filtered[data_filtered$Distrito == input$distrito,]
    }
    
    datatable(data_filtered,filter = 'top',
              extensions = c("Buttons"), options = list(
                dom = 'Bfrtip',
                buttons = c('excel'),
                scrollX = TRUE
              ))
  })
  
  
  observe({
    selected_dep <- depa[depa$nomdep == input$departamento, ]
    selected_prov <- prov[prov$nomdep == input$departamento, ]
    selected_expo_inund_grd <- expo_inund_grd[expo_inund_grd$nomdep == input$departamento, ]
    selected_capa_pc_ccpp_general_pc_depa <- capa_pc_ccpp_general_pc[(capa_pc_ccpp_general_pc$NOMDEP==input$departamento),]
    selected_capa_pc_ccpp_general_rural_depa <- capa_pc_ccpp_general_rural[(capa_pc_ccpp_general_rural$NOMDEP==input$departamento),]
    selected_capa_pc_ccpp_general_urbano_depa <- capa_pc_ccpp_general_urbano[(capa_pc_ccpp_general_urbano$NOMDEP==input$departamento),]
    selected_res_depa <- res[(res$nomdep==input$departamento),]
    selected_capta_depa <- capta[(capta$nomdep==input$departamento),]
    selected_ptap_depa <- ptap[(ptap$nomdep==input$departamento),]
    selected_ptar_depa <- ptar[(ptar$nomdep==input$departamento),]
    selected_colegio_depa <- colegio[(colegio$nomdep == input$departamento),]
    selected_eps_depa <- eps[(eps$NOMDEP == input$departamento),]
    selected_dep_id <- selected_dep$iddep
    pc_icon <- makeIcon(
      iconUrl = "https://cdn-icons-png.flaticon.com/512/565/565665.png",
      iconWidth = 20, iconHeight = 20
    )
    rural_icon <- makeIcon(
      iconUrl = "https://cdn-icons-png.flaticon.com/512/4344/4344781.png",
      iconWidth = 20, iconHeight = 20
    )
    
    urbano_icon = makeIcon(
      iconUrl = "https://cdn-icons-png.flaticon.com/512/4274/4274096.png",
      iconWidth = 20, iconHeight = 20
    )
    
    reservoir_icon <- makeIcon(
      iconUrl = "https://cdn-icons-png.flaticon.com/512/1843/1843893.png",  # Replace with the path to your reservoir icon
      iconWidth = 30, iconHeight = 30
    )
    
    capta_icon <- makeIcon(
      iconUrl = "https://cdn-icons-png.flaticon.com/512/5371/5371132.png",  # Replace with the path to your reservoir icon
      iconWidth = 30, iconHeight = 30
    )
    
    ptar_icon <- makeIcon(
      iconUrl = "https://cdn-icons-png.flaticon.com/512/10708/10708424.png",  # Replace with the path to your reservoir icon
      iconWidth = 30, iconHeight = 30
    )
    
    ptap_icon <- makeIcon(
      iconUrl = "https://cdn-icons-png.flaticon.com/512/8846/8846576.png",  # Replace with the path to your reservoir icon
      iconWidth = 30, iconHeight = 30
    )
    
    colegios_icon <- makeIcon(
      iconUrl = "https://cdn-icons-png.flaticon.com/512/1080/1080985.png",
      iconWidth = 30, iconHeight = 30
    )
    
    eps_icon <- makeIcon(
      iconUrl = "https://cdn-icons-png.flaticon.com/512/616/616546.png",
      iconWidth = 30, iconHeight = 30
    )
    
    
    # Filtra las provincias relacionadas con el departamento seleccionado
    provincias_dep <- unique(prov$nomprov[prov$iddep == selected_dep_id])
    
    # if (input$departamento != "Seleccionar todos los departamentos") {
    #   leafletProxy("map") %>%
    #     addPolygons(data = selected_expo_inund_grd,
    #                 fillColor = "orange",
    #                 fillOpacity = 0.5,
    #                 color = "white",
    #                 weight = 1,
    #                 highlightOptions = highlightOptions(color = "white", weight = 4,
    #                                                     bringToFront = TRUE),
    #                 group = "Exposicion de Inundacion - GRD")  
    # }
    if (input$departamento != "Seleccionar todos los departamentos") {
      # Ajusta el mapa para que se centre en el polígono seleccionado con un zoom específico
      leafletProxy("map") %>%
        hideGroup("Exposicion de Inundacion - GRD<hr><strong>Información secundaria:</strong>") %>% 
        hideGroup("Sectores Operacionales") %>% 
        clearGroup("Colegio <img src='https://cdn-icons-png.flaticon.com/512/1080/1080985.png' width='20' height='20'>") %>% 
        clearGroup("CCPP Pequeña Ciudad <img src='https://cdn-icons-png.flaticon.com/512/565/565665.png' width='20' height='20'>") %>% 
        clearGroup("CCPP Rural <img src='https://cdn-icons-png.flaticon.com/512/4344/4344781.png' width='20' height='20'>") %>% 
        clearGroup("CCPP Pequeña Ciudad Tipo II <img src='https://cdn-icons-png.flaticon.com/512/4274/4274096.png' width='20' height='20'>") %>% 
        clearGroup("Reservorio<img src='https://cdn-icons-png.flaticon.com/512/1843/1843893.png' width='20' height='20'>") %>% 
        clearGroup("Captacion<img src='https://cdn-icons-png.flaticon.com/512/5371/5371132.png' width='20' height='20'>") %>% 
        clearGroup("PTAP <img src='https://cdn-icons-png.flaticon.com/512/8846/8846576.png' width='20' height='20'>") %>% 
        clearGroup("PTAR <img src='https://cdn-icons-png.flaticon.com/512/10708/10708424.png' width='20' height='20'><hr><strong>Servicios de saneamiento:</strong><br>") %>% 
        clearGroup("EPS<img src='https://cdn-icons-png.flaticon.com/512/616/616546.png' width='20' height='20'>") %>% 
        addPolygons(data = selected_dep,
                    fillColor = "transparent",
                    fillOpacity = 0,
                    color = "black",
                    weight = 3,
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE),
                    popup = ~paste("<b>RESUMEN</b><br>",
                                   "Nombre del Departamento: ", depa$nomdep, "<br>",
                                   "Resolucion ADP: ", depa$ADP_Resoluc),
                    group = "Departamento") %>%
        addPolygons(data = selected_prov,
                    fillColor = "transparent",
                    fillOpacity = 0.5,
                    color = "black",
                    weight = 3,
                    # highlightOptions = highlightOptions(color = "white", weight = 2,
                    #                                     bringToFront = TRUE),
                    # popup = ~paste("<b>RESUMEN PROVINCIA</b><br>",
                    #                "Nombre de la Provincia: ", selected_prov$nomprov),
                    group = "Provincia") %>%
        addLabelOnlyMarkers(data = selected_prov,
                            lng = ~X, lat = ~Y, label = ~nomprov,
                            labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE)) %>% 
        addPolygons(data = selected_expo_inund_grd,
                    fillColor = "orange",
                    fillOpacity = 0.5,
                    color = "white",
                    weight = 1,
                    highlightOptions = highlightOptions(color = "white", weight = 4,
                                                        bringToFront = TRUE),
                    group = "Exposicion de Inundacion - GRD<hr><strong>Información secundaria:</strong>")  %>% 
        addMarkers(data = selected_capa_pc_ccpp_general_pc_depa,
                   lng = ~Longitud,
                   lat = ~Latitud,
                   icon = pc_icon,
                   popup = ~paste(
                     "<table class='popup-table'>",
                     "<tr><th>Nombre del centro poblado</th><td>", NOM_CCPP, "</td></tr>",
                     "<tr><th>Poblacion</th><td>", POBTOTAL, "</td></tr>",
                     "<tr><th>Vivienda</th><td>", VIVTOTAL, "</td></tr>",
                     "<tr><th>Continuidad horas</th><td>", conti_avenida_horas, "</td></tr>",
                     "<tr><th>Continuidad días</th><td>", conti_avenida_dias, "</td></tr>",
                     "<tr><th>Conexiones totales de agua</th><td>", Conex_tot_agua, "</td></tr>",
                     "<tr><th>Conexiones totales de alcantarillado</th><td>", Conex_tot_alca, "</td></tr>",
                     "</table>"
                   ),
                   clusterOptions = markerClusterOptions(),
                   group = "CCPP Pequeña Ciudad <img src='https://cdn-icons-png.flaticon.com/512/565/565665.png' width='20' height='20'>") %>%
        addMarkers(data = selected_capa_pc_ccpp_general_rural_depa,
                   lng = ~Longitud,
                   lat = ~Latitud,
                   icon = rural_icon,
                   popup = ~paste(
                     "<table class='popup-table'>",
                     "<tr><th>Nombre del centro poblado</th><td>", NOM_CCPP, "</td></tr>",
                     "<tr><th>Poblacion</th><td>", POBTOTAL, "</td></tr>",
                     "<tr><th>Vivienda</th><td>", VIVTOTAL, "</td></tr>",
                     "<tr><th>Continuidad horas</th><td>", conti_avenida_horas, "</td></tr>",
                     "<tr><th>Continuidad días</th><td>", conti_avenida_dias, "</td></tr>",
                     "<tr><th>Conexiones totales de agua</th><td>", Conex_tot_agua, "</td></tr>",
                     "<tr><th>Conexiones totales de alcantarillado</th><td>", Conex_tot_alca, "</td></tr>",
                     "</table>"
                   ),
                   clusterOptions = markerClusterOptions(),
                   group = "CCPP Rural <img src='https://cdn-icons-png.flaticon.com/512/4344/4344781.png' width='20' height='20'>") %>%
        addMarkers(data = selected_capa_pc_ccpp_general_urbano_depa,
                   lng = ~Longitud,
                   lat = ~Latitud,
                   icon = urbano_icon,
                   popup = ~paste(
                     "<table class='popup-table'>",
                     "<tr><th>Nombre del centro poblado</th><td>", NOM_CCPP, "</td></tr>",
                     "<tr><th>Poblacion</th><td>", POBTOTAL, "</td></tr>",
                     "<tr><th>Vivienda</th><td>", VIVTOTAL, "</td></tr>",
                     "<tr><th>Continuidad horas</th><td>", conti_avenida_horas, "</td></tr>",
                     "<tr><th>Continuidad días</th><td>", conti_avenida_dias, "</td></tr>",
                     "<tr><th>Conexiones totales de agua</th><td>", Conex_tot_agua, "</td></tr>",
                     "<tr><th>Conexiones totales de alcantarillado</th><td>", Conex_tot_alca, "</td></tr>",
                     "</table>"
                   ),
                   clusterOptions = markerClusterOptions(),
                   group = "CCPP Pequeña Ciudad Tipo II <img src='https://cdn-icons-png.flaticon.com/512/4274/4274096.png' width='20' height='20'>") %>%
        addMarkers(data = selected_res_depa,
                   lng = ~COORDX,
                   lat = ~COORDY,
                   icon = reservoir_icon,
                   popup = ~paste(
                     "<table class='popup-table'>",
                     "<tr><th>Nombre de la EPS</th><td>", NOMEPS, "</td></tr>",
                     "<tr><th>Estado operativo</th><td>", ESTADOOP, "</td></tr>",
                     "</table>"
                   ),
                   group = "Reservorio<img src='https://cdn-icons-png.flaticon.com/512/1843/1843893.png' width='20' height='20'>") %>%
        addMarkers(data = selected_capta_depa,
                   lng = ~X,
                   lat = ~Y,
                   icon = capta_icon,
                   popup = ~paste(
                     "<table class='popup-table'>",
                     "<tr><th>Nombre de la captacion</th><td>", Nombre_BD, "</td></tr>",
                     "<tr><th>Tipo de captacion</th><td>", Tipo_cap, "</td></tr>",
                     "</table>"
                   ),
                   group = "Captacion<img src='https://cdn-icons-png.flaticon.com/512/5371/5371132.png' width='20' height='20'>") %>%
        addMarkers(data = selected_ptap_depa,
                   lng = ~COORDX,
                   lat = ~COORDY,
                   icon = ptap_icon,
                   popup = ~paste(
                     "<table class='popup-table'>",
                     "<tr><th>Nombre de la PTAP</th><td>", NOMPTAP, "</td></tr>",
                     "<tr><th>Nombre de localidad</th><td>", NOMLOCALID, "</td></tr>",
                     "</table>"
                   ),
                   group = "PTAP <img src='https://cdn-icons-png.flaticon.com/512/8846/8846576.png' width='20' height='20'>") %>%
        addMarkers(data = selected_ptar_depa,
                   lng = ~X,
                   lat = ~Y,
                   icon = ptar_icon,
                   popup = ~paste(
                     "<table class='popup-table'>",
                     "<tr><th>Nombre de la PTAR</th><td>", NOMPTAR, "</td></tr>",
                     "<tr><th>Nombre de localidad</th><td>", NOMLOCALID, "</td></tr>",
                     "</table>"
                   ),
                   group = "PTAR <img src='https://cdn-icons-png.flaticon.com/512/10708/10708424.png' width='20' height='20'><hr><strong>Servicios de saneamiento:</strong><br>") %>%
        addMarkers(data = selected_colegio_depa,
                   lng = ~X,
                   lat = ~Y,
                   icon = colegios_icon,
                   popup = ~paste(
                     "<table class='popup-table'>",
                     "<tr><th>Nombre del colegio</th><td>", CEN_EDU_L, "</td></tr>",
                     "<tr><th>¿Riesgo de inundación?</th><td>", dentro_poligono, "</td></tr>",
                     "</table>"
                   ),
                   clusterOptions = markerClusterOptions(),
                   group = "Colegio <img src='https://cdn-icons-png.flaticon.com/512/1080/1080985.png' width='20' height='20'>") %>%
        addMarkers(data = selected_eps_depa,
                   lng = ~X,
                   lat = ~Y,
                   icon = eps_icon,
                   popup = ~paste(
                     "<table class='popup-table'>",
                     "<tr><th>Nombre de la EPS</th><td>", NOMEPS, "</td></tr>",
                     "<tr><th>Poblacion Ambito</th><td>", POBAMBEPS, "</td></tr>",
                     "</table>"
                   ),
                   group = "EPS<img src='https://cdn-icons-png.flaticon.com/512/616/616546.png' width='20' height='20'>") %>%
        setView(lng = selected_dep$X,
                lat = selected_dep$Y,
                zoom = 8)
      
      # Actualiza las opciones del segundo input
      updateSelectInput(session, "provincia", choices = c("Seleccionar todas las provincias", sort(provincias_dep)))
    } else {
      leafletProxy("map") %>% 
        clearGroup("Provincia") %>% 
        hideGroup("Exposicion de Inundacion - GRD<hr><strong>Información secundaria:</strong>") %>% 
        clearGroup("CCPP Pequeña Ciudad <img src='https://cdn-icons-png.flaticon.com/512/565/565665.png' width='20' height='20'>") %>% 
        clearGroup("CCPP Rural <img src='https://cdn-icons-png.flaticon.com/512/4344/4344781.png' width='20' height='20'>") %>% 
        clearGroup("CCPP Pequeña Ciudad Tipo II <img src='https://cdn-icons-png.flaticon.com/512/4274/4274096.png' width='20' height='20'>") %>% 
        clearGroup("Reservorio<img src='https://cdn-icons-png.flaticon.com/512/1843/1843893.png' width='20' height='20'>") %>% 
        clearGroup("Captacion<img src='https://cdn-icons-png.flaticon.com/512/5371/5371132.png' width='20' height='20'>") %>% 
        clearGroup("PTAP <img src='https://cdn-icons-png.flaticon.com/512/8846/8846576.png' width='20' height='20'>") %>% 
        clearGroup("PTAR <img src='https://cdn-icons-png.flaticon.com/512/10708/10708424.png' width='20' height='20'><hr><strong>Servicios de saneamiento:</strong><br>") %>% 
        hideGroup("Sectores Operacionales") %>% 
        clearGroup("Colegio <img src='https://cdn-icons-png.flaticon.com/512/1080/1080985.png' width='20' height='20'>") %>% 
        clearGroup("EPS<img src='https://cdn-icons-png.flaticon.com/512/616/616546.png' width='20' height='20'>") %>% 
        addPolygons(data = selected_dep,
                    fillColor = "transparent",
                    fillOpacity = 0,
                    color = "black",
                    weight = 2,
                    highlightOptions = highlightOptions(color = "white", weight = 4,
                                                        bringToFront = TRUE),
                    popup = ~paste("<b>RESUMEN</b><br>",
                                   "Nombre del Departamento: ", depa$nomdep, "<br>",
                                   "Resolucion ADP: ", depa$ADP_Resoluc),
                    group = "Departamento") 
      # Si se selecciona "Seleccionar todos los departamentos", limpia el input de provincia
      updateSelectInput(session, "provincia", choices = NULL, selected = NULL)
    }
  })
  
  
  observe({
    selected_dep <- depa[depa$nomdep == input$departamento, ]
    selected_dep_id <- depa$iddep[depa$nomdep == input$departamento]
    selected_prov_id <- prov$idprov[(prov$nomprov == input$provincia) & (prov$nomdep==input$departamento)]
    selected_capa_pc_ccpp_general_pc_depa <- capa_pc_ccpp_general_pc[(capa_pc_ccpp_general_pc$NOMDEP==input$departamento),]
    selected_capa_pc_ccpp_general_rural_depa <- capa_pc_ccpp_general_rural[(capa_pc_ccpp_general_rural$NOMDEP==input$departamento),]
    selected_capa_pc_ccpp_general_urbano_depa <- capa_pc_ccpp_general_urbano[(capa_pc_ccpp_general_urbano$NOMDEP==input$departamento),]
    selected_capa_pc_ccpp_general_pc_prov <- selected_capa_pc_ccpp_general_pc_depa[(selected_capa_pc_ccpp_general_pc_depa$NOMDEP==input$departamento)&(selected_capa_pc_ccpp_general_pc_depa$NOMPROV==input$provincia),]
    selected_capa_pc_ccpp_general_rural_prov <- selected_capa_pc_ccpp_general_rural_depa[(selected_capa_pc_ccpp_general_rural_depa$NOMDEP==input$departamento)&(selected_capa_pc_ccpp_general_rural_depa$NOMPROV==input$provincia),]
    selected_capa_pc_ccpp_general_urbano_prov <- selected_capa_pc_ccpp_general_urbano_depa[(selected_capa_pc_ccpp_general_urbano_depa$NOMDEP==input$departamento)&(selected_capa_pc_ccpp_general_urbano_depa$NOMPROV==input$provincia),]
    selected_res_depa <- res[(res$nomdep==input$departamento),]
    selected_res_prov <- res[(res$nomdep==input$departamento) & (res$nomprov==input$provincia),]
    selected_capta_depa <- capta[(capta$nomdep==input$departamento),]
    selected_capta_prov <- capta[(capta$nomdep==input$departamento) & (capta$nomprov==input$provincia),]
    selected_ptap_depa <- ptap[(ptap$nomdep==input$departamento),]
    selected_ptap_prov <- ptap[(ptap$nomdep==input$departamento) & (ptap$nomprov==input$provincia),]
    selected_ptar_depa <- ptar[(ptar$nomdep==input$departamento),]
    selected_ptar_prov <- ptar[(ptar$nomdep==input$departamento) & (ptar$nomprov==input$provincia),]
    selected_colegio_depa <- colegio[(colegio$nomdep == input$departamento),]
    selected_colegio_prov <- colegio[(colegio$nomdep==input$departamento) & (colegio$nomprov==input$provincia),]
    selected_eps_depa <- eps[(eps$NOMDEP == input$departamento),]
    selected_eps_prov <- eps[(eps$NOMDEP==input$departamento) & (eps$nomprov==input$provincia),]
    
    pc_icon <- makeIcon(
      iconUrl = "https://cdn-icons-png.flaticon.com/512/565/565665.png",
      iconWidth = 20, iconHeight = 20
    )
    rural_icon <- makeIcon(
      iconUrl = "https://cdn-icons-png.flaticon.com/512/4344/4344781.png",
      iconWidth = 20, iconHeight = 20
    )
    
    urbano_icon = makeIcon(
      iconUrl = "https://cdn-icons-png.flaticon.com/512/4274/4274096.png",
      iconWidth = 20, iconHeight = 20
    )
    
    reservoir_icon <- makeIcon(
      iconUrl = "https://cdn-icons-png.flaticon.com/512/1843/1843893.png",  # Replace with the path to your reservoir icon
      iconWidth = 30, iconHeight = 30
    )
    
    capta_icon <- makeIcon(
      iconUrl = "https://cdn-icons-png.flaticon.com/512/5371/5371132.png",  # Replace with the path to your reservoir icon
      iconWidth = 30, iconHeight = 30
    )
    
    ptar_icon <- makeIcon(
      iconUrl = "https://cdn-icons-png.flaticon.com/512/10708/10708424.png",  # Replace with the path to your reservoir icon
      iconWidth = 30, iconHeight = 30
    )
    
    ptap_icon <- makeIcon(
      iconUrl = "https://cdn-icons-png.flaticon.com/512/8846/8846576.png",  # Replace with the path to your reservoir icon
      iconWidth = 30, iconHeight = 30
    )
    
    colegios_icon <- makeIcon(
      iconUrl = "https://cdn-icons-png.flaticon.com/512/1080/1080985.png",
      iconWidth = 30, iconHeight = 30
    )
    
    eps_icon <- makeIcon(
      iconUrl = "https://cdn-icons-png.flaticon.com/512/616/616546.png",
      iconWidth = 30, iconHeight = 30
    )
    
    # Filtra los distritos relacionados con la provincia seleccionada
    #selected_dist <- dist[dist$idprov == selected_prov_id, ]
    selected_dist <- dist[dist$iddep == selected_dep_id & dist$idprov == selected_prov_id,  ]
    print(selected_dist$nomdist)
    
    if (!is.null(input$provincia) && input$provincia != "Seleccionar todas las provincias" && input$departamento != "Seleccionar todos los departamentos") {
      leafletProxy("map") %>%
        hideGroup("Departamento") %>%
        hideGroup("Provincia") %>%
        clearGroup("Distrito<hr><strong>Infraestructura de saneamiento:</strong>") %>%
        clearGroup("CCPP Pequeña Ciudad <img src='https://cdn-icons-png.flaticon.com/512/565/565665.png' width='20' height='20'>") %>% 
        clearGroup("CCPP Rural <img src='https://cdn-icons-png.flaticon.com/512/4344/4344781.png' width='20' height='20'>") %>% 
        clearGroup("CCPP Pequeña Ciudad Tipo II <img src='https://cdn-icons-png.flaticon.com/512/4274/4274096.png' width='20' height='20'>") %>% 
        clearGroup("Reservorio<img src='https://cdn-icons-png.flaticon.com/512/1843/1843893.png' width='20' height='20'>") %>% 
        clearGroup("Captacion<img src='https://cdn-icons-png.flaticon.com/512/5371/5371132.png' width='20' height='20'>") %>% 
        clearGroup("PTAP <img src='https://cdn-icons-png.flaticon.com/512/8846/8846576.png' width='20' height='20'>") %>% 
        clearGroup("PTAR <img src='https://cdn-icons-png.flaticon.com/512/10708/10708424.png' width='20' height='20'><hr><strong>Servicios de saneamiento:</strong><br>") %>% 
        hideGroup("Exposicion de Inundacion - GRD<hr><strong>Información secundaria:</strong>") %>% 
        hideGroup("Sectores Operacionales") %>% 
        clearGroup("Colegio <img src='https://cdn-icons-png.flaticon.com/512/1080/1080985.png' width='20' height='20'>") %>% 
        clearGroup("EPS<img src='https://cdn-icons-png.flaticon.com/512/616/616546.png' width='20' height='20'>") %>% 
        addPolygons(data = selected_dist,
                    fillColor = "transparent",
                    fillOpacity = 0.5,
                    color = "black",
                    weight = 2,
                    # highlightOptions = highlightOptions(color = "white", weight = 4,
                    #                                     bringToFront = TRUE),
                    # popup = ~paste("<b>RESUMEN DISTRITO</b><br>",
                    #                "Nombre del Distrito: ", selected_dist$nomdist),
                    group = "Distrito<hr><strong>Infraestructura de saneamiento:</strong>") %>%
        # addLabelOnlyMarkers(data = selected_dist,
        #                     lng = ~X, lat = ~Y, label = ~nomdist,
        #                     labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE)) %>% 
        addMarkers(data = selected_capa_pc_ccpp_general_pc_prov,
                   lng = ~Longitud,
                   lat = ~Latitud,
                   icon = pc_icon,
                   popup = ~paste(
                     "<table class='popup-table'>",
                     "<tr><th>Nombre del centro poblado</th><td>", NOM_CCPP, "</td></tr>",
                     "<tr><th>Poblacion</th><td>", POBTOTAL, "</td></tr>",
                     "<tr><th>Vivienda</th><td>", VIVTOTAL, "</td></tr>",
                     "<tr><th>Continuidad horas</th><td>", conti_avenida_horas, "</td></tr>",
                     "<tr><th>Continuidad días</th><td>", conti_avenida_dias, "</td></tr>",
                     "<tr><th>Conexiones totales de agua</th><td>", Conex_tot_agua, "</td></tr>",
                     "<tr><th>Conexiones totales de alcantarillado</th><td>", Conex_tot_alca, "</td></tr>",
                     "</table>"
                   ),
                   clusterOptions = markerClusterOptions(),
                   group = "CCPP Pequeña Ciudad <img src='https://cdn-icons-png.flaticon.com/512/565/565665.png' width='20' height='20'>") %>%
        addMarkers(data = selected_capa_pc_ccpp_general_rural_prov,
                   lng = ~Longitud,
                   lat = ~Latitud,
                   icon = rural_icon,
                   popup = ~paste(
                     "<table class='popup-table'>",
                     "<tr><th>Nombre del centro poblado</th><td>", NOM_CCPP, "</td></tr>",
                     "<tr><th>Poblacion</th><td>", POBTOTAL, "</td></tr>",
                     "<tr><th>Vivienda</th><td>", VIVTOTAL, "</td></tr>",
                     "<tr><th>Continuidad horas</th><td>", conti_avenida_horas, "</td></tr>",
                     "<tr><th>Continuidad días</th><td>", conti_avenida_dias, "</td></tr>",
                     "<tr><th>Conexiones totales de agua</th><td>", Conex_tot_agua, "</td></tr>",
                     "<tr><th>Conexiones totales de alcantarillado</th><td>", Conex_tot_alca, "</td></tr>",
                     "</table>"
                   ),
                   clusterOptions = markerClusterOptions(),
                   group = "CCPP Rural <img src='https://cdn-icons-png.flaticon.com/512/4344/4344781.png' width='20' height='20'>") %>%
        addMarkers(data = selected_capa_pc_ccpp_general_urbano_prov,
                   lng = ~Longitud,
                   lat = ~Latitud,
                   icon = urbano_icon,
                   popup = ~paste(
                     "<table class='popup-table'>",
                     "<tr><th>Nombre del centro poblado</th><td>", NOM_CCPP, "</td></tr>",
                     "<tr><th>Poblacion</th><td>", POBTOTAL, "</td></tr>",
                     "<tr><th>Vivienda</th><td>", VIVTOTAL, "</td></tr>",
                     "<tr><th>Continuidad horas</th><td>", conti_avenida_horas, "</td></tr>",
                     "<tr><th>Continuidad días</th><td>", conti_avenida_dias, "</td></tr>",
                     "<tr><th>Conexiones totales de agua</th><td>", Conex_tot_agua, "</td></tr>",
                     "<tr><th>Conexiones totales de alcantarillado</th><td>", Conex_tot_alca, "</td></tr>",
                     "</table>"
                   ),
                   clusterOptions = markerClusterOptions(),
                   group = "CCPP Pequeña Ciudad Tipo II <img src='https://cdn-icons-png.flaticon.com/512/4274/4274096.png' width='20' height='20'>") %>%
        addMarkers(data = selected_res_prov,
                   lng = ~COORDX,
                   lat = ~COORDY,
                   icon = reservoir_icon,
                   popup = ~paste(
                     "<table class='popup-table'>",
                     "<tr><th>Nombre de la EPS</th><td>", NOMEPS, "</td></tr>",
                     "<tr><th>Estado operativo</th><td>", ESTADOOP, "</td></tr>",
                     "</table>"
                   ),
                   group = "Reservorio<img src='https://cdn-icons-png.flaticon.com/512/1843/1843893.png' width='20' height='20'>") %>%
        addMarkers(data = selected_capta_prov,
                   lng = ~X,
                   lat = ~Y,
                   icon = capta_icon,
                   popup = ~paste(
                     "<table class='popup-table'>",
                     "<tr><th>Nombre de la captacion</th><td>", Nombre_BD, "</td></tr>",
                     "<tr><th>Tipo de captacion</th><td>", Tipo_cap, "</td></tr>",
                     "</table>"
                   ),
                   group = "Captacion<img src='https://cdn-icons-png.flaticon.com/512/5371/5371132.png' width='20' height='20'>") %>%
        addMarkers(data = selected_ptap_prov,
                   lng = ~COORDX,
                   lat = ~COORDY,
                   icon = ptap_icon,
                   popup = ~paste(
                     "<table class='popup-table'>",
                     "<tr><th>Nombre de la PTAP</th><td>", NOMPTAP, "</td></tr>",
                     "<tr><th>Nombre de localidad</th><td>", NOMLOCALID, "</td></tr>",
                     "</table>"
                   ),
                   group = "PTAP <img src='https://cdn-icons-png.flaticon.com/512/8846/8846576.png' width='20' height='20'>") %>%
        addMarkers(data = selected_ptar_prov,
                   lng = ~X,
                   lat = ~Y,
                   icon = ptar_icon,
                   popup = ~paste(
                     "<table class='popup-table'>",
                     "<tr><th>Nombre de la PTAR</th><td>", NOMPTAR, "</td></tr>",
                     "<tr><th>Nombre de localidad</th><td>", NOMLOCALID, "</td></tr>",
                     "</table>"
                   ),
                   group = "PTAR <img src='https://cdn-icons-png.flaticon.com/512/10708/10708424.png' width='20' height='20'><hr><strong>Servicios de saneamiento:</strong><br>") %>%
        addMarkers(data = selected_colegio_prov,
                   lng = ~X,
                   lat = ~Y,
                   icon = colegios_icon,
                   popup = ~paste(
                     "<table class='popup-table'>",
                     "<tr><th>Nombre del colegio</th><td>", CEN_EDU_L, "</td></tr>",
                     "<tr><th>¿Riesgo de inundación?</th><td>", dentro_poligono, "</td></tr>",
                     "</table>"
                   ),
                   clusterOptions = markerClusterOptions(),
                   group = "Colegio <img src='https://cdn-icons-png.flaticon.com/512/1080/1080985.png' width='20' height='20'>") %>%
        addMarkers(data = selected_eps_prov,
                   lng = ~X,
                   lat = ~Y,
                   icon = eps_icon,
                   popup = ~paste(
                     "<table class='popup-table'>",
                     "<tr><th>Nombre de la EPS</th><td>", NOMEPS, "</td></tr>",
                     "<tr><th>Poblacion Ambito</th><td>", POBAMBEPS, "</td></tr>",
                     "</table>"
                   ),
                   group = "EPS<img src='https://cdn-icons-png.flaticon.com/512/616/616546.png' width='20' height='20'>") %>%
        setView(lng = mean(st_coordinates(selected_dist)[, 1]),
                lat = mean(st_coordinates(selected_dist)[, 2]),
                zoom = 10) 
      # Actualiza las opciones del tercer input
      updateSelectInput(session, "distrito", choices = c("Seleccionar todos los distritos", sort(selected_dist$nomdist)))
    } else {
      # Si no se ha seleccionado ninguna provincia, muestra la Capa 2 de provincia y limpia el input de distrito
      leafletProxy("map") %>%
        addMarkers(data = selected_capa_pc_ccpp_general_pc_depa,
                   lng = ~Longitud,
                   lat = ~Latitud,
                   icon = pc_icon,
                   popup = ~paste(
                     "<table class='popup-table'>",
                     "<tr><th>Nombre del centro poblado</th><td>", NOM_CCPP, "</td></tr>",
                     "<tr><th>Poblacion</th><td>", POBTOTAL, "</td></tr>",
                     "<tr><th>Vivienda</th><td>", VIVTOTAL, "</td></tr>",
                     "<tr><th>Continuidad horas</th><td>", conti_avenida_horas, "</td></tr>",
                     "<tr><th>Continuidad días</th><td>", conti_avenida_dias, "</td></tr>",
                     "<tr><th>Conexiones totales de agua</th><td>", Conex_tot_agua, "</td></tr>",
                     "<tr><th>Conexiones totales de alcantarillado</th><td>", Conex_tot_alca, "</td></tr>",
                     "</table>"
                   ),
                   clusterOptions = markerClusterOptions(),
                   group = "CCPP Pequeña Ciudad <img src='https://cdn-icons-png.flaticon.com/512/565/565665.png' width='20' height='20'>") %>%
        addMarkers(data = selected_capa_pc_ccpp_general_rural_depa,
                   lng = ~Longitud,
                   lat = ~Latitud,
                   icon = rural_icon,
                   popup = ~paste(
                     "<table class='popup-table'>",
                     "<tr><th>Nombre del centro poblado</th><td>", NOM_CCPP, "</td></tr>",
                     "<tr><th>Poblacion</th><td>", POBTOTAL, "</td></tr>",
                     "<tr><th>Vivienda</th><td>", VIVTOTAL, "</td></tr>",
                     "<tr><th>Continuidad horas</th><td>", conti_avenida_horas, "</td></tr>",
                     "<tr><th>Continuidad días</th><td>", conti_avenida_dias, "</td></tr>",
                     "<tr><th>Conexiones totales de agua</th><td>", Conex_tot_agua, "</td></tr>",
                     "<tr><th>Conexiones totales de alcantarillado</th><td>", Conex_tot_alca, "</td></tr>",
                     "</table>"
                   ),
                   clusterOptions = markerClusterOptions(),
                   group = "CCPP Rural <img src='https://cdn-icons-png.flaticon.com/512/4344/4344781.png' width='20' height='20'>") %>%
        addMarkers(data = selected_capa_pc_ccpp_general_urbano_depa,
                   lng = ~Longitud,
                   lat = ~Latitud,
                   icon = urbano_icon,
                   popup = ~paste(
                     "<table class='popup-table'>",
                     "<tr><th>Nombre del centro poblado</th><td>", NOM_CCPP, "</td></tr>",
                     "<tr><th>Poblacion</th><td>", POBTOTAL, "</td></tr>",
                     "<tr><th>Vivienda</th><td>", VIVTOTAL, "</td></tr>",
                     "<tr><th>Continuidad horas</th><td>", conti_avenida_horas, "</td></tr>",
                     "<tr><th>Continuidad días</th><td>", conti_avenida_dias, "</td></tr>",
                     "<tr><th>Conexiones totales de agua</th><td>", Conex_tot_agua, "</td></tr>",
                     "<tr><th>Conexiones totales de alcantarillado</th><td>", Conex_tot_alca, "</td></tr>",
                     "</table>"
                   ),
                   clusterOptions = markerClusterOptions(),
                   group = "CCPP Pequeña Ciudad Tipo II <img src='https://cdn-icons-png.flaticon.com/512/4274/4274096.png' width='20' height='20'>") %>%
        addMarkers(data = selected_res_depa,
                   lng = ~COORDX,
                   lat = ~COORDY,
                   icon = reservoir_icon,
                   popup = ~paste(
                     "<table class='popup-table'>",
                     "<tr><th>Nombre de la EPS</th><td>", NOMEPS, "</td></tr>",
                     "<tr><th>Estado operativo</th><td>", ESTADOOP, "</td></tr>",
                     "</table>"
                   ),
                   group = "Reservorio<img src='https://cdn-icons-png.flaticon.com/512/1843/1843893.png' width='20' height='20'>") %>%
        addMarkers(data = selected_capta_depa,
                   lng = ~X,
                   lat = ~Y,
                   icon = capta_icon,
                   popup = ~paste(
                     "<table class='popup-table'>",
                     "<tr><th>Nombre de la captacion</th><td>", Nombre_BD, "</td></tr>",
                     "<tr><th>Tipo de captacion</th><td>", Tipo_cap, "</td></tr>",
                     "</table>"
                   ),
                   group = "Captacion<img src='https://cdn-icons-png.flaticon.com/512/5371/5371132.png' width='20' height='20'>") %>%
        addMarkers(data = selected_ptap_depa,
                   lng = ~COORDX,
                   lat = ~COORDY,
                   icon = ptap_icon,
                   popup = ~paste(
                     "<table class='popup-table'>",
                     "<tr><th>Nombre de la PTAP</th><td>", NOMPTAP, "</td></tr>",
                     "<tr><th>Nombre de localidad</th><td>", NOMLOCALID, "</td></tr>",
                     "</table>"
                   ),
                   group = "PTAP <img src='https://cdn-icons-png.flaticon.com/512/8846/8846576.png' width='20' height='20'>") %>%
        addMarkers(data = selected_ptar_depa,
                   lng = ~X,
                   lat = ~Y,
                   icon = ptar_icon,
                   popup = ~paste(
                     "<table class='popup-table'>",
                     "<tr><th>Nombre de la PTAR</th><td>", NOMPTAR, "</td></tr>",
                     "<tr><th>Nombre de localidad</th><td>", NOMLOCALID, "</td></tr>",
                     "</table>"
                   ),
                   group = "PTAR <img src='https://cdn-icons-png.flaticon.com/512/10708/10708424.png' width='20' height='20'><hr><strong>Servicios de saneamiento:</strong><br>") %>%
        addMarkers(data = selected_colegio_depa,
                   lng = ~X,
                   lat = ~Y,
                   icon = colegios_icon,
                   popup = ~paste(
                     "<table class='popup-table'>",
                     "<tr><th>Nombre del colegio</th><td>", CEN_EDU_L, "</td></tr>",
                     "<tr><th>¿Riesgo de inundación?</th><td>", dentro_poligono, "</td></tr>",
                     "</table>"
                   ),
                   clusterOptions = markerClusterOptions(),
                   group = "Colegio <img src='https://cdn-icons-png.flaticon.com/512/1080/1080985.png' width='20' height='20'>") %>%
        addMarkers(data = selected_eps_depa,
                   lng = ~X,
                   lat = ~Y,
                   icon = eps_icon,
                   popup = ~paste(
                     "<table class='popup-table'>",
                     "<tr><th>Nombre de la EPS</th><td>", NOMEPS, "</td></tr>",
                     "<tr><th>Poblacion Ambito</th><td>", POBAMBEPS, "</td></tr>",
                     "</table>"
                   ),
                   group = "EPS<img src='https://cdn-icons-png.flaticon.com/512/616/616546.png' width='20' height='20'>") %>%
        clearGroup("Distrito<hr><strong>Infraestructura de saneamiento:</strong>") %>%
        showGroup("Provincia") %>%
        hideGroup("Exposicion de Inundacion - GRD<hr><strong>Información secundaria:</strong>") %>% 
        hideGroup("Sectores Operacionales") %>% 
        setView(lng = selected_dep$X,
                lat = selected_dep$Y,
                zoom = 8)
      
      updateSelectInput(session, "distrito", choices = NULL, selected = NULL)
    }
  })
  
  
  
  

  # observe({
  #   selected_dep <- depa[depa$nomdep == input$departamento, ]
  #   selected_dep_id <- depa$iddep[depa$nomdep == input$departamento]
  #   selected_prov_id <- prov$idprov[(prov$nomprov == input$provincia) & (prov$nomdep==input$departamento)]
  #   selected_dist_id <- dist$iddist[(dist$nomdist == input$distrito) & (dist$nomprov == input$provincia) & (dist$nomdep==input$departamento)]
  #   selected_capa_pc_ccpp_general_pc_depa <- capa_pc_ccpp_general_pc[(capa_pc_ccpp_general_pc$NOMDEP==input$departamento),]
  #   selected_capa_pc_ccpp_general_rural_depa <- capa_pc_ccpp_general_rural[(capa_pc_ccpp_general_rural$NOMDEP==input$departamento),]
  #   selected_capa_pc_ccpp_general_urbano_depa <- capa_pc_ccpp_general_urbano[(capa_pc_ccpp_general_urbano$NOMDEP==input$departamento),]
  #   selected_capa_pc_ccpp_general_pc_prov <- selected_capa_pc_ccpp_general_pc_depa[(selected_capa_pc_ccpp_general_pc_depa$NOMDEP==input$departamento)&(selected_capa_pc_ccpp_general_pc_depa$NOMPROV==input$provincia),]
  #   selected_capa_pc_ccpp_general_rural_prov <- selected_capa_pc_ccpp_general_rural_depa[(selected_capa_pc_ccpp_general_rural_depa$NOMDEP==input$departamento)&(selected_capa_pc_ccpp_general_rural_depa$NOMPROV==input$provincia),]
  #   selected_capa_pc_ccpp_general_urbano_prov <- selected_capa_pc_ccpp_general_urbano_depa[(selected_capa_pc_ccpp_general_urbano_depa$NOMDEP==input$departamento)&(selected_capa_pc_ccpp_general_urbano_depa$NOMPROV==input$provincia),]
  #   selected_capa_pc_ccpp_general_pc_dist <- selected_capa_pc_ccpp_general_pc_prov[(selected_capa_pc_ccpp_general_pc_prov$NOMDEP==input$departamento)&(selected_capa_pc_ccpp_general_pc_prov$NOMPROV==input$provincia)&(selected_capa_pc_ccpp_general_pc_prov$NOMDIST==input$distrito),]
  #   selected_capa_pc_ccpp_general_rural_dist <- selected_capa_pc_ccpp_general_rural_prov[(selected_capa_pc_ccpp_general_rural_prov$NOMDEP==input$departamento)&(selected_capa_pc_ccpp_general_rural_prov$NOMPROV==input$provincia)&(selected_capa_pc_ccpp_general_rural_prov$NOMDIST==input$distrito),]
  #   selected_capa_pc_ccpp_general_urbano_dist <- selected_capa_pc_ccpp_general_urbano_prov[(selected_capa_pc_ccpp_general_urbano_prov$NOMDEP==input$departamento)&(selected_capa_pc_ccpp_general_urbano_prov$NOMPROV==input$provincia)&(selected_capa_pc_ccpp_general_urbano_prov$NOMDIST==input$distrito),]
  #   pc_icon <- makeIcon(
  #     iconUrl = "https://cdn-icons-png.flaticon.com/512/565/565665.png",
  #     iconWidth = 20, iconHeight = 20
  #   )
  #   rural_icon <- makeIcon(
  #     iconUrl = "https://cdn-icons-png.flaticon.com/512/4344/4344781.png",
  #     iconWidth = 20, iconHeight = 20
  #   )
  # 
  #   urbano_icon = makeIcon(
  #     iconUrl = "https://cdn-icons-png.flaticon.com/512/4274/4274096.png",
  #     iconWidth = 20, iconHeight = 20
  #   )
  # 
  #   # Filtra los distritos relacionados con la provincia seleccionada
  #   #selected_dist <- dist[dist$idprov == selected_prov_id, ]
  #   selected_capa_pc_ccpp_general_pc_dist <- selected_capa_pc_ccpp_general_pc_dist[selected_capa_pc_ccpp_general_pc_dist$iddep == selected_dep_id & selected_capa_pc_ccpp_general_pc_dist$idprov == selected_prov_id & selected_capa_pc_ccpp_general_pc_dist$iddist == selected_dist_id,  ]
  #   selected_capa_pc_ccpp_general_rural_dist <- selected_capa_pc_ccpp_general_rural_dist[selected_capa_pc_ccpp_general_rural_dist$iddep == selected_dep_id & selected_capa_pc_ccpp_general_rural_dist$idprov == selected_prov_id & selected_capa_pc_ccpp_general_rural_dist$iddist == selected_dist_id,  ]
  #   selected_capa_pc_ccpp_general_urbano_dist <- selected_capa_pc_ccpp_general_urbano_dist[selected_capa_pc_ccpp_general_urbano_dist$iddep == selected_dep_id & selected_capa_pc_ccpp_general_urbano_dist$idprov == selected_prov_id & selected_capa_pc_ccpp_general_urbano_dist$iddist == selected_dist_id,  ]
  #   selected_dist <- dist[dist$iddep == selected_dep_id & dist$idprov == selected_prov_id & dist$iddist == selected_dist_id,  ]
  #   
  #   if (!is.null(input$distrito) && input$distrito != "Seleccionar todos los distritos" && input$provincia != "Seleccionar todas las provincias" && input$departamento != "Seleccionar todos los departamentos") {
  #     leafletProxy("map") %>%
  #       hideGroup("Departamento") %>%
  #       hideGroup("Provincia") %>%
  #       hideGroup("Distrito<hr><strong>Infraestructura de saneamiento:</strong>") %>%
  #       clearGroup("CCPP Pequeña Ciudad <img src='https://cdn-icons-png.flaticon.com/512/565/565665.png' width='20' height='20'>") %>%
  #       clearGroup("CCPP Rural <img src='https://cdn-icons-png.flaticon.com/512/4344/4344781.png' width='20' height='20'>") %>%
  #       clearGroup("CCPP Pequeña Ciudad Tipo II <img src='https://cdn-icons-png.flaticon.com/512/4274/4274096.png' width='20' height='20'>") %>%
  #       clearGroup("Reservorio<img src='https://cdn-icons-png.flaticon.com/512/1843/1843893.png' width='20' height='20'>") %>%
  #       clearGroup("Captacion<img src='https://cdn-icons-png.flaticon.com/512/5371/5371132.png' width='20' height='20'>") %>%
  #       clearGroup("PTAP <img src='https://cdn-icons-png.flaticon.com/512/8846/8846576.png' width='20' height='20'>") %>%
  #       clearGroup("PTAR <img src='https://cdn-icons-png.flaticon.com/512/10708/10708424.png' width='20' height='20'><hr><strong>Servicios de saneamiento:</strong><br>") %>%
  #       hideGroup("Exposicion de Inundacion - GRD<hr><strong>Información secundaria:</strong>") %>%
  #       hideGroup("Sectores Operacionales") %>%
  #       hideGroup("Colegio <img src='https://cdn-icons-png.flaticon.com/512/1080/1080985.png' width='20' height='20'>") %>%
  #       addPolygons(data = selected_dist,
  #                   fillColor = "transparent",
  #                   fillOpacity = 0.5,
  #                   color = "black",
  #                   weight = 2,
  #                   highlightOptions = highlightOptions(color = "white", weight = 4,
  #                                                       bringToFront = TRUE),
  #                   group = "Distrito<hr><strong>Infraestructura de saneamiento:</strong>") %>%
  #       # addLabelOnlyMarkers(data = selected_dist,
  #       #                     lng = ~X, lat = ~Y, label = ~nomdist,
  #       #                     labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE)) %>%
  #       addMarkers(data = selected_capa_pc_ccpp_general_pc_dist,
  #                  lng = ~X,
  #                  lat = ~Y,
  #                  icon = pc_icon,
  #                  popup = ~paste(
  #                    "<table class='popup-table'>",
  #                    "<tr><th>Nombre del centro poblado</th><td>", NOM_CCPP, "</td></tr>",
  #                    "<tr><th>Poblacion</th><td>", POBTOTAL, "</td></tr>",
  #                    "<tr><th>Vivienda</th><td>", VIVTOTAL, "</td></tr>",
  #                    "<tr><th>Continuidad horas</th><td>", conti_avenida_horas, "</td></tr>",
  #                    "<tr><th>Continuidad días</th><td>", conti_avenida_dias, "</td></tr>",
  #                    "<tr><th>Conexiones totales de agua</th><td>", Conex_tot_agua, "</td></tr>",
  #                    "<tr><th>Conexiones totales de alcantarillado</th><td>", Conex_tot_alca, "</td></tr>",
  #                    "</table>"
  #                  ),
  #                  clusterOptions = markerClusterOptions(),
  #                  group = "CCPP Pequeña Ciudad <img src='https://cdn-icons-png.flaticon.com/512/565/565665.png' width='20' height='20'>") %>%
  #       addMarkers(data = selected_capa_pc_ccpp_general_rural_dist,
  #                  lng = ~X,
  #                  lat = ~Y,
  #                  icon = rural_icon,
  #                  popup = ~paste(
  #                    "<table class='popup-table'>",
  #                    "<tr><th>Nombre del centro poblado</th><td>", NOM_CCPP, "</td></tr>",
  #                    "<tr><th>Poblacion</th><td>", POBTOTAL, "</td></tr>",
  #                    "<tr><th>Vivienda</th><td>", VIVTOTAL, "</td></tr>",
  #                    "<tr><th>Continuidad horas</th><td>", conti_avenida_horas, "</td></tr>",
  #                    "<tr><th>Continuidad días</th><td>", conti_avenida_dias, "</td></tr>",
  #                    "<tr><th>Conexiones totales de agua</th><td>", Conex_tot_agua, "</td></tr>",
  #                    "<tr><th>Conexiones totales de alcantarillado</th><td>", Conex_tot_alca, "</td></tr>",
  #                    "</table>"
  #                  ),
  #                  clusterOptions = markerClusterOptions(),
  #                  group = "CCPP Rural <img src='https://cdn-icons-png.flaticon.com/512/4344/4344781.png' width='20' height='20'>") %>%
  #       addMarkers(data = selected_capa_pc_ccpp_general_urbano_dist,
  #                  lng = ~X,
  #                  lat = ~Y,
  #                  icon = urbano_icon,
  #                  popup = ~paste(
  #                    "<table class='popup-table'>",
  #                    "<tr><th>Nombre del centro poblado</th><td>", NOM_CCPP, "</td></tr>",
  #                    "<tr><th>Poblacion</th><td>", POBTOTAL, "</td></tr>",
  #                    "<tr><th>Vivienda</th><td>", VIVTOTAL, "</td></tr>",
  #                    "<tr><th>Continuidad horas</th><td>", conti_avenida_horas, "</td></tr>",
  #                    "<tr><th>Continuidad días</th><td>", conti_avenida_dias, "</td></tr>",
  #                    "<tr><th>Conexiones totales de agua</th><td>", Conex_tot_agua, "</td></tr>",
  #                    "<tr><th>Conexiones totales de alcantarillado</th><td>", Conex_tot_alca, "</td></tr>",
  #                    "</table>"
  #                  ),
  #                  clusterOptions = markerClusterOptions(),
  #                  group = "CCPP Pequeña Ciudad Tipo II <img src='https://cdn-icons-png.flaticon.com/512/4274/4274096.png' width='20' height='20'>") %>%
  #       setView(lng = mean(st_coordinates(selected_capa_pc_ccpp_general_pc_dist)[, 1]),
  #               lat = mean(st_coordinates(selected_capa_pc_ccpp_general_pc_dist)[, 2]),
  #               zoom = 10)
  #     # Actualiza las opciones del tercer input
  #     # updateSelectInput(session, "distrito", choices = c("Seleccionar todos los distritos", sort(selected_dist$nomdist)))
  #   } else {
  #     # Si no se ha seleccionado ninguna provincia, muestra la Capa 2 de provincia y limpia el input de distrito
  #     leafletProxy("map") %>%
  #       addMarkers(data = selected_capa_pc_ccpp_general_pc_prov,
  #                  lng = ~X,
  #                  lat = ~Y,
  #                  icon = pc_icon,
  #                  popup = ~paste(
  #                    "<table class='popup-table'>",
  #                    "<tr><th>Nombre del centro poblado</th><td>", NOM_CCPP, "</td></tr>",
  #                    "<tr><th>Poblacion</th><td>", POBTOTAL, "</td></tr>",
  #                    "<tr><th>Vivienda</th><td>", VIVTOTAL, "</td></tr>",
  #                    "<tr><th>Continuidad horas</th><td>", conti_avenida_horas, "</td></tr>",
  #                    "<tr><th>Continuidad días</th><td>", conti_avenida_dias, "</td></tr>",
  #                    "<tr><th>Conexiones totales de agua</th><td>", Conex_tot_agua, "</td></tr>",
  #                    "<tr><th>Conexiones totales de alcantarillado</th><td>", Conex_tot_alca, "</td></tr>",
  #                    "</table>"
  #                  ),
  #                  clusterOptions = markerClusterOptions(),
  #                  group = "CCPP Pequeña Ciudad <img src='https://cdn-icons-png.flaticon.com/512/565/565665.png' width='20' height='20'>") %>%
  #       addMarkers(data = selected_capa_pc_ccpp_general_rural_prov,
  #                  lng = ~X,
  #                  lat = ~Y,
  #                  icon = rural_icon,
  #                  popup = ~paste(
  #                    "<table class='popup-table'>",
  #                    "<tr><th>Nombre del centro poblado</th><td>", NOM_CCPP, "</td></tr>",
  #                    "<tr><th>Poblacion</th><td>", POBTOTAL, "</td></tr>",
  #                    "<tr><th>Vivienda</th><td>", VIVTOTAL, "</td></tr>",
  #                    "<tr><th>Continuidad horas</th><td>", conti_avenida_horas, "</td></tr>",
  #                    "<tr><th>Continuidad días</th><td>", conti_avenida_dias, "</td></tr>",
  #                    "<tr><th>Conexiones totales de agua</th><td>", Conex_tot_agua, "</td></tr>",
  #                    "<tr><th>Conexiones totales de alcantarillado</th><td>", Conex_tot_alca, "</td></tr>",
  #                    "</table>"
  #                  ),
  #                  clusterOptions = markerClusterOptions(),
  #                  group = "CCPP Rural <img src='https://cdn-icons-png.flaticon.com/512/4344/4344781.png' width='20' height='20'>") %>%
  #       addMarkers(data = selected_capa_pc_ccpp_general_urbano_prov,
  #                  lng = ~X,
  #                  lat = ~Y,
  #                  icon = urbano_icon,
  #                  popup = ~paste(
  #                    "<table class='popup-table'>",
  #                    "<tr><th>Nombre del centro poblado</th><td>", NOM_CCPP, "</td></tr>",
  #                    "<tr><th>Poblacion</th><td>", POBTOTAL, "</td></tr>",
  #                    "<tr><th>Vivienda</th><td>", VIVTOTAL, "</td></tr>",
  #                    "<tr><th>Continuidad horas</th><td>", conti_avenida_horas, "</td></tr>",
  #                    "<tr><th>Continuidad días</th><td>", conti_avenida_dias, "</td></tr>",
  #                    "<tr><th>Conexiones totales de agua</th><td>", Conex_tot_agua, "</td></tr>",
  #                    "<tr><th>Conexiones totales de alcantarillado</th><td>", Conex_tot_alca, "</td></tr>",
  #                    "</table>"
  #                  ),
  #                  clusterOptions = markerClusterOptions(),
  #                  group = "CCPP Pequeña Ciudad Tipo II <img src='https://cdn-icons-png.flaticon.com/512/4274/4274096.png' width='20' height='20'>") %>%
  #       #clearGroup("Distrito<hr><strong>Infraestructura de saneamiento:</strong>") %>%
  #       showGroup("Distrito<hr><strong>Infraestructura de saneamiento:</strong>") %>%
  #       showGroup("Provincia") %>%
  #       hideGroup("Exposicion de Inundacion - GRD<hr><strong>Información secundaria:</strong>") %>%
  #       hideGroup("Sectores Operacionales") %>%
  #       hideGroup("Colegio <img src='https://cdn-icons-png.flaticon.com/512/1080/1080985.png' width='20' height='20'>") %>%
  #       setView(lng = selected_dep$X,
  #               lat = selected_dep$Y,
  #               zoom = 8)
  # 
  #     #updateSelectInput(session, "distrito", choices = NULL, selected = NULL)
  #   }
  # })



  
  
  
  
  observe({
    input$map_click
    click <- input$map_click
    if (!is.null(click)) {
      coords <- sprintf("[%f, %f]", click$lat, click$lng)
      session$sendCustomMessage("copyToClipboard", coords)
      showModal(
        modalDialog(
          title = "Coordenadas Copiadas",
          "Las coordenadas han sido copiadas al portapapeles.",
          easyClose = TRUE
        )
      )
    }
  })
}

shinyApp(ui, server)
