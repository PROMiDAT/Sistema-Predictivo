
library(shiny)
library(shinyAce)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(DT)
library(promises)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(reshape)
library(corrplot)
library(dendextend)
library(scatterplot3d)
library(stringr)
library(shinyjs)


#######################################################################################
######################### Header de la pagina
#######################################################################################

mi.encabezado.dashboard <- dashboardHeader(title = tags$a(href="http://promidat.com", 
                                                      img(src="Logo2.png", 
                                                          height=55, 
                                                          width="100%", 
                                                          style="padding-top:2px; padding-bottom:6px;")))


#######################################################################################
######################### Menu de la pagina
#######################################################################################

mi.menu.Sidebar <- dashboardSidebar(
  sidebarMenu(id = "principal", 
              tags$div(style="padding-top:10px;"),
              menuItem("Cargar Datos", tabName = "cargar", icon = icon("dashboard")),
              menuItem("Resumen Numérico", tabName = "resumen", icon = icon("th")),
              menuItem("Matriz de confusión", tabName = "mc", icon = icon("th")),
              menuItem("KNN", tabName = "knn", icon = icon("th")),
              menuItem("Arboles", tabName = "arboles", icon = icon("th")),
              menuItem("Bosques", tabName = "bosques", icon = icon("th")),
              menuItem("Potenciación", tabName = "potenciacion", icon = icon("th")),
              menuItem("SVM", tabName = "svm", icon = icon("th")),
              menuItem("Generar Reporte", tabName = "reporte", icon = icon("th")),
              hr() ))

#######################################################################################
######################### Documentos importados
#######################################################################################

documents.import <- tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "style_promidat.css"),
  useShinyjs()
)

#######################################################################################
######################### Cuerpo de la pagina Cargar Datos
#######################################################################################

# Panel con las opciones para cargar datos
panel.cargar.datos <- tabPanel(title = "Cargar", width = 10, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                               checkboxInput('header', 'Encabezado (Header)', TRUE),
                               checkboxInput('columname', 'Incluir nombre de filas', TRUE),
                               radioButtons('sep', 'Seperador', c(Coma=',', 'Punto y Coma'=';', Tab='\t'), selected = 'Coma'),
                               radioButtons('dec', 'Separador Decimal', c('Punto'='.', 'Coma'=","), selected = 'Punto'),
                               fileInput('file1', 'Cargar Archivo', accept = c('text/csv','text/comma-separated-values, text/plain', '.csv'), 
                                         buttonLabel = "Subir", placeholder = "",width = "100%"),
                               actionButton("loadButton", "Cargar", width = "100%"),
                               hr(),
                               aceEditor("fieldCodeData", mode = "r", theme = "monokai", value = "", height = "15vh", readOnly = T))


panel.transformar.datos <- tabPanel(title = "Transformar", width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                                    DT::dataTableOutput('transData'),
                                    actionButton("transButton", "Aplicar", width = "100%"),
                                    hr(),
                                    aceEditor("fieldCodeTrans", mode = "r", theme = "monokai", value = "", height = "10vh",  readOnly = T))

pagina.cargar.datos <- tabItem(tabName = "cargar",
                               column(width = 5, tabBox(title = NULL, width = 10, #Panel con el panel de cargar datos y transformar datos
                                                        panel.cargar.datos,
                                                        panel.transformar.datos)),
                               column(width = 7,                                  #Lugar donde se imprimen los datos
                                      box(title = "Datos", status = "primary", width = 12, 
                                          solidHeader = TRUE, collapsible = TRUE,
                                          withSpinner(DT::DTOutput('contents'), type = 7, color = "#CBB051"))) 
                               )

#######################################################################################
######################### Cuerpo de la pagina de Resumen Numerico
#######################################################################################

resumen.numerico <- tabItem(tabName = "resumen",
        column(width = 7,
               box(title = "Resumen Numérico", status = "primary",
                   width = 12, solidHeader = TRUE, collapsible = TRUE, 
                   DT::dataTableOutput("resumen.completo"), hr(),
                   aceEditor("fieldCodeResum", mode = "r", theme = "monokai", value = "", height = "8vh", autoComplete = "enabled")
               )
        ),
        column(width = 5,
               box(title = "Resumen Numérico por Variable", status = "primary", 
                   width = 12, solidHeader = TRUE, collapsible = TRUE,
                   selectInput(inputId = "sel.resumen", label = h4("Seleccionar Variable:"), choices =  ""),
                   fluidRow(uiOutput("resumen"))
               )
        )
)

#######################################################################################
######################### Cuerpo de la pagina
#######################################################################################

mi.cuerpo.dashboard <- dashboardBody( documents.import, 
                                      tabItems( pagina.cargar.datos , #Carga de Datos y Transformar Datos
                                                resumen.numerico, #Resumen Numérico
    
    
    #Dispersión
    tabItem(tabName = "dispersion",
            column(width = 4, 
                   dropdownButton(h4("Opciones"),
                                  selectizeInput("select.var", "Seleccionar variables", 
                                                 multiple = T, choices = c(""), options = list(maxItems = 3)),
                                  circle = F, status = "danger", icon = icon("gear"), width = "100%",
                                  tooltip = tooltipOptions(title = "Clic para ver opciones")
                   )),
            plotOutput('plot.disp', height = "82vh"),
            aceEditor("fieldCodeDisp", mode = "r", theme = "monokai", value = "", height = "8vh", autoComplete = "enabled")
    ),
    
    #Correlaciones
    tabItem(tabName = "correlacion",
            fluidRow(
              column(width = 4, 
                     dropdownButton(
                       h4("Opciones"),
                       selectInput(inputId = "cor.metodo", label = "Seleccionar Método", 
                                   choices =  c("circle", "square", "ellipse", "number", "shade", "color", "pie")),
                       circle = F, status = "danger", icon = icon("gear"), width = "100%",
                       tooltip = tooltipOptions(title = "Clic para ver opciones")
                     )),
              column(width = 8, 
                     dropdownButton(
                       h4("Opciones"),
                       aceEditor("fieldModelCor", mode = "r", theme = "monokai", value = "", height = "4vh", autoComplete = "enabled"),
                       aceEditor("fieldCodeCor", mode = "r", theme = "monokai", value = "", height = "6vh", autoComplete = "enabled"),
                       circle = F, status = "danger", icon = icon("code"), width = "100%",
                       tooltip = tooltipOptions(title = "Clic para ver el código")
                     ))),
            
            #withSpinner(plotOutput('plot.cor', height = "84vh"), type = 7, color = "#CBB051"), 
            plotOutput('plot.cor', height = "84vh")
            #fluidRow(
            #  column(width = 4, 
            #         aceEditor("fieldModelCor", mode = "r", theme = "monokai", value = "", height = "6vh", autoComplete = "enabled")),
            #  column(width = 8, 
            #           aceEditor("fieldCodeCor", mode = "r", theme = "monokai", value = "", height = "6vh", autoComplete = "enabled")))
            
    ),
    
    #PCA
    tabItem(tabName = "acp",
            column(width = 12,
                   aceEditor("fieldCodePCAModelo", mode = "r", theme = "monokai", value = "", height = "3vh", readOnly = T, autoComplete = "enabled"),
                   tabBox(id = "tabPCA", title = dropdownButton(h4("Opciones"),
                                                                sliderInput("ind.cos", "Coseno de los Individuos: ", min = 0, max = 100, value = 0),
                                                                sliderInput("var.cos", "Coseno de las Variables: ", min = 0, max = 100, value = 0),
                                                                circle = F, status = "danger", icon = icon("gear"), width = "100%", right = T,
                                                                tooltip = tooltipOptions(title = "Clic para ver opciones")), width = NULL,
                          tabPanel(title = 'Individuos', value = "individuos", plotOutput('plot.ind', height = "76vh"),
                                   aceEditor("fieldCodeInd", mode = "r", theme = "monokai", value = "", height = "3vh", autoComplete = "enabled")),
                          tabPanel(title = 'Variables', value = "variables", plotOutput('plot.var', height = "76vh"),
                                   aceEditor("fieldCodeVar", mode = "r", theme = "monokai", value = "", height = "3vh", autoComplete = "enabled")),
                          tabPanel(title = 'Sobreposición', value = "sobreposicion", plotOutput('plot.biplot', height = "76vh"),
                                   aceEditor("fieldCodeBi", mode = "r", theme = "monokai", value = "", height = "3vh", autoComplete = "enabled"))
                   )
            )
    ),
    
    #Distribuciones
    tabItem(tabName = "distribucion",
            column(width = 12,
                   tabBox(id = "tabDyA", 
                          title = dropdownButton(h4("Opciones"),
                                                 conditionalPanel(
                                                   condition = "input.tabDyA == 'numericas'",
                                                   selectInput(inputId = "sel.distribucion.num", label = "Seleccionar Variable", choices =  "", selectize = T)
                                                 ),
                                                 conditionalPanel(
                                                   condition = "input.tabDyA == 'categoricas'",
                                                   selectInput(inputId = "sel.distribucion.cat", label = "Seleccionar Variable", choices =  "", selectize = T)
                                                 ), circle = F, status = "danger", icon = icon("gear"), width = "100%", right = T,
                                                 tooltip = tooltipOptions(title = "Clic para ver opciones")),
                          width = 12,
                          tabPanel(title = 'Numéricas', value = "numericas", 
                                   plotOutput('plot.num', height = "71vh"),
                                   fluidRow(
                                     column(width = 6,
                                            aceEditor("fieldCodeNum", mode = "r", theme = "monokai", value = "", height = "11vh", autoComplete = "enabled")),
                                     column(width = 6, 
                                            aceEditor("fieldFuncNum", mode = "r", theme = "monokai", value = "", height = "11vh", autoComplete = "enabled")))
                          ),
                          tabPanel(title = 'Categoricas', value = "categoricas", 
                                   plotOutput('plot.cat', height = "71vh"),
                                   fluidRow(
                                     column(width = 6,
                                            aceEditor("fieldCodeCat", mode = "r", theme = "monokai", value = "", height = "11vh", autoComplete = "enabled")),
                                     column(width = 6, 
                                            aceEditor("fieldFuncCat", mode = "r", theme = "monokai", value = "", height = "11vh", autoComplete = "enabled")))
                          )
                   )
            )
    ),
    
    #Agrupaciones
    tabItem(tabName = "agrupacion",
            column(width = 12,
                   #aceEditor("fieldCodeModelo", mode = "r", theme = "monokai", value = "", height = "3vh", readOnly = T, autoComplete = "enabled"),
                   tabBox(id = "tabjerar", title = dropdownButton(h4("Opciones"),
                                                                  selectInput(inputId = "cant.cluster", label = "Cantidad de Clusters:", choices =  c(2:10)),
                                                                  conditionalPanel(
                                                                    condition = "input.tabjerar == 'Horizontal'",
                                                                    selectInput(inputId = "sel.cluster", label = "Seleccionar Cluster:", choices =  "")
                                                                  ),
                                                                  conditionalPanel(
                                                                    condition = "input.tabjerar == 'Vertical'",
                                                                    selectInput(inputId = "sel.verticales", label = "Seleccionar Variable:", choices =  "")),
                                                                  conditionalPanel(
                                                                    condition = "input.tabjerar == 'Barras'",
                                                                    selectInput(inputId = "sel.cat.var", label = "Seleccionar Variable:", choices =  "")
                                                                  ), circle = F, status = "danger", icon = icon("gear"), width = "100%", right = T,
                                                                  tooltip = tooltipOptions(title = "Clic para ver opciones")), width = 12,
                          tabPanel(title = 'Diagrama', plotOutput('plot.diag', height = "71vh"), 
                                   aceEditor("fieldCodeDiag", mode = "r", theme = "monokai", value = "", height = "11vh", autoComplete = "enabled")),
                          tabPanel(title = 'Mapa', plotOutput('plot.mapa', height = "71vh"),
                                   aceEditor("fieldCodeMapa", mode = "r", theme = "monokai", value = "", height = "11vh", autoComplete = "enabled")),
                          tabPanel(title = 'Horizontal', 
                                   plotOutput('plot.horiz', height = "71vh"),
                                   fluidRow(column(width = 4,
                                                   aceEditor("fieldCodeHoriz", mode = "r", theme = "monokai", value = "", height = "11vh", autoComplete = "enabled")),
                                            column(width = 4,
                                                   aceEditor("fieldFuncHoriz", mode = "r", theme = "monokai", value = "", height = "11vh", autoComplete = "enabled")),
                                            column(width = 4,
                                                   aceEditor("fieldCodeCentr", mode = "r", theme = "monokai", value = "", height = "11vh", autoComplete = "enabled")))),
                          tabPanel(title = 'Vertical', 
                                   plotOutput('plot.vert', height = "71vh"),
                                   fluidRow(column(width = 4,
                                                   aceEditor("fieldCodeVert", mode = "r", theme = "monokai", value = "", height = "11vh", autoComplete = "enabled")),
                                            column(width = 4,
                                                   aceEditor("fieldFuncVert", mode = "r", theme = "monokai", value = "", height = "11vh", autoComplete = "enabled")),
                                            column(width = 4,
                                                   aceEditor("fieldCodeCentr2", mode = "r", theme = "monokai", value = "", height = "11vh", autoComplete = "enabled")))),
                          tabPanel(title = 'Radar', plotOutput('plot.radar', height = "71vh"),
                                   fluidRow(column(width = 4,
                                                   aceEditor("fieldCodeRadar", mode = "r", theme = "monokai", value = "", height = "11vh", autoComplete = "enabled")),
                                            column(width = 4,
                                                   aceEditor("fieldFuncRadar", mode = "r", theme = "monokai", value = "", height = "11vh", autoComplete = "enabled")),
                                            column(width = 4,
                                                   aceEditor("fieldCodeCentr3", mode = "r", theme = "monokai", value = "", height = "11vh", autoComplete = "enabled")))
                          ),
                          tabPanel(title = 'Barras', plotOutput('plot.bar.cat', height = "71vh"),
                                   fluidRow(column(width = 12,
                                                   aceEditor("fieldCodeBarras", mode = "r", theme = "monokai", value = "", height = "6vh", autoComplete = "enabled")))
                          )
                   )
            )
    ),
    
    tabItem(tabName = "kmedias",
            column(width = 12,
                   aceEditor("fieldCodeKModelo", mode = "r", theme = "monokai", value = "", height = "3vh", readOnly = T, autoComplete = "enabled"),
                   tabBox(id = "tabkmedias", title = dropdownButton(h4("Opciones"),
                                                                    selectInput(inputId = "cant.kmeans.cluster", label = "Cantidad de Clusters:", choices =  c(2:10)),
                                                                    conditionalPanel(
                                                                      condition = "input.tabkmedias== 'Horizontal'",
                                                                      selectInput(inputId = "sel.kmeans.cluster", label = "Seleccionar Cluster:", choices =  "")
                                                                    ),
                                                                    conditionalPanel(
                                                                      condition = "input.tabkmedias == 'Vertical'",
                                                                      selectInput(inputId = "sel.kmeans.verticales", label = "Seleccionar Variable:", choices =  "")
                                                                    ),
                                                                    conditionalPanel(
                                                                      condition = "input.tabkmedias == 'Barras'",
                                                                      selectInput(inputId = "sel.kcat.var", label = "Seleccionar Variable:", choices =  "")
                                                                    ),circle = F, status = "danger", icon = icon("gear"), width = "100%", right = T,
                                                                    tooltip = tooltipOptions(title = "Clic para ver opciones")), width = 12,
                          tabPanel(title = 'Inercia', fluidRow(uiOutput('resumen.kmedias'))),
                          tabPanel(title = 'Codo Jambu', plotOutput('plot.jambu', height = "70vh"), 
                                   fluidRow(column(width = 6, 
                                                   aceEditor("fieldCodeJambu", mode = "r", theme = "monokai", value = "", height = "7vh", autoComplete = "enabled")),
                                            column(width = 6,
                                                   aceEditor("fieldFuncJambu", mode = "r", theme = "monokai", value = "", height = "7vh", autoComplete = "enabled")))
                          ),
                          tabPanel(title = 'Mapa', plotOutput('plot.kmapa', height = "71vh"),
                                   aceEditor("fieldCodeKmapa", mode = "r", theme = "monokai", value = "", height = "6vh", autoComplete = "enabled")),
                          tabPanel(title = 'Horizontal', plotOutput('plot.khoriz', height = "71vh"),
                                   fluidRow(column(width = 6, 
                                                   aceEditor("fieldCodeKhoriz", mode = "r", theme = "monokai", value = "", height = "6vh", autoComplete = "enabled")),
                                            column(width = 6,
                                                   aceEditor("fieldFuncKhoriz", mode = "r", theme = "monokai", value = "", height = "6vh", autoComplete = "enabled")))
                          ),
                          tabPanel(title = 'Vertical', plotOutput('plot.kvert', height = "71vh"),
                                   fluidRow(column(width = 6, 
                                                   aceEditor("fieldCodeKvert", mode = "r", theme = "monokai", value = "", height = "6vh", autoComplete = "enabled")),
                                            column(width = 6,
                                                   aceEditor("fieldFuncKvert", mode = "r", theme = "monokai", value = "", height = "6vh", autoComplete = "enabled")))
                          ),
                          tabPanel(title = 'Radar', plotOutput('plot.kradar', height = "71vh"),
                                   fluidRow(column(width = 6, 
                                                   aceEditor("fieldCodeKradar", mode = "r", theme = "monokai", value = "", height = "6vh", autoComplete = "enabled")),
                                            column(width = 6,
                                                   aceEditor("fieldFuncKradar", mode = "r", theme = "monokai", value = "", height = "6vh", autoComplete = "enabled")))
                          ),
                          tabPanel(title = 'Barras', plotOutput('plot.kcat', height = "71vh"),
                                   fluidRow(column(width = 12,
                                                   aceEditor("fieldCodeKbarras", mode = "r", theme = "monokai", value = "", height = "6vh", autoComplete = "enabled")))
                          )
                   )
            )
    )
    
  ) #tabItems
) #dashboardBody

# Define UI for application that draws a histogram
shinyUI(dashboardPage( 
  mi.encabezado.dashboard ,
  mi.menu.Sidebar,
  mi.cuerpo.dashboard
)) #UI
