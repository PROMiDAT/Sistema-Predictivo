
library(shiny)
library(shinyAce)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(colourpicker)
library(shinyjs)
library(knitr)
library(DT)
library(future)
library(promises)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(reshape)
library(corrplot)
library(dendextend)
library(scatterplot3d)
library(stringr)
library(caret)
library(kknn)
library(flexdashboard)


###########################################################################################################################
#####  MENU
###########################################################################################################################

menu.cargar <- menuItem("Datos", tabName = "cargar", icon = icon("dashboard"))

menu.estadisticas <- menuItem("Estadísticas Básicas", tabName = "parte1", icon = icon("th"),
                              menuSubItem("Resumen Numérico", tabName = "resumen", icon = icon("th")),
                              menuSubItem("Test de Normalidad", tabName = "normalidad", icon = icon("th")),
                              menuSubItem("Dispersión", tabName = "dispersion", icon = icon("th")),
                              menuSubItem("Distribuciones", tabName = "distribucion", icon = icon("th")),
                              menuSubItem("Correlación", tabName = "correlacion", icon = icon("th")))

menu.aprendizaje.supervisado <- menuItem("Aprendizaje Supervisado", tabName = "parte2", icon = icon("th"),
                                         menuSubItem("K Vecinos Más Cercanos",tabName = "knn",icon = icon("bar-chart-o")),
                                         menuSubItem("Método de Bayes",tabName = "bayes",icon = icon("bar-chart-o")),
                                         menuSubItem("Soporte Vectorial",tabName = "svm",icon = icon("bar-chart-o")),
                                         menuSubItem("Árboles de Decisión",tabName = "dt",icon = icon("bar-chart-o")),
                                         menuSubItem("Bosques Aleatorios",tabName = "rf",icon = icon("bar-chart-o")),
                                         menuSubItem("ADA - Boosting",tabName = "boosting",icon = icon("bar-chart-o")),
                                         menuSubItem("Extreme Gradient Boosting",tabName = "xgboosting",icon = icon("bar-chart-o")),
                                         menuSubItem("Redes Neuronales",tabName = "nnw",icon = icon("bar-chart-o")) )

menu.reporte <- menuItem("Generar Reporte", tabName = "reporte", icon = icon("save-file",lib = "glyphicon"))

mi.menu <- sidebarMenu(id = "principal",
              tags$div(style="padding-top:10px;"),
              menu.cargar,
              menu.estadisticas,
              menu.aprendizaje.supervisado,
              menu.reporte)

###########################################################################################################################
#####  HEAD HTML
###########################################################################################################################

mi.head <- tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "style_promidat.css"),
  tags$link(rel="shortcut icon", href="http://www.promidat.org/theme/image.php/formal_white/theme/1438713216/favicon"),
  useShinyjs()
)

###########################################################################################################################
##### PAGINA DE CARGA Y  TRANSFORMACION DE DATOS
###########################################################################################################################

panel.cargar.datos <- tabPanel(title = "Cargar", width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                               checkboxInput('header', 'Encabezado (Header)', TRUE),
                               checkboxInput('rowname', 'Incluir nombre de filas', TRUE),
                               radioButtons('sep', 'Seperador', c(Coma=',', 'Punto y Coma'=';', Tab='\t'), selected = 'Coma'),
                               radioButtons('dec', 'Separador Decimal', c('Punto'='.', 'Coma'=","), selected = 'Punto'),
                               fileInput('file1', label = 'Cargar Archivo', placeholder = "", buttonLabel = "Subir", width = "100%",
                                         accept = c('text/csv', 'text/comma-separated-values, text/plain', '.csv')),
                               actionButton("loadButton", "Cargar", width = "100%"),
                               hr(),
                               aceEditor("fieldCodeData", mode = "r", theme = "monokai", value = "", height = "15vh", readOnly = T))

panel.tansformar.datos <- tabPanel(title = "Transformar", width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                                   DT::dataTableOutput('transData'),
                                   hr(),
                                   actionButton("transButton", "Aplicar", width = "100%"),
                                   hr(),
                                   aceEditor("fieldCodeTrans", mode = "r", theme = "monokai", value = "", height = "10vh",  readOnly = T))

panel.segmentar.datos <- tabPanel(title = "Prueba y Aprendizaje", width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                                  selectInput(inputId = "sel.predic.var", label = h4("Seleccionar Variable a Predecir:"), choices =  "", width = "100%"),
                                  hr(),
                                  sliderInput("segmentacionDatosA", "Proporción Aprendizaje:",width = "100%",
                                              min = 5, max = 95, value = 70, step = 5),
                                  sliderInput("segmentacionDatosT", "Proporción Prueba:", width = "100%",
                                              min = 5, max = 95, value = 30, step = 5),
                                  actionButton("segmentButton", "Segmentar", width = "100%"),
                                  hr(),
                                  aceEditor("fieldCodeSegment", mode = "r", theme = "monokai", value = "", height = "10vh",  readOnly = T))

muestra.datos <- box(title = "Datos", status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                     withSpinner(DT::DTOutput('contents'), type = 7, color = "#CBB051"))

muestra.datos.aprend <- box(title = "Datos de Aprendizaje", status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                            withSpinner(DT::DTOutput('contentsAprend'), type = 7, color = "#CBB051"))

muestra.datos.prueba <- box(title = "Datos de Prueba", status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                            withSpinner(DT::DTOutput('contentsPrueba'), type = 7, color = "#CBB051"))

pagina.cargar.datos <- tabItem(tabName = "cargar",
                               fluidRow(column(width = 5, tabBox(id ="tabs", title = NULL, width = 12, panel.cargar.datos, panel.tansformar.datos, panel.segmentar.datos)),
                               column(width = 7, muestra.datos)),
                               conditionalPanel(
                                 condition = "input.tabs == 'Prueba y Aprendizaje'",
                                 fluidRow(column(width = 6, muestra.datos.aprend ),
                                          column(width = 6, muestra.datos.prueba ))) )

###########################################################################################################################
##### PAGINA DE RESUMEN NUMERICO
###########################################################################################################################

cuadro.resumen.completo <- box(title = "Resumen Numérico", status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                               DT::dataTableOutput("resumen.completo"), hr(),
                               aceEditor("fieldCodeResum", mode = "r", theme = "monokai", value = "", height = "8vh", autoComplete = "enabled"))

cuadro.resumen.variable <- box(title = "Resumen Numérico por Variable", status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                               selectInput(inputId = "sel.resumen", label = h4("Seleccionar Variable:"), choices =  ""),
                               fluidRow(uiOutput("resumen")))

pagina.resumen.numerico <- tabItem(tabName = "resumen",
                                   column(width = 7, cuadro.resumen.completo ),
                                   column(width = 5, cuadro.resumen.variable ))


###########################################################################################################################
##### PAGINA DEL TEST DE NORMALIDAD
###########################################################################################################################

boton.colores <- dropdownButton(h4("Opciones"),
                                      colourpicker::colourInput("col.normal", "Seleccionar Color:",
                                                                value = "#00FF22AA", allowTransparent = T),
                                      circle = F, status = "danger", icon = icon("gear"), width = "100%",
                                      tooltip = tooltipOptions(title = "Clic para ver opciones"), right = T)

opciones.normalidad <- fluidRow(column(width = 9, selectInput(inputId = "sel.normal", label = NULL, choices =  "")),
                                column(width = 3, boton.colores))

panel.grafico.normalidad <- tabPanel(title = "Test de Normalidad", value = "tabNormal",
                                     plotOutput('plot.normal', height = "72vh"))

codigo.normalidad <- aceEditor("fieldCodeNormal", mode = "r", theme = "monokai", value = "",
                               height = "9vh", autoComplete = "enabled")

pagina.test.normalidad <- tabItem(tabName = "normalidad",
                                  column(width = 12,
                                         tabBox(id = "BoxNormal", width = NULL,
                                                title = opciones.normalidad,
                                                panel.grafico.normalidad)),
                                  codigo.normalidad)

###########################################################################################################################
##### PAGINA DE DISPERSION
###########################################################################################################################

# opciones.dispersion <- dropdownButton(h4("Opciones"), circle = F, status = "danger", icon = icon("gear"), width = "100%",
#                                       tooltip = tooltipOptions(title = "Clic para ver opciones"))

codigo.dispersion <- aceEditor("fieldCodeDisp", mode = "r", theme = "monokai", value = "", height = "8vh", autoComplete = "enabled")

opciones.dispersion <- fluidRow(column(width = 9, tags$div(class="select-var-ind",
                                                            selectizeInput("select.var", NULL, multiple = T, choices = c(""),
                                                                           options = list(maxItems = 3, placeholder = "Seleccione la(s) variable(s)")))),
                                 column(width = 3,dropdownButton(h4("Opciones"),
                                                                 colourpicker::colourInput("col.disp", "Seleccionar Color:",
                                                                                           value = "#FF0000AA", allowTransparent = T),
                                                                 circle = F, status = "danger", icon = icon("gear"), width = "100%",
                                                                 tooltip = tooltipOptions(title = "Clic para ver opciones"), right = T)))


grafico.dispersion <- tabPanel(title = "Dispersión", value = "tabDisp", plotOutput('plot.disp', height = "72vh"))

pagina.dispersion<- tabItem(tabName = "dispersion",
                            column(width = 12, tabBox(id = "BoxDisp", width = NULL,
                                                      title = opciones.dispersion,
                                                      grafico.dispersion)),
                            codigo.dispersion )

###########################################################################################################################
##### PAGINA DE CORRELACIONES
###########################################################################################################################

opciones.correlaciones <- dropdownButton(h4("Opciones"),
                                         selectInput(inputId = "cor.metodo", label = "Seleccionar Método",
                                                     choices =  c("circle", "square", "ellipse", "number", "shade", "color", "pie")),
                                         selectInput(inputId = "cor.tipo", label = "Seleccionar Tipo", choices =  c("lower", "upper", "full")),
                                         circle = F, status = "danger", icon = icon("gear"), width = "300px", right = T,
                                         tooltip = tooltipOptions(title = "Clic para ver opciones"))

tab.correlacion <- tabPanel(title = 'Correlación', value = "correlacion",
                            plotOutput('plot.cor', height = "76vh"),
                            fluidRow(column(width = 4, aceEditor("fieldModelCor", mode = "r", theme = "monokai", value = "",
                                                                 height = "6vh", autoComplete = "enabled")),
                                     column(width = 8, aceEditor("fieldCodeCor", mode = "r", theme = "monokai", value = "",
                                                                 height = "6vh", autoComplete = "enabled"))))

tab.codigo.correlaciones <- tabPanel(title = 'Salida Código', value = "cor.salida", verbatimTextOutput("txtcor"))

pagina.correlaciones <- tabItem(tabName = "correlacion",
                                column(width = 12, tabBox(id = "tabCor", width = NULL,
                                                          title = opciones.correlaciones,
                                                          tab.correlacion,
                                                          tab.codigo.correlaciones)))

###########################################################################################################################
##### PAGINA DE DISTRIBUCIONES
###########################################################################################################################

boton.codigo.distribuciones <- dropdownButton(h4("Código"),
                                              h5("Grafico de la Distribución (Numéricas)"),
                                              aceEditor("fieldFuncNum", mode = "r", theme = "monokai", value = "", height = "20vh", autoComplete = "enabled"),
                                              h5("Grafico de la Distribución (Categóricas)"),
                                              aceEditor("fieldFuncCat", mode = "r", theme = "monokai", value = "", height = "20vh", autoComplete = "enabled"),
                                              circle = F, status = "danger", icon = icon("code"), width = "400px", right = T,
                                              tooltip = tooltipOptions(title = "Clic para ver el código"))

boton.opciones.distribuciones <- dropdownButton(h4("Opciones"), colourpicker::colourInput("col.dist", "Seleccionar Color:", value = "#0D00FFAA", allowTransparent = T),
                                                circle = F, status = "danger", icon = icon("gear"), width = "100%", right = T,
                                                tooltip = tooltipOptions(title = "Clic para ver opciones"))

selector.variables.distribucion <- column(width = 7,tags$div(class = "select-var-ind",
                                                             conditionalPanel( condition = "input.tabDyA == 'numericas'",
                                                                               selectInput(inputId = "sel.distribucion.num", label = NULL, choices =  "")),
                                                             conditionalPanel( condition = "input.tabDyA == 'categoricas'",
                                                                               selectInput(inputId = "sel.distribucion.cat", label = NULL, choices =  "") )))

resultados.distribucion.numericas <- tabPanel(title = 'Numéricas', value = "numericas",
                                              plotOutput('plot.num', height = "65vh"),
                                              fluidRow(column(width = 6, aceEditor("fieldCodeNum", mode = "r", theme = "monokai",
                                                                                   value = "", height = "15vh", autoComplete = "enabled")),
                                                       column(width = 6, DT::dataTableOutput("mostrar.atipicos"))) )

resultados.distribucion.categoricas <- tabPanel(title = 'Categóricas', value = "categoricas", plotOutput('plot.cat', height = "76vh"),
                                                aceEditor("fieldCodeCat", mode = "r", theme = "monokai", value = "", height = "6vh", autoComplete = "enabled"))

pagina.distribuciones <- tabItem(tabName = "distribucion",
                                 column(width = 12,
                                        tabBox(id = "tabDyA",
                                               title = fluidRow(
                                                 selector.variables.distribucion,
                                                 column(width = 2, boton.codigo.distribuciones),
                                                 column(width = 2, boton.opciones.distribuciones)), width = 12,
                                               resultados.distribucion.numericas,
                                               resultados.distribucion.categoricas )) )


###########################################################################################################################
##### PAGINA DE KNN
###########################################################################################################################

panel.generar.knn <- tabPanel(title = "Generación del Modelo",
                             verbatimTextOutput("txtknn"),
                             aceEditor("fieldCodeKnn", mode = "r", theme = "monokai",
                                       value = "", height = "5vh", readOnly = F, autoComplete = "enabled"))

panel.prediccion.knn <- tabPanel(title = "Predicción del Modelo",
                                 DT::dataTableOutput("knnPrediTable"),
                                 aceEditor("fieldCodeKnnPred", mode = "r", theme = "monokai",
                                           value = "", height = "5vh", readOnly = F, autoComplete = "enabled"))

panel.matriz.confucion.knn <- tabPanel(title = "Matriz de Confusión",
                                       plotOutput('plot.knn.mc', height = "40vh"),
                                       fluidRow(column(width = 6, gaugeOutput("knnPrecGlob", width = "100%")),
                                                column(width = 6, gaugeOutput("knnErrorGlob", width = "100%"))),
                                       verbatimTextOutput("txtknnMC"),
                                       aceEditor("fieldCodeKnnMC", mode = "r", theme = "monokai",
                                                 value = "", height = "4vh", readOnly = F, autoComplete = "enabled"))

panel.indices.generales <- tabPanel(title = "Índices Generales",
                                    verbatimTextOutput("txtknnIG"),
                                    aceEditor("fieldCodeKnnIG", mode = "r", theme = "monokai",
                                              value = "", height = "10vh", readOnly = T, autoComplete = "enabled"))

selector.variables.knn <- column(width = 10,tags$div(class="select-var-ind",
                                                    selectizeInput("select.var.knn", NULL, multiple = T, choices = c(""),
                                                                   options = list(maxItems = Inf, placeholder = "Seleccione la(s) variable(s) predictoras"))))

opciones.knn <- dropdownButton(h4("Opciones"),circle = F, status = "danger", icon = icon("gear"), width = "300px", right = T,
                               tooltip = tooltipOptions(title = "Clic para ver opciones"),
                               switchInput(inputId = "switch.scale.knn", onStatus = "success", offStatus = "danger", value = T,
                                           label = "Centrar datos", onLabel = "SI", offLabel = "NO", labelWidth = "100%"),
                               sliderInput("kmax.knn", "K Máximo: ", min = 1, max = 100, value = 7),
                               selectInput(inputId = "kernel.knn", label = "Seleccionar un Kernel",selected = 1,
                                           choices =  c("optimal", "rectangular", "triangular", "epanechnikov", "biweight", "triweight", "cos","inv","gaussian","optimal")),
                               selectizeInput("select.var.knn",NULL,label = "Variables Predictoras:", multiple = T, choices = c(""),
                                              options = list(maxItems = Inf, placeholder = "Seleccione la(s) variable(s) predictoras")))


titulo.knn <- fluidRow(column(width = 4,opciones.knn),
                       column(width = 8,actionButton("runKnn", "Ejecutar", width = "100%" )))

pagina.knn <- tabItem(tabName = "knn",
                      column(width = 12,
                             tabBox(width = 12, title = titulo.knn,
                                    panel.generar.knn,
                                    panel.prediccion.knn,
                                    panel.matriz.confucion.knn,
                                    panel.indices.generales)))


###########################################################################################################################
##### PAGINA DE REPORTE
###########################################################################################################################


panel.reporte.codigo <- column(width = 5,
                               tabBox(width = 12, id = "tabReporte",
                                      tabPanel(title = "Reporte", width = 12),
                                      tabPanel(title = "Código", width = 12, aceEditor("fieldCodeReport", mode="markdown", value=''))),
                               downloadButton("descargar", "Descargar", style = "position: relative; left: 40%") )

vista.previa.reporte <- column(width = 7,
                               box(title = "Vista Previa", width = 12, height = "90vh", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                   div(style = 'overflow-x: scroll; overflow-y: scroll; height: 80vh;', htmlOutput("knitDoc"))) )

pagina.generar.reporte <- tabItem(tabName = "reporte", panel.reporte.codigo , vista.previa.reporte )

###########################################################################################################################
##### PAGINA COMPLETA
###########################################################################################################################

shinyUI(dashboardPage(title="PROMiDAT",
                      dashboardHeader(title = tags$a(href="http://promidat.com",
                                                     img(src="Logo2.png", height=55, width="100%", style="padding-top:2px; padding-bottom:6px;"))),
                      dashboardSidebar(mi.menu),
                      dashboardBody(mi.head, tabItems( pagina.cargar.datos,
                                                       pagina.resumen.numerico,
                                                       pagina.test.normalidad,
                                                       pagina.dispersion,
                                                       pagina.correlaciones,
                                                       pagina.distribuciones,
                                                       pagina.generar.reporte,
                                                       pagina.knn))) )
