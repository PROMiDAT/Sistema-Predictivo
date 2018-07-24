
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
library(modeest)
library(caret)
library(kknn)
library(flexdashboard)
library(e1071)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ada)
library(xgboost)
library(nnet)

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
                               switchInput(inputId = "deleteNA", onStatus = "success", offStatus = "danger", value = T, width = "100%",
                                           label = "Eliminar NA", onLabel = "SI", offLabel = "NO", labelWidth = "100%"),
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
                                  fluidRow(column(width = 10, numericInput("semilla", "Semilla Aleatoria:", 5550, width = "100%")), br(),
                                           column(width = 2, switchInput(inputId = "permitir.semilla", onStatus = "success", offStatus = "danger", value = F, width = "100%",
                                                                         label = "", onLabel = "", offLabel = "", labelWidth = "100%",inline = T,size = "large"))),
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
                                       value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

panel.prediccion.knn <- tabPanel(title = "Predicción del Modelo",
                                 DT::dataTableOutput("knnPrediTable"),
                                 hr(),
                                 aceEditor("fieldCodeKnnPred", mode = "r", theme = "monokai",
                                           value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

panel.matriz.confucion.knn <- tabPanel(title = "Matriz de Confusión",
                                       plotOutput('plot.knn.mc', height = "45vh"),
                                       verbatimTextOutput("txtknnMC"),
                                       aceEditor("fieldCodeKnnMC", mode = "r", theme = "monokai",
                                                 value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

panel.indices.generales.knn <- tabPanel(title = "Índices Generales",
                                    fluidRow(column(width = 6, gaugeOutput("knnPrecGlob", width = "100%")),
                                             column(width = 6, gaugeOutput("knnErrorGlob", width = "100%"))),
                                    fluidRow(column(width = 2, gaugeOutput("knnPrecP", width = "100%")),
                                             column(width = 2, gaugeOutput("knnPrecN", width = "100%")),
                                             column(width = 2, gaugeOutput("knnFalP", width = "100%")),
                                             column(width = 2, gaugeOutput("knnFalN", width = "100%")),
                                             column(width = 2, gaugeOutput("knnAserP", width = "100%")),
                                             column(width = 2, gaugeOutput("knnAserN", width = "100%"))),
                                    aceEditor("fieldCodeKnnIG", mode = "r", theme = "monokai",
                                              value = "", height = "35vh", readOnly = F, autoComplete = "enabled"))

opciones.knn <- dropdownButton(h4("Opciones"),circle = F, status = "danger", icon = icon("gear"), width = "300px", right = T,
                               tooltip = tooltipOptions(title = "Clic para ver opciones"),
                               switchInput(inputId = "switch.scale.knn", onStatus = "success", offStatus = "danger", value = T,
                                           label = "Escalar datos", onLabel = "SI", offLabel = "NO", labelWidth = "100%"),
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
                                    panel.indices.generales.knn)))

###########################################################################################################################
##### PAGINA DE BAYES
###########################################################################################################################

panel.generar.bayes <- tabPanel(title = "Generación del Modelo",
                              verbatimTextOutput("txtbayes"),
                              aceEditor("fieldCodeBayes", mode = "r", theme = "monokai",
                                        value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

panel.prediccion.bayes <- tabPanel(title = "Predicción del Modelo",
                                 DT::dataTableOutput("bayesPrediTable"),
                                 hr(),
                                 aceEditor("fieldCodeBayesPred", mode = "r", theme = "monokai",
                                           value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

panel.matriz.confucion.bayes <- tabPanel(title = "Matriz de Confusión",
                                       plotOutput('plot.bayes.mc', height = "45vh"),
                                       verbatimTextOutput("txtBayesMC"),
                                       aceEditor("fieldCodeBayesMC", mode = "r", theme = "monokai",
                                                 value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

panel.indices.generales.bayes <- tabPanel(title = "Índices Generales",
                                    fluidRow(column(width = 6, gaugeOutput("bayesPrecGlob", width = "100%")),
                                             column(width = 6, gaugeOutput("bayesErrorGlob", width = "100%"))),
                                    fluidRow(column(width = 2, gaugeOutput("bayesPrecP", width = "100%")),
                                             column(width = 2, gaugeOutput("bayesPrecN", width = "100%")),
                                             column(width = 2, gaugeOutput("bayesFalP", width = "100%")),
                                             column(width = 2, gaugeOutput("bayesFalN", width = "100%")),
                                             column(width = 2, gaugeOutput("bayesAserP", width = "100%")),
                                             column(width = 2, gaugeOutput("bayesAserN", width = "100%"))),
                                    aceEditor("fieldCodeBayesIG", mode = "r", theme = "monokai",
                                              value = "", height = "35vh", readOnly = F, autoComplete = "enabled"))

opciones.bayes <- dropdownButton(h4("Opciones"),circle = F, status = "danger", icon = icon("gear"), width = "300px", right = T,
                               tooltip = tooltipOptions(title = "Clic para ver opciones"),
                               selectizeInput("select.var.bayes",NULL,label = "Variables Predictoras:", multiple = T, choices = c(""),
                                              options = list(maxItems = Inf, placeholder = "Seleccione la(s) variable(s) predictoras")))

titulo.bayes <- fluidRow(column(width = 4,opciones.bayes),
                       column(width = 8, actionButton("runBayes", "Ejecutar", width = "100%" )))

pagina.bayes <- tabItem(tabName = "bayes",
                      column(width = 12,
                             tabBox(width = 12, title = titulo.bayes,
                                    panel.generar.bayes,
                                    panel.prediccion.bayes,
                                    panel.matriz.confucion.bayes,
                                    panel.indices.generales.bayes)))

###########################################################################################################################
##### PAGINA DE SVM
###########################################################################################################################

panel.generar.svm <- tabPanel(title = "Generación del Modelo",
                              verbatimTextOutput("txtSvm"),
                              aceEditor("fieldCodeSvm", mode = "r", theme = "monokai",
                                        value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

plot.svm <- tabPanel(title = "Gráfico Clasificación",
                     plotOutput('plot.svm', height = "55vh"),
                     hr(),
                     selectizeInput("select.var.svm.plot",NULL,label = "Variables Predictoras:", multiple = T, choices = c(""),
                                    options = list(maxItems = 2, placeholder = "Seleccione la(s) variable(s) predictoras"), width = "100%"),
                     aceEditor("fieldCodeSvmPlot", mode = "r", theme = "monokai",
                               value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

panel.prediccion.svm <- tabPanel(title = "Predicción del Modelo",
                                 DT::dataTableOutput("svmPrediTable"),
                                 hr(),
                                 aceEditor("fieldCodeSvmPred", mode = "r", theme = "monokai",
                                           value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

panel.matriz.confucion.svm <- tabPanel(title = "Matriz de Confusión",
                                       plotOutput('plot.svm.mc', height = "45vh"),
                                       verbatimTextOutput("txtSvmMC"),
                                       aceEditor("fieldCodeSvmMC", mode = "r", theme = "monokai",
                                                 value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

panel.indices.generales.svm <- tabPanel(title = "Índices Generales",
                                        fluidRow(column(width = 6, gaugeOutput("svmPrecGlob", width = "100%")),
                                                 column(width = 6, gaugeOutput("svmErrorGlob", width = "100%"))),
                                        fluidRow(column(width = 2, gaugeOutput("svmPrecP", width = "100%")),
                                                 column(width = 2, gaugeOutput("svmPrecN", width = "100%")),
                                                 column(width = 2, gaugeOutput("svmFalP", width = "100%")),
                                                 column(width = 2, gaugeOutput("svmFalN", width = "100%")),
                                                 column(width = 2, gaugeOutput("svmAserP", width = "100%")),
                                                 column(width = 2, gaugeOutput("svmAserN", width = "100%"))),
                                        aceEditor("fieldCodeSvmIG", mode = "r", theme = "monokai",
                                                  value = "", height = "35vh", readOnly = F, autoComplete = "enabled"))

opciones.svm <- dropdownButton(h4("Opciones"),circle = F, status = "danger", icon = icon("gear"), width = "300px", right = T,
                               tooltip = tooltipOptions(title = "Clic para ver opciones"),
                               switchInput(inputId = "switch.scale.svm", onStatus = "success", offStatus = "danger", value = T,
                                           label = "Escalar datos", onLabel = "SI", offLabel = "NO", labelWidth = "100%"),
                               selectInput(inputId = "kernel.svm", label = "Seleccionar un Kernel",selected = 1,
                                           choices =  c("linear", "polynomial", "radial", "sigmoid")),
                               selectizeInput("select.var.svm",NULL,label = "Variables Predictoras:", multiple = T, choices = c(""),
                                              options = list(maxItems = Inf, placeholder = "Seleccione la(s) variable(s) predictoras")))

titulo.svm <- fluidRow(column(width = 4, opciones.svm),
                       column(width = 8, actionButton("runSvm", "Ejecutar", width = "100%" )))

pagina.svm <- tabItem(tabName = "svm",
                      column(width = 12,
                             tabBox(width = 12, title = titulo.svm,
                                    panel.generar.svm,
                                    plot.svm,
                                    panel.prediccion.svm,
                                    panel.matriz.confucion.svm,
                                    panel.indices.generales.svm)))

###########################################################################################################################
##### PAGINA DE DT
###########################################################################################################################

panel.generar.dt <- tabPanel(title = "Generación del Modelo",
                              verbatimTextOutput("txtDt"),
                              aceEditor("fieldCodeDt", mode = "r", theme = "monokai",
                                        value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

plot.dt <- tabPanel(title = "Gráfico Árbol",
                     plotOutput('plot.dt', height = "55vh"),
                     hr(),
                     aceEditor("fieldCodeDtPlot", mode = "r", theme = "monokai",
                               value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

panel.prediccion.dt <- tabPanel(title = "Predicción del Modelo",
                                 DT::dataTableOutput("dtPrediTable"),
                                 hr(),
                                 aceEditor("fieldCodeDtPred", mode = "r", theme = "monokai",
                                           value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

panel.matriz.confucion.dt <- tabPanel(title = "Matriz de Confusión",
                                       plotOutput('plot.dt.mc', height = "45vh"),
                                       verbatimTextOutput("txtDtMC"),
                                       aceEditor("fieldCodeDtMC", mode = "r", theme = "monokai",
                                                 value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

panel.indices.generales.dt <- tabPanel(title = "Índices Generales",
                                        fluidRow(column(width = 6, gaugeOutput("dtPrecGlob", width = "100%")),
                                                 column(width = 6, gaugeOutput("dtErrorGlob", width = "100%"))),
                                        fluidRow(column(width = 2, gaugeOutput("dtPrecP", width = "100%")),
                                                 column(width = 2, gaugeOutput("dtPrecN", width = "100%")),
                                                 column(width = 2, gaugeOutput("dtFalP", width = "100%")),
                                                 column(width = 2, gaugeOutput("dtFalN", width = "100%")),
                                                 column(width = 2, gaugeOutput("dtAserP", width = "100%")),
                                                 column(width = 2, gaugeOutput("dtAserN", width = "100%"))),
                                        aceEditor("fieldCodeDtIG", mode = "r", theme = "monokai",
                                                  value = "", height = "35vh", readOnly = F, autoComplete = "enabled"))

opciones.dt <- dropdownButton(h4("Opciones"),circle = F, status = "danger", icon = icon("gear"), width = "300px", right = T,
                               tooltip = tooltipOptions(title = "Clic para ver opciones"),
                               numericInput("minsplit.dt", "Mínimo para dividir un nodo:", 20, width = "100%"),
                               selectizeInput("select.var.dt",NULL,label = "Variables Predictoras:", multiple = T, choices = c(""),
                                              options = list(maxItems = Inf, placeholder = "Seleccione la(s) variable(s) predictoras")))


titulo.dt <- fluidRow(column(width = 4, opciones.dt),
                       column(width = 8, actionButton("runDt", "Ejecutar", width = "100%" )))

pagina.dt <- tabItem(tabName = "dt",
                      column(width = 12,
                             tabBox(width = 12, title = titulo.dt,
                                    panel.generar.dt,
                                    plot.dt,
                                    panel.prediccion.dt,
                                    panel.matriz.confucion.dt,
                                    panel.indices.generales.dt)))

###########################################################################################################################
##### PAGINA DE RF
###########################################################################################################################

panel.generar.rf <- tabPanel(title = "Generación del Modelo",
                             verbatimTextOutput("txtRf"),
                             aceEditor("fieldCodeRf", mode = "r", theme = "monokai",
                                       value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

plot.rf <- tabPanel(title = "Importancia",
                     plotOutput('plot.rf', height = "55vh"),
                     hr(),
                     aceEditor("fieldCodeRfPlot", mode = "r", theme = "monokai",
                               value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

panel.prediccion.rf <- tabPanel(title = "Predicción del Modelo",
                                DT::dataTableOutput("rfPrediTable"),
                                hr(),
                                aceEditor("fieldCodeRfPred", mode = "r", theme = "monokai",
                                          value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

panel.matriz.confucion.rf <- tabPanel(title = "Matriz de Confusión",
                                      plotOutput('plot.rf.mc', height = "45vh"),
                                      verbatimTextOutput("txtRfMC"),
                                      aceEditor("fieldCodeRfMC", mode = "r", theme = "monokai",
                                                value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

panel.indices.generales.rf <- tabPanel(title = "Índices Generales",
                                       fluidRow(column(width = 6, gaugeOutput("rfPrecGlob", width = "100%")),
                                                column(width = 6, gaugeOutput("rfErrorGlob", width = "100%"))),
                                       fluidRow(column(width = 2, gaugeOutput("rfPrecP", width = "100%")),
                                                column(width = 2, gaugeOutput("rfPrecN", width = "100%")),
                                                column(width = 2, gaugeOutput("rfFalP", width = "100%")),
                                                column(width = 2, gaugeOutput("rfFalN", width = "100%")),
                                                column(width = 2, gaugeOutput("rfAserP", width = "100%")),
                                                column(width = 2, gaugeOutput("rfAserN", width = "100%"))),
                                       aceEditor("fieldCodeRfIG", mode = "r", theme = "monokai",
                                                 value = "", height = "35vh", readOnly = F, autoComplete = "enabled"))

opciones.rf <- dropdownButton(h4("Opciones"),circle = F, status = "danger", icon = icon("gear"), width = "300px", right = T,
                              tooltip = tooltipOptions(title = "Clic para ver opciones"),
                              numericInput("ntree.rf", "Número de Áboles:", 20, width = "100%"),
                              selectizeInput("select.var.rf",NULL,label = "Variables Predictoras:", multiple = T, choices = c(""),
                                             options = list(maxItems = Inf, placeholder = "Seleccione la(s) variable(s) predictoras")))


titulo.rf <- fluidRow(column(width = 4, opciones.rf),
                      column(width = 8, actionButton("runRf", "Ejecutar", width = "100%" )))

pagina.rf <- tabItem(tabName = "rf",
                     column(width = 12,
                            tabBox(width = 12, title = titulo.rf,
                                   panel.generar.rf,
                                   plot.rf,
                                   panel.prediccion.rf,
                                   panel.matriz.confucion.rf,
                                   panel.indices.generales.rf)))

###########################################################################################################################
##### PAGINA DE BOOSTING
###########################################################################################################################

panel.generar.boosting <- tabPanel(title = "Generación del Modelo",
                              verbatimTextOutput("txtBoosting"),
                              aceEditor("fieldCodeBoosting", mode = "r", theme = "monokai",
                                        value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

plot.boosting <- tabPanel(title = "Gráfico del Modelo",
                                 plotOutput('plot.boosting', height = "55vh"),
                                 hr(),
                                 aceEditor("fieldCodeBoostingPlot", mode = "r", theme = "monokai",
                                           value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

plot.boosting.import <- tabPanel(title = "Importancia",
                    plotOutput('plot.boosting.import', height = "55vh"),
                    hr(),
                    aceEditor("fieldCodeBoostingPlotImport", mode = "r", theme = "monokai",
                              value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

panel.prediccion.boosting <- tabPanel(title = "Predicción del Modelo",
                                 DT::dataTableOutput("boostingPrediTable"),
                                 hr(),
                                 aceEditor("fieldCodeBoostingPred", mode = "r", theme = "monokai",
                                           value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

panel.matriz.confucion.boosting <- tabPanel(title = "Matriz de Confusión",
                                       plotOutput('plot.boosting.mc', height = "45vh"),
                                       verbatimTextOutput("txtBoostingMC"),
                                       aceEditor("fieldCodeBoostingMC", mode = "r", theme = "monokai",
                                                 value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

panel.indices.generales.boosting <- tabPanel(title = "Índices Generales",
                                        fluidRow(column(width = 6, gaugeOutput("boostingPrecGlob", width = "100%")),
                                                 column(width = 6, gaugeOutput("boostingErrorGlob", width = "100%"))),
                                        fluidRow(column(width = 2, gaugeOutput("boostingPrecP", width = "100%")),
                                                 column(width = 2, gaugeOutput("boostingPrecN", width = "100%")),
                                                 column(width = 2, gaugeOutput("boostingFalP", width = "100%")),
                                                 column(width = 2, gaugeOutput("boostingFalN", width = "100%")),
                                                 column(width = 2, gaugeOutput("boostingAserP", width = "100%")),
                                                 column(width = 2, gaugeOutput("boostingAserN", width = "100%"))),
                                        aceEditor("fieldCodeBoostingIG", mode = "r", theme = "monokai",
                                                  value = "", height = "35vh", readOnly = F, autoComplete = "enabled"))

opciones.boosting <- dropdownButton(h4("Opciones"),circle = F, status = "danger", icon = icon("gear"), width = "300px", right = T,
                               tooltip = tooltipOptions(title = "Clic para ver opciones"),
                               numericInput("iter.boosting", "Número de iteraciones:", 50, width = "100%",min = 1),
                               numericInput("nu.boosting", "Valor de nu:", 1, width = "100%",min = 0, max = 1),
                               selectInput(inputId = "tipo.boosting", label = "Seleccionar un tipo",selected = 1,
                                           choices =  c("discrete", "real", "gentle")),
                               selectizeInput("select.var.boosting",NULL,label = "Variables Predictoras:", multiple = T, choices = c(""),
                                              options = list(maxItems = Inf, placeholder = "Seleccione la(s) variable(s) predictoras")))


titulo.boosting <- fluidRow(column(width = 4,opciones.boosting),
                       column(width = 8,actionButton("runBoosting", "Ejecutar", width = "100%" )))

pagina.boosting <- tabItem(tabName = "boosting",
                      column(width = 12,
                             tabBox(width = 12, title = titulo.boosting,
                                    panel.generar.boosting,
                                    plot.boosting,
                                    plot.boosting.import,
                                    panel.prediccion.boosting,
                                    panel.matriz.confucion.boosting,
                                    panel.indices.generales.boosting)))


###########################################################################################################################
##### PAGINA DE XGBOOSTING
###########################################################################################################################

panel.generar.xgboosting <- tabPanel(title = "Generación del Modelo",
                                   verbatimTextOutput("txtXgBoosting"),
                                   aceEditor("fieldCodeXgBoosting", mode = "r", theme = "monokai",
                                             value = "", height = "30vh", readOnly = F, autoComplete = "enabled"))

plot.xgboosting.import <- tabPanel(title = "Importancia",
                                 plotOutput('plot.xgboosting.import', height = "55vh"),
                                 hr(),
                                 aceEditor("fieldCodeXgBoostingPlotImport", mode = "r", theme = "monokai",
                                           value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

panel.prediccion.xgboosting <- tabPanel(title = "Predicción del Modelo",
                                      DT::dataTableOutput("xgboostingPrediTable"),
                                      hr(),
                                      aceEditor("fieldCodeXgBoostingPred", mode = "r", theme = "monokai",
                                                value = "", height = "6vh", readOnly = F, autoComplete = "enabled"))

panel.matriz.confucion.xgboosting <- tabPanel(title = "Matriz de Confusión",
                                            plotOutput('plot.xgboosting.mc', height = "45vh"),
                                            verbatimTextOutput("txtXgBoostingMC"),
                                            aceEditor("fieldCodeXgBoostingMC", mode = "r", theme = "monokai",
                                                      value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

panel.indices.generales.xgboosting <- tabPanel(title = "Índices Generales",
                                             fluidRow(column(width = 6, gaugeOutput("xgboostingPrecGlob", width = "100%")),
                                                      column(width = 6, gaugeOutput("xgboostingErrorGlob", width = "100%"))),
                                             fluidRow(column(width = 2, gaugeOutput("xgboostingPrecP", width = "100%")),
                                                      column(width = 2, gaugeOutput("xgboostingPrecN", width = "100%")),
                                                      column(width = 2, gaugeOutput("xgboostingFalP", width = "100%")),
                                                      column(width = 2, gaugeOutput("xgboostingFalN", width = "100%")),
                                                      column(width = 2, gaugeOutput("xgboostingAserP", width = "100%")),
                                                      column(width = 2, gaugeOutput("xgboostingAserN", width = "100%"))),
                                             aceEditor("fieldCodeXgBoostingIG", mode = "r", theme = "monokai",
                                                       value = "", height = "35vh", readOnly = F, autoComplete = "enabled"))

# opciones.xgboosting <- dropdownButton(h4("Opciones"),circle = F, status = "danger", icon = icon("gear"), width = "300px", right = T,
#                                     tooltip = tooltipOptions(title = "Clic para ver opciones"),
#                                     numericInput("iter.xgboosting", "Número de iteraciones:", 50, width = "100%"),
#                                     numericInput("nu.xgboosting", "Valor de nu:", 1, width = "100%"),
#                                     selectInput(inputId = "tipo.xgboosting", label = "Seleccionar un tipo",selected = 1,
#                                                 choices =  c("discrete", "real", "gentle")),
#                                     selectizeInput("select.var.xgboosting",NULL,label = "Variables Predictoras:", multiple = T, choices = c(""),
#                                                    options = list(maxItems = Inf, placeholder = "Seleccione la(s) variable(s) predictoras")))


titulo.xgboosting <- fluidRow(column(width = 12, actionButton("runXgBoosting", "Ejecutar", width = "100%")))

pagina.xgboosting <- tabItem(tabName = "xgboosting",
                           column(width = 12,
                                  tabBox(width = 12, title = titulo.xgboosting,
                                         panel.generar.xgboosting,
                                         plot.xgboosting.import,
                                         panel.prediccion.xgboosting,
                                         panel.matriz.confucion.xgboosting,
                                         panel.indices.generales.xgboosting)))

###########################################################################################################################
##### PAGINA DE NN
###########################################################################################################################

panel.generar.nn <- tabPanel(title = "Generación del Modelo",
                              verbatimTextOutput("txtNn"),
                              aceEditor("fieldCodeNn", mode = "r", theme = "monokai",
                                        value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

panel.prediccion.nn <- tabPanel(title = "Predicción del Modelo",
                                 DT::dataTableOutput("nnPrediTable"),
                                 hr(),
                                 aceEditor("fieldCodeNnPred", mode = "r", theme = "monokai",
                                           value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

panel.matriz.confucion.nn <- tabPanel(title = "Matriz de Confusión",
                                       plotOutput('plot.nn.mc', height = "45vh"),
                                       verbatimTextOutput("txtNnMC"),
                                       aceEditor("fieldCodeNnMC", mode = "r", theme = "monokai",
                                                 value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))

panel.indices.generales.nn <- tabPanel(title = "Índices Generales",
                                        fluidRow(column(width = 6, gaugeOutput("nnPrecGlob", width = "100%")),
                                                 column(width = 6, gaugeOutput("nnErrorGlob", width = "100%"))),
                                        fluidRow(column(width = 2, gaugeOutput("nnPrecP", width = "100%")),
                                                 column(width = 2, gaugeOutput("nnPrecN", width = "100%")),
                                                 column(width = 2, gaugeOutput("nnFalP", width = "100%")),
                                                 column(width = 2, gaugeOutput("nnFalN", width = "100%")),
                                                 column(width = 2, gaugeOutput("nnAserP", width = "100%")),
                                                 column(width = 2, gaugeOutput("nnAserN", width = "100%"))),
                                        aceEditor("fieldCodeNnIG", mode = "r", theme = "monokai",
                                                  value = "", height = "35vh", readOnly = F, autoComplete = "enabled"))

opciones.nn <- dropdownButton(h4("Opciones"),circle = F, status = "danger", icon = icon("gear"), width = "300px", right = T,
                               tooltip = tooltipOptions(title = "Clic para ver opciones"),
                               numericInput("size.nn", "Número de unidades en capas ocultas:", 4, width = "100%", min = 0),
                               numericInput("rang.nn", "Inicio aleatorio de pesos:", 0.5, width = "100%", min = 0, max = 1),
                               numericInput("maxit.nn", "Número máximo de iteraciones:", 100, width = "100%", min = 1),
                               switchInput(inputId = "switch.trace.nn", onStatus = "success", offStatus = "danger", value = T,
                                           label = "Optimización:", onLabel = "SI", offLabel = "NO", labelWidth = "100%"),
                               sliderInput("kmax.nn", "K Máximo: ", min = 1, max = 100, value = 7),
                               selectInput(inputId = "kernel.nn", label = "Seleccionar un Kernel",selected = 1,
                                           choices =  c("optimal", "rectangular", "triangular", "epanechnikov", "biweight", "triweight", "cos","inv","gaussian","optimal")),
                               selectizeInput("select.var.nn",NULL,label = "Variables Predictoras:", multiple = T, choices = c(""),
                                              options = list(maxItems = Inf, placeholder = "Seleccione la(s) variable(s) predictoras")))


titulo.nn <- fluidRow(column(width = 4,opciones.nn),
                       column(width = 8,actionButton("runNn", "Ejecutar", width = "100%" )))

pagina.nn <- tabItem(tabName = "nnw",
                      column(width = 12,
                             tabBox(width = 12, title = titulo.nn,
                                    panel.generar.nn,
                                    panel.prediccion.nn,
                                    panel.matriz.confucion.nn,
                                    panel.indices.generales.nn)))


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
                                                       pagina.knn,
                                                       pagina.bayes,
                                                       pagina.svm,
                                                       pagina.dt,
                                                       pagina.rf,
                                                       pagina.boosting,
                                                       pagina.xgboosting,
                                                       pagina.nn))) )
