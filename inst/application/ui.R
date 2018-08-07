
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
library(dplyr)
library(forcats)
library(psych)
library(ROCR)


#  FUNCIONES --------------------------------------------------------------------------------------------------------------

# Crea un campo de codigo con boton de ejecutar y cargar
campo.codigo <- function(runid, refid, fieldid, ...){
  tags$div(class = "box box-solid bg-black",
           tags$div(style = "text-align:right;padding-right: 10px;",
                    tags$button(id = runid, type = "button", class = "run-button action-button",
                                icon("play"), tags$a("Ejecutar", style = "color:white")),
           tags$button(id = refid, type = "button", class = "run-button action-button",
                       icon("undo"), tags$a("Recuperar", style = "color:white"))),
           tags$div(class = "box-body",
                    aceEditor(fieldid, mode = "r", theme = "monokai", value = "", ...)))
}

# MENU --------------------------------------------------------------------------------------------------------------------

menu.cargar <- menuItem("Datos", tabName = "cargar", icon = icon("dashboard"))

menu.estadisticas <- menuItem("Estadísticas Básicas", tabName = "parte1", icon = icon("th-list"),
                              menuSubItem("Resumen Numérico", tabName = "resumen", icon = icon("sort-numeric-asc")),
                              menuSubItem("Test de Normalidad", tabName = "normalidad", icon = icon("bar-chart")),
                              menuSubItem("Dispersión", tabName = "dispersion", icon = icon("line-chart")),
                              menuSubItem("Distribuciones", tabName = "distribucion", icon = icon("area-chart")),
                              menuSubItem("Correlación", tabName = "correlacion", icon = icon("table")),
                              menuItem("Poder Predictivo", tabName = "poderPred", icon = icon("rocket")))

menu.aprendizaje.supervisado <- menuItem("Aprendizaje Supervisado", tabName = "parte2", icon = icon("th-list"),
                                         menuSubItem("K Vecinos Más Cercanos",tabName = "knn",icon = icon("bar-chart-o")),
                                         menuSubItem("Soporte Vectorial",tabName = "svm",icon = icon("bar-chart-o")),
                                         menuSubItem("Árboles de Decisión",tabName = "dt",icon = icon("bar-chart-o")),
                                         menuSubItem("Bosques Aleatorios",tabName = "rf",icon = icon("bar-chart-o")),
                                         menuSubItem("ADA - Boosting",tabName = "boosting",icon = icon("bar-chart-o")) )

menu.reporte <- menuItem("Generar Reporte", tabName = "reporte", icon = icon("save-file",lib = "glyphicon"))

menu.comparar <- menuItem("Comparación de Modelos", tabName = "comparar", icon = icon("eye"))

menu.info <- menuItem("Acerca De", tabName = "acercaDe", icon = icon("info"))

mi.menu <- sidebarMenu(id = "principal",
              tags$div(style="padding-top:10px;"),
              menu.cargar,
              menu.estadisticas,
              menu.aprendizaje.supervisado,
              menu.comparar,
              menu.reporte,
              menu.info)


# HEAD HTML ---------------------------------------------------------------------------------------------------------

mi.head <- tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "style_promidat.css"),
  tags$link(rel="shortcut icon", href="http://www.promidat.org/theme/image.php/formal_white/theme/1438713216/favicon"),
  useShinyjs()
)

# PAGINA DE CARGA Y  TRANSFORMACION DE DATOS -----------------------------------------------------------------------------

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
                                  fluidRow(column(width = 9, numericInput("semilla", "Semilla Aleatoria:", "NULL", width = "100%")), br(),
                                           column(width = 1, switchInput(inputId = "permitir.semilla", onStatus = "success", offStatus = "danger", value = F, width = "100%",
                                                                         label = "", onLabel = "", offLabel = "", labelWidth = "100%",inline = T,size = "large"))),
                                  selectInput(inputId = "sel.predic.var", label = h4("Seleccionar Variable a Predecir:"), choices =  "", width = "100%"),
                                  sliderInput("segmentacionDatosA", "Proporción Aprendizaje:",width = "100%",
                                              min = 5, max = 95, value = 70, step = 5),
                                  sliderInput("segmentacionDatosT", "Proporción Prueba:", width = "100%",
                                              min = 5, max = 95, value = 30, step = 5),
                                  actionButton("segmentButton", "Generar", width = "100%"),
                                  br(),br(),
                                  aceEditor("fieldCodeSegment", mode = "r", theme = "monokai", value = "", height = "8vh",  readOnly = T))

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


# PAGINA DE RESUMEN NUMERICO ----------------------------------------------------------------------------------------------

cuadro.resumen.completo <- box(title = "Resumen Numérico", status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                               DT::dataTableOutput("resumen.completo"), hr(),
                               campo.codigo("run.resume", "ref.resume", "fieldCodeResum", height = "8vh"))

cuadro.resumen.variable <- box(title = "Resumen Numérico por Variable", status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                               selectInput(inputId = "sel.resumen", label = h4("Seleccionar Variable:"), choices =  ""),
                               fluidRow(uiOutput("resumen")))

pagina.resumen.numerico <- tabItem(tabName = "resumen",
                                   column(width = 7, cuadro.resumen.completo ),
                                   column(width = 5, cuadro.resumen.variable ))

# PAGINA DEL TEST DE NORMALIDAD -------------------------------------------------------------------------------------------

boton.colores <- column(width = 3, dropdownButton(h4("Opciones"),
                                                  colourpicker::colourInput("col.normal", "Seleccionar Color:",
                                                                            value = "#00FF22AA", allowTransparent = T),
                                                  circle = F, status = "danger", icon = icon("gear"), width = "100%",
                                                  tooltip = tooltipOptions(title = "Clic para ver opciones"), right = T))


opciones.normalidad <-  fluidRow( column(width = 9, selectInput(inputId = "sel.normal", label = NULL, choices =  "")),
                                  boton.colores )

panel.grafico.normalidad.num <- tabPanel(title = "Gráfico Normalidad", value = "tabNormalPlot", plotOutput('plot.normal', height = "65vh"))

panel.grafico.normalidad.cat <- tabPanel(title = "Test de Normalidad", value = "tabNormalCalc", DT::dataTableOutput('calculo.normal'))

codigo.normalidad.uno <- conditionalPanel("input.BoxNormal == 'tabNormalPlot'",
                                          column(width = 12, campo.codigo("run.normal", "ref.normal", "fieldCodeNormal", height = "8vh")))

codigo.normalidad.dos <- conditionalPanel("input.BoxNormal == 'tabNormalCalc'",
                                          column(width = 12, campo.codigo("run.calc.normal", "ref.calc.normal", "fieldCalcNormal", height = "8vh")))

pagina.test.normalidad <- tabItem(tabName = "normalidad",
                                  column(width = 12,  tabBox(id = "BoxNormal",
                                                             width = NULL,
                                                             title = opciones.normalidad,
                                                             panel.grafico.normalidad.num,
                                                             panel.grafico.normalidad.cat)),
                                  codigo.normalidad.uno,
                                  codigo.normalidad.dos)

# PAGINA DE DISPERSION -----------------------------------------------------------------------------------------------------


codigo.dispersion <- column(width = 12, campo.codigo("run.disp", "ref.disp", "fieldCodeDisp", height = "8vh"))

opciones.dispersion <- fluidRow(column(width = 9, tags$div(class="select-var-ind",
                                                            selectizeInput("select.var", NULL, multiple = T, choices = c(""),
                                                                           options = list(maxItems = 3, placeholder = "Seleccione la(s) variable(s)")))),
                                 column(width = 3,dropdownButton(h4("Opciones"),
                                                                 colourpicker::colourInput("col.disp", "Seleccionar Color:",
                                                                                           value = "#FF0000AA", allowTransparent = T),
                                                                 circle = F, status = "danger", icon = icon("gear"), width = "100%",
                                                                 tooltip = tooltipOptions(title = "Clic para ver opciones"), right = T)))


grafico.dispersion <- tabPanel(title = "Dispersión", value = "tabDisp", plotOutput('plot.disp', height = "65vh"))

pagina.dispersion<- tabItem(tabName = "dispersion",
                            column(width = 12, tabBox(id = "BoxDisp", width = NULL,
                                                      title = opciones.dispersion,
                                                      grafico.dispersion)),
                            codigo.dispersion )

# PAGINA DE CORRELACIONES -------------------------------------------------------------------------------------------------

opciones.correlaciones <- dropdownButton(h4("Opciones"),
                                         selectInput(inputId = "cor.metodo", label = "Seleccionar Método",
                                                     choices =  c("circle", "square", "ellipse", "number", "shade", "color", "pie")),
                                         selectInput(inputId = "cor.tipo", label = "Seleccionar Tipo", choices =  c("lower", "upper", "full")),
                                         circle = F, status = "danger", icon = icon("gear"), width = "300px", right = T,
                                         tooltip = tooltipOptions(title = "Clic para ver opciones"))

tab.correlacion <- tabPanel(title = 'Correlación', value = "correlacion",
                            plotOutput('plot.cor', height = "67vh"),
                            fluidRow(column(width = 4,aceEditor("fieldModelCor", height = "6vh", mode = "r",
                                                                theme = "monokai", value = "", readOnly = T)),
                                     column(width = 8,campo.codigo("run.code.cor", "ref.code.cor", "fieldCodeCor", height = "6vh"))))

tab.codigo.correlaciones <- tabPanel(title = 'Resultados Numéricos', value = "cor.salida", verbatimTextOutput("txtcor"))

pagina.correlaciones <- tabItem(tabName = "correlacion",
                                column(width = 12, tabBox(id = "tabCor", width = NULL,
                                                          title = opciones.correlaciones,
                                                          tab.correlacion,
                                                          tab.codigo.correlaciones)))

#PAGINA DE DISTRIBUCIONES -------------------------------------------------------------------------------------------------

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


# PAGINA DE PODER PREDICTIVO ----------------------------------------------------------------------------------------------

plot.dist.poder <- tabPanel(title = 'Variables Numéricas',
                             plotOutput('plot.dist.poder', height = "65vh"),
                             selectInput(inputId = "sel.distribucion.poder", label = NULL, choices =  "", width = "100%"))

plot.pairs.poder <- tabPanel(title = 'Variables Categóricas',
                             plotOutput('plot.pairs.poder', height = "65vh"))


pagina.poder <- tabItem(tabName = "poderPred", column(width = 12, tabBox(width = 12, plot.dist.poder,plot.pairs.poder)) )

# PAGINA DE KNN -----------------------------------------------------------------------------------------------------------

panel.generar.knn <- tabPanel(title = "Generación del Modelo",
                             verbatimTextOutput("txtknn"),
                             campo.codigo("runKnn","restarKnn","fieldCodeKnn",height = "3vh", readOnly = FALSE))

panel.prediccion.knn <- tabPanel(title = "Predicción del Modelo",
                                 DT::dataTableOutput("knnPrediTable"),
                                 hr(),
                                 aceEditor("fieldCodeKnnPred", mode = "r", theme = "monokai",
                                           value = "", height = "3vh", readOnly = T, autoComplete = "enabled"))

panel.matriz.confucion.knn <- tabPanel(title = "Matriz de Confusión",
                                       plotOutput('plot.knn.mc', height = "45vh"),
                                       verbatimTextOutput("txtknnMC"),
                                       aceEditor("fieldCodeKnnMC", mode = "r", theme = "monokai",
                                                 value = "", height = "3vh", readOnly = T, autoComplete = "enabled"))

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
                                              value = "", height = "37vh", readOnly = T, autoComplete = "enabled"))

opciones.knn <- dropdownButton(h4("Opciones"),circle = F, status = "danger", icon = icon("gear"), width = "300px", right = T,
                               tooltip = tooltipOptions(title = "Clic para ver opciones"),
                               switchInput(inputId = "switch.scale.knn", onStatus = "success", offStatus = "danger", value = T,
                                           label = "Escalar datos", onLabel = "SI", offLabel = "NO", labelWidth = "100%"),
                               sliderInput("kmax.knn", "K Máximo: ", min = 1, max = 100, value = 7),
                               selectInput(inputId = "kernel.knn", label = "Seleccionar un Kernel",selected = 1,
                                           choices =  c("optimal", "rectangular", "triangular", "epanechnikov", "biweight",
                                                        "triweight", "cos","inv","gaussian","optimal")))


titulo.knn <- fluidRow(column(width = 12,opciones.knn))

pagina.knn <- tabItem(tabName = "knn",
                      column(width = 12,
                             tabBox(width = 12, title = titulo.knn,
                                    panel.generar.knn,
                                    panel.prediccion.knn,
                                    panel.matriz.confucion.knn,
                                    panel.indices.generales.knn)))


# PAGINA DE SVM -----------------------------------------------------------------------------------------------------------

panel.generar.svm <- tabPanel(title = "Generación del Modelo",
                              verbatimTextOutput("txtSvm"),
                              campo.codigo("runSvm","restarSvm","fieldCodeSvm",height = "3vh", readOnly = FALSE, autoComplete = "enabled"))

plot.svm <- tabPanel(title = "Gráfico Clasificación",
                     plotOutput('plot.svm', height = "55vh"),
                     hr(),
                     selectizeInput("select.var.svm.plot",NULL,label = "Variables Predictoras:", multiple = T, choices = c(""),
                                    options = list(maxItems = 2, placeholder = "Seleccione la(s) variable(s) predictoras"), width = "100%"),
                     aceEditor("fieldCodeSvmPlot", mode = "r", theme = "monokai",
                               value = "", height = "3vh", readOnly = T, autoComplete = "enabled"))

panel.prediccion.svm <- tabPanel(title = "Predicción del Modelo",
                                 DT::dataTableOutput("svmPrediTable"),
                                 hr(),
                                 aceEditor("fieldCodeSvmPred", mode = "r", theme = "monokai",
                                           value = "", height = "3vh", readOnly = T, autoComplete = "enabled"))

panel.matriz.confucion.svm <- tabPanel(title = "Matriz de Confusión",
                                       plotOutput('plot.svm.mc', height = "45vh"),
                                       verbatimTextOutput("txtSvmMC"),
                                       aceEditor("fieldCodeSvmMC", mode = "r", theme = "monokai",
                                                 value = "", height = "3vh", readOnly = T, autoComplete = "enabled"))

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
                                                  value = "", height = "37vh", readOnly = T, autoComplete = "enabled"))

opciones.svm <- dropdownButton(h4("Opciones"),circle = F, status = "danger", icon = icon("gear"), width = "300px", right = T,
                               tooltip = tooltipOptions(title = "Clic para ver opciones"),
                               switchInput(inputId = "switch.scale.svm", onStatus = "success", offStatus = "danger", value = T,
                                           label = "Escalar datos", onLabel = "SI", offLabel = "NO", labelWidth = "100%"),
                               selectInput(inputId = "kernel.svm", label = "Seleccionar un Kernel", selected = 1,
                                           choices =  c("linear", "polynomial", "radial", "sigmoid")))

titulo.svm <- fluidRow(column(width = 12, opciones.svm))

pagina.svm <- tabItem(tabName = "svm",
                      column(width = 12,
                             tabBox(width = 12, title = titulo.svm,
                                    panel.generar.svm,
                                    plot.svm,
                                    panel.prediccion.svm,
                                    panel.matriz.confucion.svm,
                                    panel.indices.generales.svm)))

# PAGINA DE DT ------------------------------------------------------------------------------------------------------------

panel.generar.dt <- tabPanel(title = "Generación del Modelo",
                              verbatimTextOutput("txtDt"),
                              campo.codigo("runDt","restarDt","fieldCodeDt",height = "3vh", readOnly = F, autoComplete = "enabled"))

plot.dt <- tabPanel(title = "Gráfico Árbol",
                     plotOutput('plot.dt', height = "55vh"),
                     hr(),
                     aceEditor("fieldCodeDtPlot", mode = "r", theme = "monokai",
                               value = "", height = "6vh", readOnly = T, autoComplete = "enabled"))

panel.prediccion.dt <- tabPanel(title = "Predicción del Modelo",
                                 DT::dataTableOutput("dtPrediTable"),
                                 hr(),
                                 aceEditor("fieldCodeDtPred", mode = "r", theme = "monokai",
                                           value = "", height = "3vh", readOnly = T, autoComplete = "enabled"))

panel.matriz.confucion.dt <- tabPanel(title = "Matriz de Confusión",
                                       plotOutput('plot.dt.mc', height = "45vh"),
                                       verbatimTextOutput("txtDtMC"),
                                       aceEditor("fieldCodeDtMC", mode = "r", theme = "monokai",
                                                 value = "", height = "3vh", readOnly = T, autoComplete = "enabled"))

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
                                                  value = "", height = "37vh", readOnly = T, autoComplete = "enabled"))

opciones.dt <- dropdownButton(h4("Opciones"),circle = F, status = "danger", icon = icon("gear"), width = "300px", right = T,
                               tooltip = tooltipOptions(title = "Clic para ver opciones"),
                               numericInput("minsplit.dt", "Mínimo para dividir un nodo:", 20, width = "100%"))


titulo.dt <- fluidRow(column(width = 12, opciones.dt))

pagina.dt <- tabItem(tabName = "dt",
                      column(width = 12,
                             tabBox(width = 12, title = titulo.dt,
                                    panel.generar.dt,
                                    plot.dt,
                                    panel.prediccion.dt,
                                    panel.matriz.confucion.dt,
                                    panel.indices.generales.dt)))

# PAGINA DE RF ------------------------------------------------------------------------------------------------------------

panel.generar.rf <- tabPanel(title = "Generación del Modelo",
                             verbatimTextOutput("txtRf"),
                             campo.codigo("runRf","restarRf","fieldCodeRf",height = "3vh", readOnly = F, autoComplete = "enabled"))

plot.rf <- tabPanel(title = "Importancia de Variables",
                     plotOutput('plot.rf', height = "55vh"),
                     hr(),
                     aceEditor("fieldCodeRfPlot", mode = "r", theme = "monokai",
                               value = "", height = "3vh", readOnly = T, autoComplete = "enabled"))

panel.prediccion.rf <- tabPanel(title = "Predicción del Modelo",
                                DT::dataTableOutput("rfPrediTable"),
                                hr(),
                                aceEditor("fieldCodeRfPred", mode = "r", theme = "monokai",
                                          value = "", height = "3vh", readOnly = T, autoComplete = "enabled"))

panel.matriz.confucion.rf <- tabPanel(title = "Matriz de Confusión",
                                      plotOutput('plot.rf.mc', height = "45vh"),
                                      verbatimTextOutput("txtRfMC"),
                                      aceEditor("fieldCodeRfMC", mode = "r", theme = "monokai",
                                                value = "", height = "3vh", readOnly = T, autoComplete = "enabled"))

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
                                                 value = "", height = "37vh", readOnly = T, autoComplete = "enabled"))

opciones.rf <- dropdownButton(h4("Opciones"),circle = F, status = "danger", icon = icon("gear"), width = "300px", right = T,
                              tooltip = tooltipOptions(title = "Clic para ver opciones"),
                              numericInput("ntree.rf", "Número de Áboles:", 20, width = "100%"))


titulo.rf <- fluidRow(column(width = 12, opciones.rf))

pagina.rf <- tabItem(tabName = "rf",
                     column(width = 12,
                            tabBox(width = 12, title = titulo.rf,
                                   panel.generar.rf,
                                   plot.rf,
                                   panel.prediccion.rf,
                                   panel.matriz.confucion.rf,
                                   panel.indices.generales.rf)))

# PAGINA DE BOOSTING ------------------------------------------------------------------------------------------------------

panel.generar.boosting <- tabPanel(title = "Generación del Modelo",
                              verbatimTextOutput("txtBoosting"),
                              campo.codigo("runBoosting","restarBoosting","fieldCodeBoosting",height = "3vh", readOnly = F, autoComplete = "enabled"))

plot.boosting <- tabPanel(title = "Evolución del Error",
                                 plotOutput('plot.boosting', height = "55vh"),
                                 hr(),
                                 aceEditor("fieldCodeBoostingPlot", mode = "r", theme = "monokai",
                                           value = "", height = "3vh", readOnly = T, autoComplete = "enabled"))

plot.boosting.import <- tabPanel(title = "Importancia de Variables",
                    plotOutput('plot.boosting.import', height = "55vh"),
                    hr(),
                    aceEditor("fieldCodeBoostingPlotImport", mode = "r", theme = "monokai",
                              value = "", height = "3vh", readOnly = T, autoComplete = "enabled"))

panel.prediccion.boosting <- tabPanel(title = "Predicción del Modelo",
                                 DT::dataTableOutput("boostingPrediTable"),
                                 hr(),
                                 aceEditor("fieldCodeBoostingPred", mode = "r", theme = "monokai",
                                           value = "", height = "3vh", readOnly = T, autoComplete = "enabled"))

panel.matriz.confucion.boosting <- tabPanel(title = "Matriz de Confusión",
                                       plotOutput('plot.boosting.mc', height = "45vh"),
                                       verbatimTextOutput("txtBoostingMC"),
                                       aceEditor("fieldCodeBoostingMC", mode = "r", theme = "monokai",
                                                 value = "", height = "3vh", readOnly = T, autoComplete = "enabled"))

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
                                                  value = "", height = "37vh", readOnly = T, autoComplete = "enabled"))

opciones.boosting <- dropdownButton(h4("Opciones"),circle = F, status = "danger", icon = icon("gear"), width = "300px", right = T,
                               tooltip = tooltipOptions(title = "Clic para ver opciones"),
                               numericInput("iter.boosting", "Número de áboles:", 50, width = "100%",min = 1),
                               numericInput("nu.boosting", "Valor de nu:", 1, width = "100%",min = 0, max = 1),
                               selectInput(inputId = "tipo.boosting", label = "Seleccionar un tipo",selected = 1,
                                           choices =  c("discrete", "real", "gentle")))


titulo.boosting <- fluidRow(column(width = 12,opciones.boosting))

pagina.boosting <- tabItem(tabName = "boosting",
                      column(width = 12,
                             tabBox(width = 12, title = titulo.boosting,
                                    panel.generar.boosting,
                                    plot.boosting,
                                    plot.boosting.import,
                                    panel.prediccion.boosting,
                                    panel.matriz.confucion.boosting,
                                    panel.indices.generales.boosting)))

# PAGINA DE COMPARACION DE MODELOS ---------------------------------------------------------------------------------------

panel.comparacion.tabla <- tabPanel(title = "Tabla Comparativa",
                                    DT::dataTableOutput("TablaComp"),
                                    hr(),
                                    checkboxGroupInput("select.models", "Mostrar Modelos:",
                                                       c("KNN" = "sel.knn",
                                                         "SVM" = "sel.svm",
                                                         "ÁRBOLES" = "sel.dt",
                                                         "BOSQUES" = "sel.rf",
                                                         "ADA - BOOSTING" = "sel.boosting"),
                                                       inline = TRUE,
                                                       selected = c("sel.knn","sel.svm","sel.dt","sel.rf","sel.boosting")))

plot.comparacion.roc <- tabPanel(title = "Curva ROC",
                          plotOutput('plot.roc', height = "55vh"),
                          hr(),
                          checkboxGroupInput("select.models.roc", "Mostrar Modelos:",
                                             c("KNN" = "sel.knn",
                                               "SVM" = "sel.svm",
                                               "ÁRBOLES" = "sel.dt",
                                               "BOSQUES" = "sel.rf",
                                               "ADA - BOOSTING" = "sel.boosting"),
                                             inline = TRUE,
                                             selected = c("sel.knn","sel.svm","sel.dt","sel.rf","sel.boosting")),
                          fluidRow(column(width = 12, selectInput(inputId = "roc.sel",
                                                                  label = h4("Seleccionar la Categoría:"),
                                                                  choices =  "", width = "100%"))))

pagina.comparacion <- tabItem(tabName = "comparar",
                           column(width = 12,
                                  tabBox(width = 12,
                                         panel.comparacion.tabla,
                                         plot.comparacion.roc )))

# PAGINA DE REPORTE -------------------------------------------------------------------------------------------------------

panel.reporte.codigo <- column(width = 5,
                               tabBox(width = 12, id = "tabReporte",
                                      tabPanel(title = "Reporte", width = 12),
                                      tabPanel(title = "Código", width = 12, aceEditor("fieldCodeReport", mode="markdown", value=''))),
                               downloadButton("descargar", "Descargar", style = "position: relative; left: 40%") )

vista.previa.reporte <- column(width = 7,
                               box(title = "Vista Previa", width = 12, height = "90vh", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                   div(style = 'overflow-x: scroll; overflow-y: scroll; height: 80vh;', htmlOutput("knitDoc"))) )

pagina.generar.reporte <- tabItem(tabName = "reporte", panel.reporte.codigo , vista.previa.reporte )

# PAGINA DE INFORMACION ---------------------------------------------------------------------------------------------------

pagina.info <- tabItem(tabName = "acercaDe",
                img(src="Logo.png", style="padding-bottom:20px;margin-left: auto;margin-right: auto;display: block;width: 50%;"),
                infoBox("Todos los derechos reservados a", "PROMiDAT S.A", icon = icon("copyright"), fill = T, color = "yellow", width = "100%"),
                infoBox("Versión del Sistema", "1.0.1", icon = icon("file-code-o"), fill = T, color = "yellow", width = "100%"))

# PAGINA COMPLETA ---------------------------------------------------------------------------------------------------------

shinyUI(dashboardPage(title="PROMiDAT",
                      dashboardHeader(title = tags$a(href="http://promidat.com",
                                                     img(src="Logo2.png", height=55, width="100%", style="padding-top:2px; padding-bottom:6px;"))),
                      dashboardSidebar(mi.menu),
                      dashboardBody(mi.head,
                                    div(id = "loaderWrapper", div(id="loader")),
                                    tabItems( pagina.cargar.datos,
                                              pagina.resumen.numerico,
                                              pagina.test.normalidad,
                                              pagina.dispersion,
                                              pagina.correlaciones,
                                              pagina.distribuciones,
                                              pagina.poder,
                                              pagina.knn,
                                              pagina.svm,
                                              pagina.dt,
                                              pagina.rf,
                                              pagina.boosting,
                                              pagina.comparacion,
                                              pagina.generar.reporte,
                                              pagina.info))) )
