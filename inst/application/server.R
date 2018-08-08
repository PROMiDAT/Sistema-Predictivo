
shinyServer(function(input, output, session) {

  # Funciones Utilitarias ---------------------------------------------------------------------------------------------------

  #Mostar o ocultar la paginina de cargado
  load.page <- function(value = NA){
    if(value){
      shinyjs::show("loaderWrapper")
    }else{
      Sys.sleep(1)
      shinyjs::hide("loaderWrapper")
    }
  }

  #Crea una tabla dependiendo de los datos ingresados
  renderizar.tabla.datos <- function(data,editable = TRUE, extensions = c('Buttons'), dom = 'Bfrtip', pageLength = 5, buttons = T, filename = NA){
    if(buttons){
      buttons <- list(list(extend = 'csv', filename = filename, text = 'Descargar'))
    }else{
      buttons <- NULL
    }
    nombre.columnas <- c("ID", colnames(data))
    tipo.columnas <- c("", sapply(colnames(data),
                                  function(i) ifelse(class(data[,i]) %in% c("numeric", "integer"), "Numérico", "Categórico")))

    sketch <- htmltools::withTags(table(
      tableHeader(nombre.columnas),
      tableFooter(tipo.columnas)
    ))

    return(DT::datatable(data, selection = 'none', editable = editable, container = sketch, extensions = extensions,
                         options = list(dom = dom, pageLength = pageLength, buttons = buttons, scrollY = T)))
  }

  #Acualiza las distintas tablas
  actualizar.tabla <- function(x = c("datos","datos.aprendizaje","datos.prueba")){
    if(any("datos" %in% x)){ #Cambia la tabla de datos
      output$contents <- DT::renderDT(renderizar.tabla.datos(datos,editable = T,
                                                             pageLength = 10,
                                                             buttons = T,
                                                             filename = "datos"), server = F)
    }

    if(any("datos.aprendizaje" %in% x)){ #Cambia la tabla de datos de aprendizaje
      output$contentsAprend <- DT::renderDT(renderizar.tabla.datos(datos.aprendizaje,editable = T,
                                                                   pageLength = 5,
                                                                   buttons = T,
                                                                   filename = "datos.aprendizaje"), server = F)
    }

    if(any("datos.prueba" %in% x)){ #Cambia la tabla de datos de prueba
      output$contentsPrueba <- DT::renderDT(renderizar.tabla.datos(datos.prueba, editable = T,
                                                                   pageLength = 5,
                                                                   buttons = T,
                                                                   filename = "datos.prueba"), server = F)
    }
  }

  #Cierra un menu segun su tabName
  close.menu <- function(tabname = NA, valor  = T){
    select <- paste0("a[href^='#shiny-tab-",tabname,"']")
    if(valor){
      shinyjs::hide(selector = "ul.menu-open");
      shinyjs::disable(selector = select)
    }else{
      shinyjs::enable(selector = select)
    }
  }

  # Configuraciones iniciales -----------------------------------------------------------------------------------------------

  source('global.R', local = T)
  options(shiny.maxRequestSize=200*1024^2, DT.options = list(aLengthMenu = c(10, 30, 50), iDisplayLength = 10, scrollX = TRUE))
  load.page(F)
  shinyjs::disable(selector = 'a[href^="#shiny-tab-parte1"]')
  shinyjs::disable(selector = 'a[href^="#shiny-tab-parte2"]')
  shinyjs::disable(selector = 'a[href^="#shiny-tab-comparar"]')
  shinyjs::disable(selector = 'a[href^="#shiny-tab-poderPred]')
  shinyjs::disable(selector = 'a[href^="#shiny-tab-parte1"]')
  actualizar.tabla()
  updateAceEditor(session, "fieldCodeResum", value = cod.resum())

  # Valores Reactivos -------------------------------------------------------------------------------------------------------

  updatePlot <- reactiveValues(calc.normal = default.calc.normal(), normal = NULL, disp = NULL,
                               cor = NULL, dya.num = NULL, dya.cat = NULL)

  # Pagina de Datos ---------------------------------------------------------------------------------------------------------

  #Carga datos
  cargar.datos <- function(codigo.carga = ""){
    tryCatch({
      isolate(eval(parse(text = codigo.carga)))
      if(ncol(datos) < 1){
        showNotification(paste0("Error al cargar los Datos: Revisar separadores"), duration = 10, type = "error")
        return(NULL)
      }
    },
    error = function(e) {
      showNotification(paste0("Error al cargar los Datos: ", e), duration = 10, type = "error")
      datos <<- NULL
      datos.originales <<- NULL
      return(NULL)
    })
  }

  #Limpiado datos
  limpiar.datos <- function(){
    if(any(is.na(datos))){
      tryCatch({
        codigo.na <- paste0(code.NA(deleteNA = input$deleteNA), "\n", "datos <<- datos.originales")
        isolate(eval(parse(text = codigo.na)))
      }, error = function(e) {
        showNotification(paste0("Error al eliminar NAs: ", e), duration = 10, type = "error")
        datos <<- NULL
        datos.originales <<- NULL
        return(NULL)
      })
    } else {
      codigo.na <- ""
    }
    return(codigo.na)
  }

  #Transforma los datos
  transformar.datos <- function(){
    var.noactivas <- c()
    code.res <- "datos <<- datos.originales \n"
    for (var in colnames(datos.originales)) {
      if(input[[paste0("box", var, contador)]]) {
        if(input[[paste0("sel", var, contador)]] == "categorico" & class(datos.originales[, var]) %in% c("numeric","integer")) {
          code.res <- paste0(code.res, code.trans(var, "categorico"), "\n")
        }
        if(input[[paste0("sel", var, contador)]] == "numerico" & !(class(datos.originales[, var]) %in% c("numeric","integer"))) {
          code.res <- paste0(code.res, code.trans(var, "numerico"), "\n")
        }
        if(input[[paste0("sel", var, contador)]] == "disyuntivo"){
          code.res <- paste0(code.res, code.trans(var, "disyuntivo"), "\n")
        }
      } else {
        var.noactivas <- c(var.noactivas, var)
      }
    }

    isolate(eval(parse(text = code.res)))
    if(length(var.noactivas) > 0)
      isolate(eval(parse(text = code.desactivar(var.noactivas))))

    code.res <- paste0(code.res, "\n", code.desactivar(var.noactivas))

    return(code.res)
  }

  #Actualizar los distintos selectores
  acualizar.selecctores <- function(){
    updateSelectizeInput(session, "sel.normal", choices = colnames.empty(var.numericas(datos)))
    updateSelectizeInput(session, "select.var", choices = colnames.empty(var.numericas(datos)))
    updateSelectInput(session, "sel.distribucion.num", choices = colnames.empty(var.numericas(datos)))
    updateSelectInput(session, "sel.distribucion.cat", choices = colnames.empty(var.categoricas(datos)))
    updateSelectInput(session, "sel.resumen", choices = colnames.empty(datos))
    updateSelectInput(session, "sel.predic.var", choices = colnames.empty(var.categoricas(datos)))
  }

  #Crea las correlaciones
  ejecutar.modelo.cor <- function(){
    tryCatch({
      isolate(eval(parse(text = modelo.cor())))
      output$txtcor <- renderPrint(print(correlacion))
    }, error = function(e) {
      return(datos <- NULL)
    })
  }

  #Borra los datos de los modelos
  borrar.modelos <- function(){

    # -------------------  DATA ------------------------ #
    datos.prueba <<- NULL
    datos.aprendizaje <<- NULL
    variable.predecir <<- NULL

    # -------------------  KNN ------------------------ #

    modelo.knn <<- NULL
    MC.knn <<- NULL
    prediccion.knn <<- NULL
    indices.knn <<- rep(0,8)
    score.knn <<- NULL
    area.knn <<- NA

    # -------------------  SVM ------------------------ #

    modelo.svm <<- NULL
    MC.svm <<- NULL
    prediccion.svm <<- NULL
    indices.svm <<- rep(0,8)
    score.svm <<- NULL

    # -------------------  DT ------------------------ #

    modelo.dt <<- NULL
    MC.dt <<- NULL
    prediccion.dt <<- NULL
    indices.dt <<- rep(0,8)
    score.dt <<- NULL
    svm.roc <<- NULL

    # -------------------  RF ------------------------ #

    modelo.rf <<- NULL
    MC.rf <<- NULL
    prediccion.rf <<- NULL
    indices.rf <<- rep(0,8)
    score.rf <<- NULL

    # -------------------  BOOSTING ------------------------ #

    modelo.boosting <<- NULL
    MC.boosting <<- NULL
    prediccion.boosting <<- NULL
    indices.boosting <<- rep(0,8)
    score.booting <<- NULL
  }

  #Cunado es precionado el boton de cargar datos
  observeEvent(input$loadButton, {
    codigo.reporte <<- list()
    codigo.carga <- code.carga(nombre.filas = input$rowname, ruta = input$file1$datapath,
                         separador = input$sep, sep.decimal = input$dec, encabezado = input$header)

    #Carga los datos
    cargar.datos(codigo.carga)

    #Limpia los datos
    codigo.na <- limpiar.datos()

    # Actualiza el codigo
    updateAceEditor(session, "fieldCodeData", value = paste0(codigo.carga, "\n", codigo.na))

    #Actualiza los selectores que dependen de los datos
    acualizar.selecctores()

    # modelo correlacion
    ejecutar.modelo.cor()

    #Cierra o abre lo s menus los menus
    close.menu("parte1", is.null(datos))
    close.menu("parte2", is.null(datos.aprendizaje))
    close.menu("comparar", is.null(datos.aprendizaje))
    close.menu("poderPred", is.null(datos.aprendizaje))

    #Cambia las tablas de datos
    actualizar.tabla()

    #borra los datos de modelos
    borrar.modelos()
  }, priority = 4)

  #Cunado es precionado el boton de transformar datos
  observeEvent(input$transButton, {

    #transforma los datos
    code.res <- transformar.datos()

    # Actualiza el codigo
    updateAceEditor(session, "fieldCodeTrans", value = code.res)

    #Actualiza los selectores que dependen de los datos
    acualizar.selecctores()

    # modelo correlacion
    ejecutar.modelo.cor()

    #Cierra o abre lo s menus los menus
    close.menu("parte1", is.null(datos))
    close.menu("parte2", is.null(datos.aprendizaje))
    close.menu("comparar", is.null(datos.aprendizaje))
    close.menu("poderPred", is.null(datos.aprendizaje))

    #Cambia las tablas de datos
    actualizar.tabla()

    #borra los datos de modelos
    borrar.modelos()
  }, priority = 4)

  #Crea los select box del panel de trasnformar datos
  update.trans <- eventReactive(input$loadButton, {
    contador <<- contador + 1
    if(!is.null(datos) && ncol(datos) > 0) {
      res <-  data.frame(Variables = colnames(datos), Tipo = c(1:ncol(datos)), Activa = c(1:ncol(datos)))
      res$Tipo <- sapply(colnames(datos), function(i) paste0('<select id="sel', i, contador, '"> <option value="categorico">Categórico</option>
                                                             <option value="numerico" ', ifelse(class(datos[, i]) %in% c("numeric","integer"),
                                                                                                ' selected="selected"', ''),
                                                             '>Numérico</option> <option value="disyuntivo">Disyuntivo</option> </select>'))
      res$Activa <- sapply(colnames(datos), function(i) paste0('<input type="checkbox" id="box', i, contador, '" checked/>'))
    }else{
      res <-  as.data.frame(NULL)
      showNotification("Tiene que cargar los datos", duration = 10, type = "error")
    }
    return(res)
  })

  #Cambia la tabla de con las opciones del panel de transformar
  output$transData <- DT::renderDataTable(update.trans(), escape = FALSE, selection = 'none', server = FALSE,
                                          options = list(dom = 't', paging = FALSE, ordering = FALSE), rownames = F,
                                          callback = JS("table.rows().every(function(i, tab, row) {
                                                        var $this = $(this.node());
                                                        $this.attr('id', this.data()[0]);
                                                        $this.addClass('shiny-input-checkbox');});
                                                        Shiny.unbindAll(table.table().node());
                                                        Shiny.bindAll(table.table().node());"))

  # Pagina de Resumen --------------------------------------------------------------------------------------------------------

  #Cambia la tabla con el summary en la pagina de resumen
  output$resumen.completo <- DT::renderDataTable(obj.resum(), options = list(dom = 'ft', scrollX = TRUE), rownames = F)

  #Se crea una tabla summary
  obj.resum <- eventReactive(c(input$loadButton, input$transButton), {
    codigo.reporte[["resumen"]] <<- c(paste0("## Resumen Numérico \n", "```{r} \n",
                                             "summary(datos) \n", "```"))
    data.frame(unclass(summary(datos)), check.names = FALSE, stringsAsFactors = FALSE)
  })

  #Cambia los cuadros de summary por varibale
  output$resumen <- renderUI({
    if(input$sel.resumen %in% colnames(var.numericas(datos))){
      HTML(resumen.numerico(datos, input$sel.resumen))
    } else {
      HTML(resumen.categorico(datos, input$sel.resumen))
    }
  })

  # Pagina del Test de Normalidad --------------------------------------------------------------------------------------------

  #Hace el grafico de la pagina de test de normalidad
  observeEvent(c(input$loadButton, input$transButton), {
    output$plot.normal = renderPlot({
      tryCatch({
        cod.normal <<- updatePlot$normal
        res <- isolate(eval(parse(text = cod.normal)))
        updateAceEditor(session, "fieldCodeNormal", value = cod.normal)
        codigo.reporte[[paste0("normalidad.", input$sel.normal)]] <<- paste0("## Test de Normalidad \n```{r}\n", cod.normal, "\n```")
        return(res)
      }, error = function(e){
        showNotification(paste0("ERROR AL GENERAR TEST DE NORMALIDAD: ", e), duration = 10, type = "error")
      })
    })
  })

  observeEvent(input$run.normal, {
    updatePlot$normal <- input$fieldCodeNormal
  })

  observeEvent(c(input$sel.normal, input$col.normal), {
    updatePlot$normal <- default.normal(data = "datos", vars = input$sel.normal, color = input$col.normal)
  })


  # Termina la Sesion -------------------------------------------------------------------------------------------------------

  session$onSessionEnded(function() {
    rm(envir = .GlobalEnv, list = ls(envir = .GlobalEnv))
    stopApp()
  })

})
