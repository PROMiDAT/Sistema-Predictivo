
shinyServer(function(input, output, session) {

  ###########################################################################################################################
  ##### Configuraciones iniciales
  ###########################################################################################################################

  source('global.R', local = T)
  Sys.setenv("LANGUAGE" = "ES")
  options(shiny.maxRequestSize=200*1024^2,
          DT.options = list(aLengthMenu = c(10, 30, 50),
                            iDisplayLength = 10, scrollX = TRUE),
          encoding = "utf8")


  ###########################################################################################################################
  ##### Eventos
  ###########################################################################################################################

  #Termina la Sesion
  session$onSessionEnded(function() {
    stopApp()
  })

  #Cunado es precionado el boton de cargar datos
  observeEvent(input$loadButton, {
    codigo <- code.carga(nombre.filas = input$rowname, ruta = input$file1$datapath,
                         separador = input$sep, sep.decimal = input$dec, encabezado = input$header)

    updateAceEditor(session, "fieldCodeData", value = codigo)

    tryCatch({
      isolate(eval(parse(text = codigo)))
    },
    error = function(e) {
      showNotification("Error al cargar los datos, intente nuevamente", duration = 15, type = "error")
      datos <<- NULL
      datos.originales <<- NULL
      return(NULL)
    })

    updateSelectizeInput(session, "sel.normal", choices = colnames.empty(var.numericas(datos)))
    updateSelectizeInput(session, "select.var", choices = colnames.empty(var.numericas(datos)))
    updateSelectInput(session, "sel.distribucion.num", choices = colnames.empty(var.numericas(datos)))
    updateSelectInput(session, "sel.distribucion.cat", choices = colnames.empty(var.categoricas(datos)))
    updateSelectInput(session, "sel.resumen", choices = colnames.empty(datos))
    updateSelectInput(session, "sel.predic.var", choices = colnames.empty(var.categoricas(datos)))

    datos.prueba <<- NULL
    datos.aprendizaje <<- NULL
    variable.predecir <<- NULL
    #modelo.knn <<- NULL

    updateAceEditor(session, "fieldModelCor", value = modelo.cor())

    tryCatch({
      isolate(eval(parse(text = modelo.cor())))
      output$txtcor <- renderPrint(print(correlacion))
    },
    error = function(e) {
      return(datos <- NULL)
    })

  })

  #Cunado es precionado el boton de transformar datos
  observeEvent(input$transButton, {
    var.noactivas <- c()
    code.res <- "datos <<- datos.originales \n"
    for (var in colnames(datos.originales)) {
      if(input[[paste0("box", var)]]) {
        if(input[[paste0("sel", var)]] == "categorico" & class(datos.originales[, var]) %in% c("numeric","integer")) {
          code.res <- paste0(code.res, code.trans(var, "categorico"), "\n")
        }
        if(input[[paste0("sel", var)]] == "numerico" & !(class(datos.originales[, var]) %in% c("numeric","integer"))) {
          code.res <- paste0(code.res, code.trans(var, "numerico"), "\n")
        }
        if(input[[paste0("sel", var)]] == "disyuntivo"){
          code.res <- paste0(code.res, code.trans(var, "disyuntivo"), "\n")
        }
      } else {
        var.noactivas <- c(var.noactivas, var)
      }
    }

    isolate(eval(parse(text = code.res)))
    if(length(var.noactivas) > 0)
      isolate(eval(parse(text = code.desactivar(var.noactivas))))

    updateAceEditor(session, "fieldCodeTrans", value = code.res)

    updateSelectizeInput(session, "sel.normal", choices = colnames.empty(var.numericas(datos)))
    updateSelectizeInput(session, "select.var", choices = colnames.empty(var.numericas(datos)))
    updateSelectInput(session, "sel.distribucion.num", choices = colnames.empty(var.numericas(datos)))
    updateSelectInput(session, "sel.distribucion.cat", choices = colnames.empty(var.categoricas(datos)))
    updateSelectInput(session, "sel.resumen", choices = colnames.empty(datos))
    updateSelectInput(session, "sel.predic.var", choices = colnames.empty(var.categoricas(datos)))
    datos.prueba <<- NULL
    datos.aprendizaje <<- NULL
    variable.predecir <<- NULL
    #modelo.knn <<- NULL

    updateAceEditor(session, "fieldModelCor", value = modelo.cor())
    tryCatch({
      isolate(eval(parse(text = modelo.cor())))
      output$txtcor <- renderPrint(print(correlacion))
    }, error = function(e) {
      return(datos <- NULL)
    })
  })

  observeEvent(input$segmentButton,{
    if(input$sel.predic.var != ""){
      codigo <- particion.code("datos", input$segmentacionDatosA ,input$sel.predic.var)
      updateAceEditor(session, "fieldCodeSegment", value = codigo)
      isolate(eval(parse(text = codigo)))
      nombres <- colnames.empty(datos)
      updateSelectizeInput(session, "select.var.knn", choices = nombres[-which(nombres == variable.predecir)])
    }else{
      showNotification("Tiene que seleccionar una variable a predecir", duration = 15, type = "error")
    }
  })

  #Crea la tabla de datos que se muestra en la pagina de datos
  mostrarData <- eventReactive(c(input$loadButton, input$transButton), {
    nombre.columnas <- c("ID", colnames(datos))
    tipo.columnas <- c("", sapply(colnames(datos),
                                  function(i) ifelse(class(datos[,i]) %in% c("numeric", "integer"), "Numérico", "Categórico")))
    sketch <- htmltools::withTags(table(
      tableHeader(nombre.columnas),
      tableFooter(tipo.columnas)
    ))
    return(DT::datatable(datos, selection = 'none', editable = TRUE, container = sketch,extensions = c('Responsive','Buttons'),
                         options = list(dom = 'Bfrtip', buttons = list(list(extend = 'csv', filename = "datos", text = 'Descargar')))))
  })

  #Crea la tabla de datos de aprendizaje que se muestra en la pagina de datos
  mostrarDataAprendizaje <- eventReactive(c(input$loadButton, input$transButton, input$segmentButton), {
    nombre.columnas <- c("ID", colnames(datos.aprendizaje))
    tipo.columnas <- c("", sapply(colnames(datos.aprendizaje),
                                  function(i) ifelse(class(datos.aprendizaje[,i]) %in% c("numeric", "integer"), "Numérico", "Categórico")))
    sketch <- htmltools::withTags(table(
      tableHeader(nombre.columnas),
      tableFooter(tipo.columnas)
    ))
    return(DT::datatable(datos.aprendizaje, selection = 'none', editable = TRUE, container = sketch,extensions = c('Responsive','Buttons'),
                         options = list(dom = 'Bfrtip',pageLength = 5, buttons = list(list(extend = 'csv', filename = "datos.aprendizaje", text = 'Descargar')))))
  })

  #Crea la tabla de datos de prueba que se muestra en la pagina de datos
  mostrarDataPrueba <- eventReactive(c(input$loadButton, input$transButton, input$segmentButton), {
    nombre.columnas <- c("ID", colnames(datos.prueba))
    tipo.columnas <- c("", sapply(colnames(datos.prueba),
                                  function(i) ifelse(class(datos.prueba[,i]) %in% c("numeric", "integer"), "Numérico", "Categórico")))
    sketch <- htmltools::withTags(table(
      tableHeader(nombre.columnas),
      tableFooter(tipo.columnas)
    ))
    return(DT::datatable(datos.prueba, selection = 'none', editable = TRUE, container = sketch,extensions = c('Responsive','Buttons'),
                         options = list(dom = 'Bfrtip',pageLength = 5, buttons = list(list(extend = 'csv', filename = "datos.prueba", text = 'Descargar')))))
  })

  #Crea los select box del panel de trasnformar datos
  update.trans <- eventReactive(c(input$loadButton),{
    if(!is.null(datos) && ncol(datos) > 0){
      n <- ncol(datos)
      res <-  data.frame(Variables = colnames(datos), Tipo = c(1:n), Activa = c(1:n))
    }else{
      res <-  as.data.frame(NULL)
      showNotification("Tiene que cargar los datos", duration = 15, type = "error")
    }
    res$Tipo <- sapply(colnames(datos), function(i) paste0('<select id="sel', i, '"> <option value="categorico">Categórico</option>
                                                             <option value="numerico" ', ifelse(class(datos[, i]) %in% c("numeric","integer"), ' selected="selected"', ''),
                                                           '>Numérico</option> <option value="disyuntivo">Disyuntivo</option> </select>'))
    res$Activa <- sapply(colnames(datos), function(i) paste0('<input type="checkbox" id="box', i, '" checked/>'))
    return(res)
  })

  #Cuando es precionado el boton de cargar datos o de transformar datos
  #Se crea una tabla summary
  obj.resum <- eventReactive(c(input$loadButton, input$transButton), {
    data.frame(unclass(summary(datos)), check.names = FALSE, stringsAsFactors = FALSE)
  })

  #Cuando es precionado el boton de cargar datos, de transformar datos o se modifica el codigo
  #Hace el grafico la distribucion de normalidad
  obj.normal <- eventReactive(c(input$loadButton, input$transButton, input$fieldCodeNormal), {
    cod.normal <<- input$fieldCodeNormal
    isolate(eval(parse(text = cod.normal)))
  })

  #Cuando se seleccionan las variables o se cambia el color del test de normalidad
  #Hace el grafico la distribucion de normalidad
  observeEvent(c(input$sel.normal, input$col.normal), {
    cod.normal <<- default.normal(data = "datos", vars = input$sel.normal, color = input$col.normal)
    updateAceEditor(session, "fieldCodeNormal", value = cod.normal)
  })

  #Cuando es precionado el boton de cargar datos, de transformar datos o se modifica el codigo
  #Hace el grafico de dispersion
  obj.disp <- eventReactive(c(input$loadButton, input$transButton, input$fieldCodeDisp), {
    cod.disp <<- input$fieldCodeDisp
    isolate(eval(parse(text = cod.disp)))
  })

  #Cuando es precionado el boton de cargar datos, de transformar datos o se modifica el codigo
  #Hace el grafico de correlaciones
  obj.cor <- eventReactive(c(input$loadButton, input$transButton, input$fieldModelCor, input$fieldCodeCor), {
    cod.cor <<- input$fieldCodeCor
    return(isolate(eval(parse(text = cod.cor))))
  })

  #Cuando es precionado el boton de cargar datos, de transformar datos o se modifica el codigo
  #Hace el grafico de distribuciones numericas
  obj.dya.num <- eventReactive(c(input$loadButton, input$transButton, input$fieldFuncNum, input$fieldCodeNum), {
    cod.dya.num  <<- input$fieldCodeNum
    func.dya.num <<- input$fieldFuncNum
    isolate(eval(parse(text = func.dya.num)))
    return(isolate(eval(parse(text = cod.dya.num))))
  })

  #Cuando es precionado el boton de cargar datos, de transformar datos o se modifica el codigo
  #Hace el grafico de distribuciones categoricas
  obj.dya.cat <- eventReactive(c(input$loadButton, input$transButton, input$fieldFuncCat, input$fieldCodeCat), {
    cod.dya.cat  <<- input$fieldCodeCat
    func.dya.cat <<- input$fieldFuncCat
    isolate(eval(parse(text = func.dya.cat)))
    return(isolate(eval(parse(text = cod.dya.cat))))
  })


  #Cuando cambia la variable del grafico de distribucion numerica
  observeEvent(c(input$sel.distribucion.num, input$col.dist), {
    cod.dya.num <<- def.code.num(data = "datos", variable = paste0("'", input$sel.distribucion.num, "'"),
                                 color = paste0("'", input$col.dist, "'"))
    updateAceEditor(session, "fieldCodeNum", value = cod.dya.num)
  })

  #Cuando cambia la variable del grafico de distribucion categorica
  observeEvent(c(input$sel.distribucion.cat), {
    cod.dya.cat <<- def.code.cat(data = "datos", variable = paste0("'", input$sel.distribucion.cat, "'"))
    updateAceEditor(session, "fieldCodeCat", value = cod.dya.cat)
  })

  #Cuando cambia la variable seleccionada para el grafico de normalidad
  observeEvent(c(input$sel.normal), {
    cod.normal <<- default.normal(data = "datos", vars = input$sel.normal)
    updateAceEditor(session, "fieldCodeNormal", value = cod.normal)
  })

  #Cuando cambian las variables seleccionadas para el grafico de disperesion o el color
  observeEvent(c(input$select.var, input$col.disp), {
    cod.disp <<- default.disp(data = "datos", vars = input$select.var, color = input$col.disp)
    updateAceEditor(session, "fieldCodeDisp", value = cod.disp)
  })

  #Cuando cambian las opciones para el grafico de correlacion
  observeEvent(c(input$cor.metodo, input$cor.tipo), {
    cod.cor <<- correlaciones(metodo = input$cor.metodo, tipo = input$cor.tipo)
    updateAceEditor(session, "fieldCodeCor", value = cod.cor)
  })

  observeEvent(input$segmentacionDatosA,{
    updateSliderInput(session,"segmentacionDatosT",value = 100 - input$segmentacionDatosA)
  })

  observeEvent(input$segmentacionDatosT,{
    updateSliderInput(session,"segmentacionDatosA",value = 100 - input$segmentacionDatosT)

  })

  observeEvent(input$runKnn,{
    #Validaciones
    if(is.null(variable.predecir))
      showNotification("Tiene que seleccionar una variable a predecir",duration = 15, type = "error")
    if(is.null(datos))
      showNotification("Tiene que ingresar datos",duration = 15, type = "error")
    if(is.null(datos.aprendizaje))
      showNotification("Tiene que crear los datos de aprendizaje y de prueba",duration = 15, type = "error")

    if(!is.null(datos) & !is.null(variable.predecir) & !is.null(datos.aprendizaje)){ #Si se tiene los datos entonces :

      #Se genera el codigo del modelo
      codigo.knn <- kkn.modelo(variable.pr = variable.predecir,predictoras = input$select.var.knn,
                               scale = input$switch.scale.knn,
                               kmax = input$kmax.knn, kernel = input$kernel.knn)

      updateAceEditor(session, "fieldCodeKnn", value = codigo.knn)
    }
  })

  observeEvent(c(input$runKnn,input$fieldCodeKnn),{
    if(input$fieldCodeKnn != ""){
      tryCatch({ #Se corren los codigo
        isolate(eval(parse(text = input$fieldCodeKnn)))
        output$txtknn <- renderPrint(print(modelo.knn))

        #Se genera el codigo de la prediccion
        codigo.knn.pred <- kkn.prediccion()
        updateAceEditor(session, "fieldCodeKnnPred", value = codigo.knn.pred)
      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        modelo.knn <<- NULL
        MC.knn <<- NULL
        prediccion.knn <<- NULL
        showNotification("Error al ejecutar el modelo, intente nuevamente",duration = 15,type = "error")
      })
    }
  })

  observeEvent(c(input$runKnn,input$fieldCodeKnn,input$fieldCodeKnnPred),{
    if(input$fieldCodeKnnPred != ""){
      tryCatch({ #Se corren los codigo
        isolate(eval(parse(text = input$fieldCodeKnnPred)))

        #Cambia la tabla con la prediccion de knn
        output$knnPrediTable <- DT::renderDataTable(obj.knn.predic())

        # Se genera el codigo de la matriz
        codigo.knn.mc <- knn.MC(variable.predecir)
        updateAceEditor(session, "fieldCodeKnnMC", value = codigo.knn.mc)
      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        MC.knn <<- NULL
        prediccion.knn <<- NULL

        #Cambia la tabla con la prediccion de knn
        output$knnPrediTable <- DT::renderDataTable(obj.knn.predic())

        showNotification("Error al ejecutar la prediccion, intente nuevamente",duration = 15,type = "error")
      })
    }
  })

  observeEvent(c(input$runKnn,input$fieldCodeKnn,input$fieldCodeKnnPred,input$fieldCodeKnnMC),{
    if(input$fieldCodeKnnMC != ""){
      tryCatch({ #Se corren los codigo
        isolate(eval(parse(text = input$fieldCodeKnnMC)))
        output$txtknnMC <- renderPrint(print(MC.knn))

        isolate(eval(parse(text = plot.MC.code())))
        output$plot.knn.mc <- renderPlot(isolate(eval(parse(text = "plot.MC(MC.knn)" ))))

        output$knnPrecGlob <- renderGauge({ gauge(round(100 * (sum(diag(MC.knn))/ sum(MC.knn)),2), min = 0, max = 100, symbol = '%', label = "Precisión Global", gaugeSectors(
          success = c(70, 100), warning = c(50, 69), danger = c(0, 49) ) ) })
        output$knnErrorGlob <- renderGauge({ gauge(round(100 - (100 * (sum(diag(MC.knn))/ sum(MC.knn))),2), min = 0, max = 100, symbol = '%',label = "Error Global", gaugeSectors(
          success = c(0, 30), warning = c(31, 45), danger = c(46, 100))) })

      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        MC.knn <<- NULL
        output$plot.knn.mc <- renderPlot(isolate(eval(parse(text = "NULL" ))))
        showNotification("Error al ejecutar la matriz, intente nuevamente",duration = 15,type = "error")
      })
    }
  })

  observe({
    #Sys.setlocale("LC_ALL","en_US.UTF-8")
    updateAceEditor(session, "fieldCodeResum", value = cod.resum())

    updateAceEditor(session, "fieldFuncNum", value = func.dya.num)
    updateAceEditor(session, "fieldFuncCat", value = func.dya.cat)

    updateAceEditor(session, "fieldCodeReport", value = def.reporte())
  })


  ###########################################################################################################################
  ##### Funcionalidades
  ###########################################################################################################################

  #Cambia la tabla de datos
  output$contents <- DT::renderDT(mostrarData(),server = FALSE)

  #Cambia la tabla de datos
  output$contentsAprend <- DT::renderDT(mostrarDataAprendizaje(),server = FALSE)

  #Cambia la tabla de datos
  output$contentsPrueba <- DT::renderDT(mostrarDataPrueba(),server = FALSE)

  #Cambia la tabla de con las opciones del panel de transformar
  output$transData <- DT::renderDataTable(update.trans(), escape = FALSE, selection = 'none', server = FALSE,
                                         options = list(dom = 't', paging = FALSE, ordering = FALSE), rownames = F,
                                         callback = JS("table.rows().every(function(i, tab, row) {
                                                       var $this = $(this.node());
                                                       $this.attr('id', this.data()[0]);
                                                       $this.addClass('shiny-input-checkbox');});
                                                       Shiny.unbindAll(table.table().node());
                                                       Shiny.bindAll(table.table().node());"))

  #Cambia la tabla con el summary en la pagina de resumen
  output$resumen.completo <- DT::renderDataTable(obj.resum(), options = list(dom = 'ft', scrollX = TRUE), rownames = F)

  #Cambia los cuadros de summary por varibale
  output$resumen <- renderUI({
    if(input$sel.resumen %in% colnames(var.numericas(datos))){
      HTML(resumen.numerico(datos, input$sel.resumen))
    } else {
      HTML(resumen.categorico(datos, input$sel.resumen))
    }
  })

  #Cambia la tabla de valores atipicos en la pagina de distribuciones
  output$mostrar.atipicos <- DT::renderDataTable({
    atipicos <- boxplot.stats(datos[, input$sel.distribucion.num])
    datos <- datos[datos[, input$sel.distribucion.num] %in% atipicos$out, input$sel.distribucion.num, drop = F]
    return(datos[order(datos[, input$sel.distribucion.num]), , drop = F])
  }, options = list(dom = 't', scrollX = TRUE, scrollY = "10vh"))

  #Hace el grafico de la pagina de test de normalidad
  output$plot.normal <- renderPlot(obj.normal())

  #Hace el grafico de la pagina de dispersion
  output$plot.disp <- renderPlot(obj.disp())

  #Hace el grafico de correlaciones
  output$plot.cor <- renderPlot(obj.cor())

  #Hace el grafico de distribuciones numericas
  output$plot.num <- renderPlot(obj.dya.num())

  #Hace el grafico de distribuciones categoricas
  output$plot.cat <- renderPlot(obj.dya.cat())

  #Muestra el documento del reporte
  output$knitDoc <- renderUI({
    c(input$fieldCodeReport)
    return(isolate(HTML(knit2html(text = input$fieldCodeReport, fragment.only = T, quiet = T))))
  })

  #Descarga el reporte
  output$descargar <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.zip', sep='')
    },
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;

      namermd <- paste('data-', Sys.Date(), '.rmd', sep='')
      writeLines(input$fieldCodeReport, namermd)
      files <- c(namermd, files)

      src <- normalizePath(namermd)
      out <- rmarkdown::render(src,  params = NULL, rmarkdown::html_document())
      file.rename(out, paste('data-', Sys.Date(), '.html', sep=''))
      files <- c(paste('data-', Sys.Date(), '.html', sep=''), files)

      zip(file, files)
    }
  )

  obj.knn.predic <- function() {
    real <- as.character(datos.prueba[,variable.predecir])
    predi <- as.character(prediccion.knn)
    df <- cbind(real,predi,ifelse(real == predi,
                                  rep("<span style='color:green'><b>ACIERTO</b></span>",length(real)),
                                  rep("<span style='color:red'><b>FALLO</b></span>",length(real))))
    colnames(df) <- c( "Datos Reales", "Predicción", " ")
    sketch <- htmltools::withTags(table(
      tableHeader(c("Datos Reales", "Predicción", " "))
    ))
    return(DT::datatable(df, selection = 'none', editable = FALSE,escape = FALSE, container = sketch,
                         extensions = c('Responsive'),
                         options = list(dom = 'frtip',pageLength = 15) ))
  }


}) ## FIN SERVER




