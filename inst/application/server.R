
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

  # -------------------  Pagina Datos ------------------------ #

  #Cunado es precionado el boton de cargar datos
  observeEvent(input$loadButton, {
    codigo <- code.carga(nombre.filas = input$rowname, ruta = input$file1$datapath,
                         separador = input$sep, sep.decimal = input$dec, encabezado = input$header)

    tryCatch({
      isolate(eval(parse(text = codigo)))
    },
    error = function(e) {
      showNotification("Error al cargar los datos, intente nuevamente", duration = 15, type = "error")
      datos <<- NULL
      datos.originales <<- NULL
      return(NULL)
    })

    if(any(is.na(datos))){
      codigo.na <- code.NA(deleteNA = input$deleteNA)
      tryCatch({
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

    updateAceEditor(session, "fieldCodeData", value = paste0(codigo, "\n", codigo.na))

    updateSelectizeInput(session, "sel.normal", choices = colnames.empty(var.numericas(datos)))
    updateSelectizeInput(session, "select.var", choices = colnames.empty(var.numericas(datos)))
    updateSelectInput(session, "sel.distribucion.num", choices = colnames.empty(var.numericas(datos)))
    updateSelectInput(session, "sel.distribucion.cat", choices = colnames.empty(var.categoricas(datos)))
    updateSelectInput(session, "sel.resumen", choices = colnames.empty(datos))
    updateSelectInput(session, "sel.predic.var", choices = colnames.empty(var.categoricas(datos)))

    datos.prueba <<- NULL
    datos.aprendizaje <<- NULL
    variable.predecir <<- NULL

    updateAceEditor(session, "fieldModelCor", value = modelo.cor())

    tryCatch({
      isolate(eval(parse(text = modelo.cor())))
      output$txtcor <- renderPrint(print(correlacion))
    },
    error = function(e) {
      return(datos <- NULL)
    })

    close.menu("datos", is.null(datos))
    close.menu("aprendizaje", is.null(datos.aprendizaje))

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

    updateAceEditor(session, "fieldModelCor", value = modelo.cor())
    tryCatch({
      isolate(eval(parse(text = modelo.cor())))
      output$txtcor <- renderPrint(print(correlacion))
    }, error = function(e) {
      return(datos <- NULL)
    })

    close.menu("datos", is.null(datos))
    close.menu("aprendizaje", is.null(datos.aprendizaje))
  })

  #Segmenta los datos en aprendizaje y prueba
  observeEvent(input$segmentButton,{
    if(input$sel.predic.var != ""){
      codigo <- particion.code("datos", input$segmentacionDatosA ,input$sel.predic.var, input$semilla, input$permitir.semilla)
      updateAceEditor(session, "fieldCodeSegment", value = codigo)
      isolate(eval(parse(text = codigo)))
      nombres <- colnames.empty(datos)

      nambres.sin.pred <- nombres[-which(nombres == variable.predecir)]
      updateSelectizeInput(session, "select.var.knn", choices = nambres.sin.pred)
      updateSelectizeInput(session, "select.var.bayes", choices = nambres.sin.pred)
      updateSelectizeInput(session, "select.var.svm", choices = nambres.sin.pred)
      updateSelectizeInput(session, "select.var.svm.plot", choices = nambres.sin.pred)
      updateSelectizeInput(session, "select.var.dt", choices = nambres.sin.pred)
      updateSelectizeInput(session, "select.var.rf", choices = nambres.sin.pred)
      updateSelectizeInput(session, "select.var.boosting", choices = nambres.sin.pred)
    }else{
      showNotification("Tiene que seleccionar una variable a predecir", duration = 15, type = "error")
    }

    close.menu("aprendizaje", is.null(datos.aprendizaje))
  })

  #Habilitada o deshabilitada la semilla
  observeEvent(input$permitir.semilla,{
    if(input$permitir.semilla)
      shinyjs::enable("semilla")
    else
      shinyjs::disable("semilla")
  })

  #Cuando cambia la barra de proporcion de datos de prueba (Segmentar Datos)
  observeEvent(input$segmentacionDatosA,{
    updateSliderInput(session,"segmentacionDatosT",value = 100 - input$segmentacionDatosA)
  })

  #Cuando cambia la barra de proporcion de datos de aprendizaje (Segmentar Datos)
  observeEvent(input$segmentacionDatosT,{
    updateSliderInput(session,"segmentacionDatosA",value = 100 - input$segmentacionDatosT)

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

  # Al inicio de la sesion
  observe({
    updateAceEditor(session, "fieldCodeResum", value = cod.resum())

    updateAceEditor(session, "fieldFuncNum", value = func.dya.num)
    updateAceEditor(session, "fieldFuncCat", value = func.dya.cat)

    updateAceEditor(session, "fieldCodeReport", value = def.reporte())

    shinyjs::disable(selector = 'a[href^="#shiny-tab-parte1"]')
    shinyjs::disable(selector = 'a[href^="#shiny-tab-parte2"]')
  })

  # -------------------  Estadisticas Basicas ------------------------ #

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


  # -------------------  KNN ------------------------ #

  #Cuando se genera el modelo knn
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

  #Cuando se cambia el codigo del modelo  knn
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
        output$txtknn <- ""
        showNotification("Error al ejecutar el modelo, intente nuevamente",duration = 15,type = "error")
      })
    }
  })

  #Cuando se cambia el codigo de la prediccion de knn
  observeEvent(c(input$runKnn,input$fieldCodeKnn,input$fieldCodeKnnPred),{
    if(input$fieldCodeKnnPred != ""){
      tryCatch({ #Se corren los codigo
        isolate(eval(parse(text = input$fieldCodeKnnPred)))

        #Cambia la tabla con la prediccion de knn
        output$knnPrediTable <- DT::renderDataTable(obj.predic(prediccion.knn))

        # Se genera el codigo de la matriz
        codigo.knn.mc <- knn.MC(variable.predecir)
        updateAceEditor(session, "fieldCodeKnnMC", value = codigo.knn.mc)
      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        MC.knn <<- NULL
        prediccion.knn <<- NULL

        #Cambia la tabla con la prediccion de knn
        output$knnPrediTable <- DT::renderDataTable(obj.predic(prediccion.knn))

        showNotification("Error al ejecutar la prediccion, intente nuevamente",duration = 15,type = "error")
      })
    }
  })

  #Cuando se cambia el codigo de la matriz de confucion de knn
  observeEvent(c(input$runKnn,input$fieldCodeKnn,input$fieldCodeKnnPred,input$fieldCodeKnnMC),{
    if(input$fieldCodeKnnMC != ""){
      tryCatch({ #Se corren los codigo
        isolate(eval(parse(text = input$fieldCodeKnnMC)))
        output$txtknnMC <- renderPrint(print(MC.knn))

        isolate(eval(parse(text = plot.MC.code())))
        output$plot.knn.mc <- renderPlot(isolate(eval(parse(text = "plot.MC(MC.knn)" ))))

        # Se genera el codigo de la indices
        codigo.indices <- cod.indices()
        updateAceEditor(session, "fieldCodeKnnIG", value = codigo.indices)
      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        MC.knn <<- NULL
        indices.knn <<- rep(0,8)
        output$plot.knn.mc <- renderPlot(isolate(eval(parse(text = "NULL" ))))
        output$txtknnMC <- ""
        showNotification("Error al ejecutar la matriz, intente nuevamente",duration = 15,type = "error")
      })
    }
  })

  #Cuando se cambia el codigo de los indices de knn
  observeEvent(c(input$runKnn,input$fieldCodeKnn,input$fieldCodeKnnPred,input$fieldCodeKnnMC,input$fieldCodeKnnIG),{
    if(input$fieldCodeKnnIG != ""){
      tryCatch({ #Se corren los codigo
        isolate(eval(parse(text = input$fieldCodeKnnIG)))

        indices.knn <<- indices.generales(MC.knn)

        if(ncol(MC.knn) > 2){
          shinyjs::show("knnPrecGlob")
          shinyjs::show("knnErrorGlob")
          shinyjs::hide("knnPrecP")
          shinyjs::hide("knnPrecN")
          shinyjs::hide("knnFalP")
          shinyjs::hide("knnFalN")
          shinyjs::hide("knnAserP")
          shinyjs::hide("knnAserN")
        }else{
          shinyjs::show("knnPrecGlob")
          shinyjs::show("knnErrorGlob")
          shinyjs::show("knnPrecP")
          shinyjs::show("knnPrecN")
          shinyjs::show("knnFalP")
          shinyjs::show("knnFalN")
          shinyjs::show("knnAserP")
          shinyjs::show("knnAserN")
        }

        output$knnPrecGlob <- renderGauge({
          gauge(round(indices.knn[[1]],2),
                min = 0, max = 100, symbol = '%',
                label = "Precisión Global",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

        output$knnErrorGlob <- renderGauge({
          gauge(round(indices.knn[[2]],2),
                min = 0, max = 100, symbol = '%',
                label = "Error Global",
                gaugeSectors( success = c(0, 30), warning = c(31, 45), danger = c(46, 100)))
        })

        output$knnPrecP <- renderGauge({
          gauge(round(indices.knn[[3]],2) ,
                min = 0, max = 100, symbol = '%',
                label = "Precisión Positiva",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

        output$knnPrecN <- renderGauge({
          gauge(round(indices.knn[[4]],2), min = 0, max = 100,
                 symbol = '%',
                 label = "Precisión Negativa",
                 gaugeSectors(success = c(70, 100),
                              warning = c(50, 69),
                              danger = c(0, 49)))
        })

        output$knnFalP <- renderGauge({
          gauge(round(indices.knn[[5]],2), min = 0, max = 100, symbol = '%',
                 label = "Falsos Positivos",
                 gaugeSectors( success = c(0, 30),
                               warning = c(31, 45),
                               danger = c(46, 100)))
        })

        output$knnFalN <- renderGauge({
          gauge(round(indices.knn[[6]],2), min = 0, max = 100, symbol = '%',
                 label = "Falsos Negativos",
                 gaugeSectors( success = c(0, 30),
                               warning = c(31, 45),
                               danger = c(46, 100)))
        })

        output$knnAserP <- renderGauge({
          gauge(round(indices.knn[[7]],2), min = 0, max = 100, symbol = '%',
                 label = "Asertividad Positiva",
                 gaugeSectors(success = c(70, 100),
                              warning = c(50, 69),
                              danger = c(0, 49)))
        })

        output$knnAserN <- renderGauge({
          gauge(round(indices.knn[[8]],2), min = 0, max = 100, symbol = '%',
                 label = "Asertividad Negativa",
                 gaugeSectors(success = c(70, 100),
                              warning = c(50, 69),
                              danger = c(0, 49)))
        })

      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        indices.knn <<- rep(0,8)
        showNotification("Error al ejecutar los indices, intente nuevamente",duration = 15,type = "error")
      })
    }else{
      shinyjs::hide("knnPrecGlob")
      shinyjs::hide("knnErrorGlob")
      shinyjs::hide("knnPrecP")
      shinyjs::hide("knnPrecN")
      shinyjs::hide("knnFalP")
      shinyjs::hide("knnFalN")
      shinyjs::hide("knnAserP")
      shinyjs::hide("knnAserN")
    }
  })

  # -------------------  Bayes ------------------------ #

  #Cuando se genera el modelo bayes
  observeEvent(input$runBayes,{
    #Validaciones
    if(is.null(variable.predecir))
      showNotification("Tiene que seleccionar una variable a predecir",duration = 15, type = "error")
    if(is.null(datos))
      showNotification("Tiene que ingresar datos",duration = 15, type = "error")
    if(is.null(datos.aprendizaje))
      showNotification("Tiene que crear los datos de aprendizaje y de prueba",duration = 15, type = "error")

    if(!is.null(datos) & !is.null(variable.predecir) & !is.null(datos.aprendizaje)){ #Si se tiene los datos entonces :

      #Se genera el codigo del modelo
      codigo.bayes <- bayes.modelo(variable.pr = variable.predecir,
                                   predictoras = input$select.var.bayes)

      updateAceEditor(session, "fieldCodeBayes", value = codigo.bayes)
    }
  })

  #Cuando se cambia el codigo del modelo  bayes
  observeEvent(c(input$runBayes,input$fieldCodeBayes),{
    if(input$fieldCodeBayes != ""){
      tryCatch({ #Se corren los codigo
        isolate(eval(parse(text = input$fieldCodeBayes)))
        output$txtbayes <- renderPrint(print(modelo.bayes))

        #Se genera el codigo de la prediccion
        codigo.bayes.pred <- bayes.prediccion(variable.predecir)
        updateAceEditor(session, "fieldCodeBayesPred", value = codigo.bayes.pred)
      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        modelo.bayes <<- NULL
        MC.bayes <<- NULL
        prediccion.bayes <<- NULL
        showNotification("Error al ejecutar el modelo, intente nuevamente",duration = 15,type = "error")
      })
    }
  })

  #Cuando se cambia el codigo de la prediccion de bayes
  observeEvent(c(input$runBayes,input$fieldCodeBayes,input$fieldCodeBayesPred),{
    if(input$fieldCodeBayesPred != ""){
      tryCatch({ #Se corren los codigo
        isolate(eval(parse(text = input$fieldCodeBayesPred)))

        #Cambia la tabla con la prediccion de bayes
        output$bayesPrediTable <- DT::renderDataTable(obj.predic(prediccion.bayes))

        # Se genera el codigo de la matriz
        codigo.bayes.mc <- bayes.MC(variable.predecir)
        updateAceEditor(session, "fieldCodeBayesMC", value = codigo.bayes.mc)
      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        MC.bayes <<- NULL
        prediccion.bayes <<- NULL

        #Cambia la tabla con la prediccion de bayes
        output$bayesPrediTable <- DT::renderDataTable(obj.predic(prediccion.bayes))

        showNotification("Error al ejecutar la prediccion, intente nuevamente",duration = 15,type = "error")
      })
    }
  })

  #Cuando se cambia el codigo de la matriz de confucion de bayes
  observeEvent(c(input$runBayes,input$fieldCodeBayes,input$fieldCodeBayesPred,input$fieldCodeBayesMC),{
    if(input$fieldCodeBayesMC != ""){
      tryCatch({ #Se corren los codigo
        isolate(eval(parse(text = input$fieldCodeBayesMC)))
        output$txtBayesMC <- renderPrint(print(MC.bayes))

        isolate(eval(parse(text = plot.MC.code())))
        output$plot.bayes.mc <- renderPlot(isolate(eval(parse(text = "plot.MC(MC.bayes)" ))))

        # Se genera el codigo de la indices
        codigo.indices <- cod.indices()
        updateAceEditor(session, "fieldCodeBayesIG", value = codigo.indices)
      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        MC.bayes <<- NULL
        indices.bayes <<- rep(0,8)
        output$plot.bayes.mc <- renderPlot(isolate(eval(parse(text = "NULL" ))))
        showNotification("Error al ejecutar la matriz, intente nuevamente",duration = 15,type = "error")
      })
    }
  })

  #Cuando se cambia el codigo de los indices de bayes
  observeEvent(c(input$runBayes,input$fieldCodeBayes,input$fieldCodeBayesPred,input$fieldCodeBayesMC,input$fieldCodeBayesIG),{
    if(input$fieldCodeBayesIG != ""){
      tryCatch({ #Se corren los codigo
        isolate(eval(parse(text = input$fieldCodeBayesIG)))

        indices.bayes <<- indices.generales(MC.bayes)

        if(ncol(MC.bayes) > 2){
          shinyjs::hide("bayesPrecP")
          shinyjs::hide("bayesPrecN")
          shinyjs::hide("bayesFalP")
          shinyjs::hide("bayesFalN")
          shinyjs::hide("bayesAserP")
          shinyjs::hide("bayesAserN")
        }else{
          shinyjs::show("bayesPrecP")
          shinyjs::show("bayesPrecN")
          shinyjs::show("bayesFalP")
          shinyjs::show("bayesFalN")
          shinyjs::show("bayesAserP")
          shinyjs::show("bayesAserN")
        }

        output$bayesPrecGlob <- renderGauge({
          gauge(round(indices.bayes[[1]],2),
                min = 0, max = 100, symbol = '%',
                label = "Precisión Global",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

        output$bayesErrorGlob <- renderGauge({
          gauge(round(indices.bayes[[2]],2),
                min = 0, max = 100, symbol = '%',
                label = "Error Global",
                gaugeSectors( success = c(0, 30), warning = c(31, 45), danger = c(46, 100)))
        })

        output$bayesPrecP <- renderGauge({
          gauge(round(indices.bayes[[3]],2) ,
                min = 0, max = 100, symbol = '%',
                label = "Precisión Positiva",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

        output$bayesPrecN <- renderGauge({
          gauge(round(indices.bayes[[4]],2), min = 0, max = 100,
                symbol = '%',
                label = "Precisión Negativa",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

        output$bayesFalP <- renderGauge({
          gauge(round(indices.bayes[[5]],2), min = 0, max = 100, symbol = '%',
                label = "Falsos Positivos",
                gaugeSectors( success = c(0, 30),
                              warning = c(31, 45),
                              danger = c(46, 100)))
        })

        output$bayesFalN <- renderGauge({
          gauge(round(indices.bayes[[6]],2), min = 0, max = 100, symbol = '%',
                label = "Falsos Negativos",
                gaugeSectors( success = c(0, 30),
                              warning = c(31, 45),
                              danger = c(46, 100)))
        })

        output$bayesAserP <- renderGauge({
          gauge(round(indices.bayes[[7]],2), min = 0, max = 100, symbol = '%',
                label = "Asertividad Positiva",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

        output$bayesAserN <- renderGauge({
          gauge(round(indices.bayes[[8]],2), min = 0, max = 100, symbol = '%',
                label = "Asertividad Negativa",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        indices.bayes <<- rep(0,8)
        showNotification("Error al ejecutar los indices, intente nuevamente",duration = 15,type = "error")
      })
    }
  })


  # -------------------  SVM ------------------------ #

  #Cuando se genera el modelo svm
  observeEvent(input$runSvm,{
    #Validaciones
    if(is.null(variable.predecir))
      showNotification("Tiene que seleccionar una variable a predecir",duration = 15, type = "error")
    if(is.null(datos))
      showNotification("Tiene que ingresar datos",duration = 15, type = "error")
    if(is.null(datos.aprendizaje))
      showNotification("Tiene que crear los datos de aprendizaje y de prueba",duration = 15, type = "error")

    if(!is.null(datos) & !is.null(variable.predecir) & !is.null(datos.aprendizaje)){ #Si se tiene los datos entonces :

      #Se genera el codigo del modelo
      codigo.svm <- svm.modelo(variable.pr = variable.predecir, predictoras = input$select.var.svm,
                               scale = input$switch.scale.svm,
                               kernel = input$kernel.svm)

      updateAceEditor(session, "fieldCodeSvm", value = codigo.svm)
    }
  })

  #Cuando se cambia el codigo del modelo svm
  observeEvent(c(input$runSvm,input$fieldCodeSvm),{
    if(input$fieldCodeSvm != ""){
      tryCatch({ #Se corren los codigo
        isolate(eval(parse(text = input$fieldCodeSvm)))
        output$txtSvm <- renderPrint(print(modelo.svm))

        #Acutaliza el codigo del grafico de clasificacion svm
        updateAceEditor(session, "fieldCodeSvmPlot", value = svm.plot(NULL))

        #Se genera el codigo de la prediccion
        codigo.svm.pred <- svm.prediccion()
        updateAceEditor(session, "fieldCodeSvmPred", value = codigo.svm.pred)
      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        modelo.svm <<- NULL
        MC.svm <<- NULL
        prediccion.svm <<- NULL
        showNotification("Error al ejecutar el modelo, intente nuevamente",duration = 15,type = "error")
      })
    }
  })

  #Cuando se cambia el codigo de la prediccion de svm
  observeEvent(c(input$runSvm,input$fieldCodeSvm,input$fieldCodeSvmPred),{
    if(input$fieldCodeSvmPred != ""){
      tryCatch({ #Se corren los codigo
        isolate(eval(parse(text = input$fieldCodeSvmPred)))

        #Cambia la tabla con la prediccion de knn
        output$svmPrediTable <- DT::renderDataTable(obj.predic(prediccion.svm))

        # Se genera el codigo de la matriz
        codigo.svm.mc <- svm.MC(variable.predecir)
        updateAceEditor(session, "fieldCodeSvmMC", value = codigo.svm.mc)
      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        MC.svm <<- NULL
        prediccion.svm <<- NULL

        #Cambia la tabla con la prediccion de knn
        output$svmPrediTable <- DT::renderDataTable(obj.predic(prediccion.svm))

        showNotification("Error al ejecutar la prediccion, intente nuevamente",duration = 15,type = "error")
      })
    }
  })

  #cuando cambia el codigo del grafico de clasificacion svm
  svm.graf <- eventReactive(c(input$runSvm, input$fieldCodeSvmPlot,input$select.var.svm.plot),{
    if(length(input$select.var.svm.plot) == 2){
      v <- colnames(datos)
      v <- v[v != variable.predecir]
      v <- v[!(v %in% input$select.var.svm.plot)]
      if(length(v) == 0)
        v <- input$select.var.svm.plot
      updateAceEditor(session, "fieldCodeSvmPlot", value = svm.plot(input$select.var.svm.plot, v))
      return(isolate(eval(parse(text = input$fieldCodeSvmPlot ))))
    }else{
      updateAceEditor(session, "fieldCodeSvmPlot", value = "")
      return(isolate(eval(parse(text = "NULL" ))))
    }
  })

  output$plot.svm <- renderPlot({svm.graf()})

  #Cuando se cambia el codigo de la matriz de confucion de knn
  observeEvent(c(input$runSvm,input$fieldCodeSvm,input$fieldCodeSvmPred,input$fieldCodeSvmMC),{
    if(input$fieldCodeSvmMC != ""){
      tryCatch({ #Se corren los codigo
        isolate(eval(parse(text = input$fieldCodeSvmMC)))
        output$txtSvmMC <- renderPrint(print(MC.svm))

        isolate(eval(parse(text = plot.MC.code())))
        output$plot.svm.mc <- renderPlot(isolate(eval(parse(text = "plot.MC(MC.svm)" ))))

        # Se genera el codigo de la indices
        codigo.indices <- cod.indices()
        updateAceEditor(session, "fieldCodeSvmIG", value = codigo.indices)
      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        MC.svm <<- NULL
        indices.svm <<- rep(0,8)
        output$plot.svm.mc <- renderPlot(isolate(eval(parse(text = "NULL" ))))
        showNotification("Error al ejecutar la matriz, intente nuevamente",duration = 15,type = "error")
      })
    }
  })

  #Cuando se cambia el codigo de los indices de knn
  observeEvent(c(input$runSvm,input$fieldCodeSvm,input$fieldCodeSvmPred,input$fieldCodeSvmMC,input$fieldCodeSvmIG),{
    if(input$fieldCodeSvmIG != ""){
      tryCatch({ #Se corren los codigo
        isolate(eval(parse(text = input$fieldCodeSvmIG)))

        indices.svm <<- indices.generales(MC.svm)

        if(ncol(MC.svm) > 2){
          shinyjs::hide("svmPrecP")
          shinyjs::hide("svmPrecN")
          shinyjs::hide("svmFalP")
          shinyjs::hide("svmFalN")
          shinyjs::hide("svmAserP")
          shinyjs::hide("svmAserN")
        }else{
          shinyjs::show("svmPrecP")
          shinyjs::show("svmPrecN")
          shinyjs::show("svmFalP")
          shinyjs::show("svmFalN")
          shinyjs::show("svmAserP")
          shinyjs::show("svmAserN")
        }

        output$svmPrecGlob <- renderGauge({
          gauge(round(indices.svm[[1]],2),
                min = 0, max = 100, symbol = '%',
                label = "Precisión Global",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

        output$svmErrorGlob <- renderGauge({
          gauge(round(indices.svm[[2]],2),
                min = 0, max = 100, symbol = '%',
                label = "Error Global",
                gaugeSectors( success = c(0, 30), warning = c(31, 45), danger = c(46, 100)))
        })

        output$svmPrecP <- renderGauge({
          gauge(round(indices.svm[[3]],2) ,
                min = 0, max = 100, symbol = '%',
                label = "Precisión Positiva",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

        output$svmPrecN <- renderGauge({
          gauge(round(indices.svm[[4]],2), min = 0, max = 100,
                symbol = '%',
                label = "Precisión Negativa",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

        output$svmFalP <- renderGauge({
          gauge(round(indices.svm[[5]],2), min = 0, max = 100, symbol = '%',
                label = "Falsos Positivos",
                gaugeSectors( success = c(0, 30),
                              warning = c(31, 45),
                              danger = c(46, 100)))
        })

        output$svmFalN <- renderGauge({
          gauge(round(indices.svm[[6]],2), min = 0, max = 100, symbol = '%',
                label = "Falsos Negativos",
                gaugeSectors( success = c(0, 30),
                              warning = c(31, 45),
                              danger = c(46, 100)))
        })

        output$svmAserP <- renderGauge({
          gauge(round(indices.svm[[7]],2), min = 0, max = 100, symbol = '%',
                label = "Asertividad Positiva",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

        output$svmAserN <- renderGauge({
          gauge(round(indices.svm[[8]],2), min = 0, max = 100, symbol = '%',
                label = "Asertividad Negativa",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        indices.svm <<- rep(0,8)
        showNotification("Error al ejecutar los indices, intente nuevamente",duration = 15,type = "error")
      })
    }
  })

  # -------------------  DT ------------------------ #

  #Cuando se genera el modelo dt
  observeEvent(input$runDt,{
    #Validaciones
    if(is.null(variable.predecir))
      showNotification("Tiene que seleccionar una variable a predecir",duration = 15, type = "error")
    if(is.null(datos))
      showNotification("Tiene que ingresar datos",duration = 15, type = "error")
    if(is.null(datos.aprendizaje))
      showNotification("Tiene que crear los datos de aprendizaje y de prueba",duration = 15, type = "error")

    if(!is.null(datos) & !is.null(variable.predecir) & !is.null(datos.aprendizaje)){ #Si se tiene los datos entonces :

      #Se genera el codigo del modelo
      codigo.dt <- dt.modelo( variable.pr = variable.predecir,
                              predictoras = input$select.var.dt,
                              minsplit = input$minsplit.dt)

      updateAceEditor(session, "fieldCodeDt", value = codigo.dt)
    }
  })

  #Cuando se cambia el codigo del modelo dt
  observeEvent(c(input$runDt,input$fieldCodeDt),{
    if(input$fieldCodeDt != ""){
      tryCatch({ #Se corren los codigo
        isolate(eval(parse(text = input$fieldCodeDt)))
        output$txtDt <- renderPrint(print(modelo.dt))

        #Cambia el codigo del grafico del árbol
        updateAceEditor(session, "fieldCodeDtPlot", value = dt.plot())
        output$plot.dt <- renderPlot(isolate(eval(parse(text = input$fieldCodeDtPlot ))))

        #Se genera el codigo de la prediccion
        codigo.dt.pred <- dt.prediccion()
        updateAceEditor(session, "fieldCodeDtPred", value = codigo.dt.pred)
      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        output$plot.dt <- renderPlot(renderPlot(isolate(eval(parse(text = "NULL" )))))
        modelo.dt <<- NULL
        MC.dt <<- NULL
        prediccion.dt <<- NULL
        showNotification("Error al ejecutar el modelo, intente nuevamente",duration = 15,type = "error")
      })
    }
  })

  #Cuando se cambia el codigo de la prediccion de dt
  observeEvent(c(input$runDt,input$fieldCodeDt,input$fieldCodeDtPred),{
    if(input$fieldCodeDtPred != ""){
      tryCatch({ #Se corren los codigo
        isolate(eval(parse(text = input$fieldCodeDtPred)))

        #Cambia la tabla con la prediccion de dt
        output$dtPrediTable <- DT::renderDataTable(obj.predic(prediccion.dt))

        # Se genera el codigo de la matriz
        codigo.dt.mc <- dt.MC(variable.predecir)
        updateAceEditor(session, "fieldCodeDtMC", value = codigo.dt.mc)
      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        MC.dt <<- NULL
        prediccion.dt <<- NULL

        #Cambia la tabla con la prediccion de knn
        output$dtPrediTable <- DT::renderDataTable(obj.predic(prediccion.dt))

        showNotification("Error al ejecutar la prediccion, intente nuevamente",duration = 15,type = "error")
      })
    }
  })

  #Cuando se cambia el codigo de la matriz de confucion de dt
  observeEvent(c(input$runDt,input$fieldCodeDt,input$fieldCodeDtPred,input$fieldCodeDtMC),{
    if(input$fieldCodeDtMC != ""){
      tryCatch({ #Se corren los codigo
        isolate(eval(parse(text = input$fieldCodeDtMC)))
        output$txtDtMC <- renderPrint(print(MC.dt))

        isolate(eval(parse(text = plot.MC.code())))
        output$plot.dt.mc <- renderPlot(isolate(eval(parse(text = "plot.MC(MC.dt)" ))))

        # Se genera el codigo de la indices
        codigo.indices <- cod.indices()
        updateAceEditor(session, "fieldCodeDtIG", value = codigo.indices)
      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        MC.dt <<- NULL
        indices.dt <<- rep(0,8)
        output$plot.dt.mc <- renderPlot(isolate(eval(parse(text = "NULL" ))))
        showNotification("Error al ejecutar la matriz, intente nuevamente",duration = 15,type = "error")
      })
    }
  })

  #Cuando se cambia el codigo de los indices de dt
  observeEvent(c(input$runDt,input$fieldCodeDt,input$fieldCodeDtPred,input$fieldCodeDtMC,input$fieldCodeDtIG),{
    if(input$fieldCodeDtIG != ""){
      tryCatch({ #Se corren los codigo
        isolate(eval(parse(text = input$fieldCodeDtIG)))

        indices.dt <<- indices.generales(MC.dt)

        if(ncol(MC.dt) > 2){
          shinyjs::hide("dtPrecP")
          shinyjs::hide("dtPrecN")
          shinyjs::hide("dtFalP")
          shinyjs::hide("dtFalN")
          shinyjs::hide("dtAserP")
          shinyjs::hide("dtAserN")
        }else{
          shinyjs::show("dtPrecP")
          shinyjs::show("dtPrecN")
          shinyjs::show("dtFalP")
          shinyjs::show("dtFalN")
          shinyjs::show("dtAserP")
          shinyjs::show("dtAserN")
        }

        output$dtPrecGlob <- renderGauge({
          gauge(round(indices.dt[[1]],2),
                min = 0, max = 100, symbol = '%',
                label = "Precisión Global",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

        output$dtErrorGlob <- renderGauge({
          gauge(round(indices.dt[[2]],2),
                min = 0, max = 100, symbol = '%',
                label = "Error Global",
                gaugeSectors( success = c(0, 30), warning = c(31, 45), danger = c(46, 100)))
        })

        output$dtPrecP <- renderGauge({
          gauge(round(indices.dt[[3]],2) ,
                min = 0, max = 100, symbol = '%',
                label = "Precisión Positiva",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

        output$dtPrecN <- renderGauge({
          gauge(round(indices.dt[[4]],2), min = 0, max = 100,
                symbol = '%',
                label = "Precisión Negativa",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

        output$dtFalP <- renderGauge({
          gauge(round(indices.dt[[5]],2), min = 0, max = 100, symbol = '%',
                label = "Falsos Positivos",
                gaugeSectors( success = c(0, 30),
                              warning = c(31, 45),
                              danger = c(46, 100)))
        })

        output$dtFalN <- renderGauge({
          gauge(round(indices.dt[[6]],2), min = 0, max = 100, symbol = '%',
                label = "Falsos Negativos",
                gaugeSectors( success = c(0, 30),
                              warning = c(31, 45),
                              danger = c(46, 100)))
        })

        output$dtAserP <- renderGauge({
          gauge(round(indices.dt[[7]],2), min = 0, max = 100, symbol = '%',
                label = "Asertividad Positiva",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

        output$dtAserN <- renderGauge({
          gauge(round(indices.dt[[8]],2), min = 0, max = 100, symbol = '%',
                label = "Asertividad Negativa",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        indices.dt <<- rep(0,8)
        showNotification("Error al ejecutar los indices, intente nuevamente",duration = 15,type = "error")
      })
    }
  })

  # -------------------  RF ------------------------ #

  #Cuando se genera el modelo rf
  observeEvent(input$runRf,{
    #Validaciones
    if(is.null(variable.predecir))
      showNotification("Tiene que seleccionar una variable a predecir",duration = 15, type = "error")
    if(is.null(datos))
      showNotification("Tiene que ingresar datos",duration = 15, type = "error")
    if(is.null(datos.aprendizaje))
      showNotification("Tiene que crear los datos de aprendizaje y de prueba",duration = 15, type = "error")

    if(!is.null(datos) & !is.null(variable.predecir) & !is.null(datos.aprendizaje)){ #Si se tiene los datos entonces :

      #Se genera el codigo del modelo
      codigo.rf <- rf.modelo( variable.pr = variable.predecir,
                              predictoras = input$select.var.rf,
                              ntree = input$ntree.rf)

      updateAceEditor(session, "fieldCodeRf", value = codigo.rf)
    }
  })

  #Cuando se cambia el codigo del modelo rf
  observeEvent(c(input$runRf,input$fieldCodeRf),{
    if(input$fieldCodeRf != ""){
      tryCatch({ #Se corren los codigo
        isolate(eval(parse(text = input$fieldCodeRf)))
        output$txtRf <- renderPrint(print(modelo.rf))

        #Se genera el codigo de la prediccion
        codigo.rf.pred <- rf.prediccion(variable.predecir)
        updateAceEditor(session, "fieldCodeRfPred", value = codigo.rf.pred)

        #Cambia el codigo del grafico de rf
        updateAceEditor(session, "fieldCodeRfPlot", value = rf.plot())
        output$plot.rf <- renderPlot(isolate(eval(parse(text = input$fieldCodeRfPlot ))))
      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        modelo.rf <<- NULL
        MC.rf <<- NULL
        prediccion.rf <<- NULL
        showNotification("Error al ejecutar el modelo, intente nuevamente",duration = 15,type = "error")
      })
    }
  })

  #Cuando se cambia el codigo de la prediccion de rf
  observeEvent(c(input$runRf,input$fieldCodeRf,input$fieldCodeRfPred),{
    if(input$fieldCodeRfPred != ""){
      tryCatch({ #Se corren los codigo
        isolate(eval(parse(text = input$fieldCodeRfPred)))

        #Cambia la tabla con la prediccion de rf
        output$rfPrediTable <- DT::renderDataTable(obj.predic(prediccion.rf))

        # Se genera el codigo de la matriz
        codigo.rf.mc <- rf.MC(variable.predecir)
        updateAceEditor(session, "fieldCodeRfMC", value = codigo.rf.mc)
      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        MC.rf <<- NULL
        prediccion.rf <<- NULL

        #Cambia la tabla con la prediccion de knn
        output$rfPrediTable <- DT::renderDataTable(obj.predic(prediccion.rf))

        showNotification("Error al ejecutar la prediccion, intente nuevamente",duration = 15,type = "error")
      })
    }
  })

  #Cuando se cambia el codigo de la matriz de confucion de rf
  observeEvent(c(input$runRf,input$fieldCodeRf,input$fieldCodeRfPred,input$fieldCodeRfMC),{
    if(input$fieldCodeRfMC != ""){
      tryCatch({ #Se corren los codigo
        isolate(eval(parse(text = input$fieldCodeRfMC)))
        output$txtRfMC <- renderPrint(print(MC.rf))

        isolate(eval(parse(text = plot.MC.code())))
        output$plot.rf.mc <- renderPlot(isolate(eval(parse(text = "plot.MC(MC.rf)" ))))

        # Se genera el codigo de la indices
        codigo.indices <- cod.indices()
        updateAceEditor(session, "fieldCodeRfIG", value = codigo.indices)
      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        MC.rf <<- NULL
        indices.rf <<- rep(0,8)
        output$plot.rf.mc <- renderPlot(isolate(eval(parse(text = "NULL" ))))
        showNotification("Error al ejecutar la matriz, intente nuevamente",duration = 15,type = "error")
      })
    }
  })

  #Cuando se cambia el codigo de los indices de rf
  observeEvent(c(input$runRf,input$fieldCodeRf,input$fieldCodeRfPred,input$fieldCodeRfMC,input$fieldCodeRfIG),{
    if(input$fieldCodeRfIG != ""){
      tryCatch({ #Se corren los codigo
        isolate(eval(parse(text = input$fieldCodeRfIG)))

        indices.rf <<- indices.generales(MC.rf)

        if(ncol(MC.rf) > 2){
          shinyjs::hide("rfPrecP")
          shinyjs::hide("rfPrecN")
          shinyjs::hide("rfFalP")
          shinyjs::hide("rfFalN")
          shinyjs::hide("rfAserP")
          shinyjs::hide("rfAserN")
        }else{
          shinyjs::show("rfPrecP")
          shinyjs::show("rfPrecN")
          shinyjs::show("rfFalP")
          shinyjs::show("rfFalN")
          shinyjs::show("rfAserP")
          shinyjs::show("rfAserN")
        }

        output$rfPrecGlob <- renderGauge({
          gauge(round(indices.rf[[1]],2),
                min = 0, max = 100, symbol = '%',
                label = "Precisión Global",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

        output$rfErrorGlob <- renderGauge({
          gauge(round(indices.rf[[2]],2),
                min = 0, max = 100, symbol = '%',
                label = "Error Global",
                gaugeSectors( success = c(0, 30), warning = c(31, 45), danger = c(46, 100)))
        })

        output$rfPrecP <- renderGauge({
          gauge(round(indices.rf[[3]],2) ,
                min = 0, max = 100, symbol = '%',
                label = "Precisión Positiva",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

        output$rfPrecN <- renderGauge({
          gauge(round(indices.rf[[4]],2), min = 0, max = 100,
                symbol = '%',
                label = "Precisión Negativa",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

        output$rfFalP <- renderGauge({
          gauge(round(indices.rf[[5]],2), min = 0, max = 100, symbol = '%',
                label = "Falsos Positivos",
                gaugeSectors( success = c(0, 30),
                              warning = c(31, 45),
                              danger = c(46, 100)))
        })

        output$rfFalN <- renderGauge({
          gauge(round(indices.rf[[6]],2), min = 0, max = 100, symbol = '%',
                label = "Falsos Negativos",
                gaugeSectors( success = c(0, 30),
                              warning = c(31, 45),
                              danger = c(46, 100)))
        })

        output$rfAserP <- renderGauge({
          gauge(round(indices.rf[[7]],2), min = 0, max = 100, symbol = '%',
                label = "Asertividad Positiva",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

        output$rfAserN <- renderGauge({
          gauge(round(indices.rf[[8]],2), min = 0, max = 100, symbol = '%',
                label = "Asertividad Negativa",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        indices.rf <<- rep(0,8)
        showNotification("Error al ejecutar los indices, intente nuevamente",duration = 15,type = "error")
      })
    }
  })


  # -------------------  BOOSTING ------------------------ #

  #Cuando se genera el modelo boosting
  observeEvent(input$runBoosting,{
    if(length(levels(datos[,variable.predecir])) == 2  ) {
      #Validaciones
      if(is.null(variable.predecir))
        showNotification("Tiene que seleccionar una variable a predecir",duration = 15, type = "error")
      if(is.null(datos))
        showNotification("Tiene que ingresar datos",duration = 15, type = "error")
      if(is.null(datos.aprendizaje))
        showNotification("Tiene que crear los datos de aprendizaje y de prueba",duration = 15, type = "error")

      if(!is.null(datos) & !is.null(variable.predecir) & !is.null(datos.aprendizaje)){ #Si se tiene los datos entonces :

        #Se genera el codigo del modelo
        codigo.boosting <- boosting.modelo(variable.pr = variable.predecir,
                                           predictoras = input$select.var.boosting,
                                           iter = input$iter.boosting,
                                           nu = input$nu.boosting,
                                           type = input$tipo.boosting)

        updateAceEditor(session, "fieldCodeBoosting", value = codigo.boosting)
      }
    }else{
      showModal(modalDialog(title = "ADA - BOOSTING","Este modelo solo se puede aplicar a variables binarias",
                            footer = modalButton("Cerrar"), easyClose = T))
    }
  })

  #Cuando se cambia el codigo del modelo  boosting
  observeEvent(c(input$runBoosting,input$fieldCodeBoosting),{
    if(input$fieldCodeBoosting != ""){
      tryCatch({ #Se corren los codigo
        isolate(eval(parse(text = input$fieldCodeBoosting)))
        output$txtBoosting <- renderPrint(print(modelo.boosting))

        #Se genera el codigo de la prediccion
        codigo.boosting.pred <- boosting.prediccion(variable.predecir)
        updateAceEditor(session, "fieldCodeBoostingPred", value = codigo.boosting.pred)

        #Cambia el codigo del grafico del modelo
        updateAceEditor(session, "fieldCodeBoostingPlot", value = boosting.plot())
        output$plot.boosting <- renderPlot(isolate(eval(parse(text = input$fieldCodeBoostingPlot))))

        #Cambia el codigo del grafico de importancia
        updateAceEditor(session, "fieldCodeBoostingPlotImport", value = boosting.plot.import())
        output$plot.boosting.import <- renderPlot(isolate(eval(parse(text = input$fieldCodeBoostingPlotImport ))))
      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        modelo.boosting <<- NULL
        MC.boosting <<- NULL
        prediccion.boosting <<- NULL
        showNotification("Error al ejecutar el modelo, intente nuevamente",duration = 15,type = "error")
      })
    }
  })

  #Cuando se cambia el codigo de la prediccion de boosting
  observeEvent(c(input$runBoosting,input$fieldCodeBoosting,input$fieldCodeBoostingPred),{
    if(input$fieldCodeBoostingPred != ""){
      tryCatch({ #Se corren los codigo
        isolate(eval(parse(text = input$fieldCodeBoostingPred)))

        #Cambia la tabla con la prediccion de boosting
        output$boostingPrediTable <- DT::renderDataTable(obj.predic(prediccion.boosting))

        # Se genera el codigo de la matriz
        codigo.boosting.mc <- boosting.MC(variable.predecir)
        updateAceEditor(session, "fieldCodeBoostingMC", value = codigo.boosting.mc)
      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        MC.boosting <<- NULL
        prediccion.boosting <<- NULL

        #Cambia la tabla con la prediccion de boosting
        output$boostingPrediTable <- DT::renderDataTable(obj.predic(prediccion.boosting))

        showNotification("Error al ejecutar la prediccion, intente nuevamente",duration = 15,type = "error")
      })
    }
  })

  #Cuando se cambia el codigo de la matriz de confucion de boosting
  observeEvent(c(input$runBoosting,input$fieldCodeBoosting,input$fieldCodeBoostingPred,input$fieldCodeBoostingMC),{
    if(input$fieldCodeBoostingMC != ""){
      tryCatch({ #Se corren los codigo
        isolate(eval(parse(text = input$fieldCodeBoostingMC)))
        output$txtBoostingMC <- renderPrint(print(MC.boosting))

        isolate(eval(parse(text = plot.MC.code())))
        output$plot.boosting.mc <- renderPlot(isolate(eval(parse(text = "plot.MC(MC.boosting)" ))))

        # Se genera el codigo de la indices
        codigo.indices <- cod.indices()
        updateAceEditor(session, "fieldCodeBoostingIG", value = codigo.indices)
      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        MC.boosting <<- NULL
        indices.boosting <<- rep(0,8)
        output$plot.boosting.mc <- renderPlot(isolate(eval(parse(text = "NULL" ))))
        showNotification("Error al ejecutar la matriz, intente nuevamente",duration = 15,type = "error")
      })
    }
  })

  #Cuando se cambia el codigo de los indices de boosting
  observeEvent(c(input$runBoosting,input$fieldCodeBoosting,input$fieldCodeBoostingPred,input$fieldCodeBoostingMC,input$fieldCodeBoostingIG),{
    if(input$fieldCodeBoostingIG != ""){
      tryCatch({ #Se corren los codigo
        isolate(eval(parse(text = input$fieldCodeBoostingIG)))

        indices.boosting <<- indices.generales(MC.boosting)

        if(ncol(MC.boosting) > 2){
          shinyjs::hide("boostingPrecP")
          shinyjs::hide("boostingPrecN")
          shinyjs::hide("boostingFalP")
          shinyjs::hide("boostingFalN")
          shinyjs::hide("boostingAserP")
          shinyjs::hide("boostingAserN")
        }else{
          shinyjs::show("boostingPrecP")
          shinyjs::show("boostingPrecN")
          shinyjs::show("boostingFalP")
          shinyjs::show("boostingFalN")
          shinyjs::show("boostingAserP")
          shinyjs::show("boostingAserN")
        }

        output$boostingPrecGlob <- renderGauge({
          gauge(round(indices.boosting[[1]],2),
                min = 0, max = 100, symbol = '%',
                label = "Precisión Global",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

        output$boostingErrorGlob <- renderGauge({
          gauge(round(indices.boosting[[2]],2),
                min = 0, max = 100, symbol = '%',
                label = "Error Global",
                gaugeSectors( success = c(0, 30), warning = c(31, 45), danger = c(46, 100)))
        })

        output$boostingPrecP <- renderGauge({
          gauge(round(indices.boosting[[3]],2) ,
                min = 0, max = 100, symbol = '%',
                label = "Precisión Positiva",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

        output$boostingPrecN <- renderGauge({
          gauge(round(indices.boosting[[4]],2), min = 0, max = 100,
                symbol = '%',
                label = "Precisión Negativa",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

        output$boostingFalP <- renderGauge({
          gauge(round(indices.boosting[[5]],2), min = 0, max = 100, symbol = '%',
                label = "Falsos Positivos",
                gaugeSectors( success = c(0, 30),
                              warning = c(31, 45),
                              danger = c(46, 100)))
        })

        output$boostingFalN <- renderGauge({
          gauge(round(indices.boosting[[6]],2), min = 0, max = 100, symbol = '%',
                label = "Falsos Negativos",
                gaugeSectors( success = c(0, 30),
                              warning = c(31, 45),
                              danger = c(46, 100)))
        })

        output$boostingAserP <- renderGauge({
          gauge(round(indices.boosting[[7]],2), min = 0, max = 100, symbol = '%',
                label = "Asertividad Positiva",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

        output$boostingAserN <- renderGauge({
          gauge(round(indices.boosting[[8]],2), min = 0, max = 100, symbol = '%',
                label = "Asertividad Negativa",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        indices.boosting <<- rep(0,8)
        showNotification("Error al ejecutar los indices, intente nuevamente",duration = 15,type = "error")
      })
    }
  })

  # -------------------  XGBOOSTING ------------------------ #

  #Cuando se genera el modelo xgboosting
  observeEvent(input$runXgBoosting,{
    #Validaciones
    if(is.null(variable.predecir))
      showNotification("Tiene que seleccionar una variable a predecir",duration = 15, type = "error")
    if(is.null(datos))
      showNotification("Tiene que ingresar datos",duration = 15, type = "error")
    if(is.null(datos.aprendizaje))
      showNotification("Tiene que crear los datos de aprendizaje y de prueba",duration = 15, type = "error")

    if(!is.null(datos) & !is.null(variable.predecir) & !is.null(datos.aprendizaje)){ #Si se tiene los datos entonces :
      #Se genera el codigo del modelo
      codigo.xgboosting <- xg.modelo()
      updateAceEditor(session, "fieldCodeXgBoosting", value = codigo.xgboosting)
    }
  })

  #Cuando se cambia el codigo del modelo  xgboosting
  observeEvent(c(input$runXgBoosting,input$fieldCodeXgBoosting),{
    if(input$fieldCodeXgBoosting != ""){
      tryCatch({ #Se corren los codigo
        isolate(eval(parse(text = input$fieldCodeXgBoosting)))
        output$txtXgBoosting <- renderPrint(print(modelo.xg))

        #Se genera el codigo de la prediccion
        codigo.xgboosting.pred <- xg.prediccion()
        updateAceEditor(session, "fieldCodeXgBoostingPred", value = codigo.xgboosting.pred)

        #Cambia el codigo del grafico de importancia
        updateAceEditor(session, "fieldCodeXgBoostingPlotImport", value = xgboosting.plot.import())
        output$plot.xgboosting.import <- renderPlot(isolate(eval(parse(text = input$fieldCodeXgBoostingPlotImport ))))
      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        modelo.xg <<- NULL
        MC.xg <<- NULL
        prediccion.xg <<- NULL
        aprendizaje.xg <<- NULL
        prueba.xg <<- NULL
        showNotification("Error al ejecutar el modelo, intente nuevamente",duration = 15,type = "error")
      })
    }
  })

  #Cuando se cambia el codigo de la prediccion de xgboosting
  observeEvent(c(input$runXgBoosting,input$fieldCodeXgBoosting,input$fieldCodeXgBoostingPred),{
    if(input$fieldCodeXgBoostingPred != ""){
      tryCatch({ #Se corren los codigo
        isolate(eval(parse(text = input$fieldCodeXgBoostingPred)))

        #Cambia la tabla con la prediccion de boosting
        output$xgboostingPrediTable <- DT::renderDataTable(obj.predic(prediccion.xg))

        # Se genera el codigo de la matriz
        codigo.xg.mc <- xg.MC()
        updateAceEditor(session, "fieldCodeXgBoostingMC", value = codigo.xg.mc)
      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        MC.xg <<- NULL
        prediccion.xg <<- NULL

        #Cambia la tabla con la prediccion de boosting
        output$xgboostingPrediTable <- DT::renderDataTable(obj.predic(prediccion.xg))

        showNotification("Error al ejecutar la prediccion, intente nuevamente",duration = 15,type = "error")
      })
    }
  })

  #Cuando se cambia el codigo de la matriz de confucion de xgboosting
  observeEvent(c(input$runXgBoosting,input$fieldCodeXgBoosting,input$fieldCodeXgBoostingPred,input$fieldCodeXgBoostingMC),{
    if(input$fieldCodeXgBoostingMC != ""){
      tryCatch({ #Se corren los codigo
        isolate(eval(parse(text = input$fieldCodeXgBoostingMC)))
        output$txtXgBoostingMC <- renderPrint(print(MC.xg))

        isolate(eval(parse(text = plot.MC.code())))
        output$plot.xgboosting.mc <- renderPlot(isolate(eval(parse(text = "plot.MC(MC.xg)" ))))

        # Se genera el codigo de la indices
        codigo.indices <- cod.indices()
        updateAceEditor(session, "fieldCodeXgBoostingIG", value = codigo.indices)
      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        MC.xg <<- NULL
        indices.xg <<- rep(0,8)
        output$plot.xgboosting.mc <- renderPlot(isolate(eval(parse(text = "NULL" ))))
        showNotification("Error al ejecutar la matriz, intente nuevamente",duration = 15,type = "error")
      })
    }
  })

  #Cuando se cambia el codigo de los indices de boosting
  observeEvent(c(input$runXgBoosting,input$fieldCodeXgBoosting,input$fieldCodeXgBoostingPred,input$fieldCodeXgBoostingMC,input$fieldCodeXgBoostingIG),{
    if(input$fieldCodeXgBoostingIG != ""){
      tryCatch({ #Se corren los codigo
        isolate(eval(parse(text = input$fieldCodeXgBoostingIG)))

        indices.xg <<- indices.generales(MC.xg)

        if(ncol(MC.xg) > 2){
          shinyjs::hide("xgboostingPrecP")
          shinyjs::hide("xgboostingPrecN")
          shinyjs::hide("xgboostingFalP")
          shinyjs::hide("xgboostingFalN")
          shinyjs::hide("xgboostingAserP")
          shinyjs::hide("xgboostingAserN")
        }else{
          shinyjs::show("xgboostingPrecP")
          shinyjs::show("xgboostingPrecN")
          shinyjs::show("xgboostingFalP")
          shinyjs::show("xgboostingFalN")
          shinyjs::show("xgboostingAserP")
          shinyjs::show("xgboostingAserN")
        }

        output$xgboostingPrecGlob <- renderGauge({
          gauge(round(indices.xg[[1]],2),
                min = 0, max = 100, symbol = '%',
                label = "Precisión Global",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

        output$xgboostingErrorGlob <- renderGauge({
          gauge(round(indices.xg[[2]],2),
                min = 0, max = 100, symbol = '%',
                label = "Error Global",
                gaugeSectors( success = c(0, 30), warning = c(31, 45), danger = c(46, 100)))
        })

        output$xgboostingPrecP <- renderGauge({
          gauge(round(indices.xg[[3]],2) ,
                min = 0, max = 100, symbol = '%',
                label = "Precisión Positiva",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

        output$xgboostingPrecN <- renderGauge({
          gauge(round(indices.xg[[4]],2), min = 0, max = 100,
                symbol = '%',
                label = "Precisión Negativa",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

        output$xgboostingFalP <- renderGauge({
          gauge(round(indices.xg[[5]],2), min = 0, max = 100, symbol = '%',
                label = "Falsos Positivos",
                gaugeSectors( success = c(0, 30),
                              warning = c(31, 45),
                              danger = c(46, 100)))
        })

        output$xgboostingFalN <- renderGauge({
          gauge(round(indices.xg[[6]],2), min = 0, max = 100, symbol = '%',
                label = "Falsos Negativos",
                gaugeSectors( success = c(0, 30),
                              warning = c(31, 45),
                              danger = c(46, 100)))
        })

        output$xgboostingAserP <- renderGauge({
          gauge(round(indices.xg[[7]],2), min = 0, max = 100, symbol = '%',
                label = "Asertividad Positiva",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

        output$xgboostingAserN <- renderGauge({
          gauge(round(indices.xg[[8]],2), min = 0, max = 100, symbol = '%',
                label = "Asertividad Negativa",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        indices.xg <<- rep(0,8)
        showNotification("Error al ejecutar los indices, intente nuevamente",duration = 15,type = "error")
      })
    }
  })


  # -------------------  NN ------------------------ #

  #Cuando se genera el modelo nn
  observeEvent(input$runNn,{
    #Validaciones
    if(is.null(variable.predecir))
      showNotification("Tiene que seleccionar una variable a predecir",duration = 15, type = "error")
    if(is.null(datos))
      showNotification("Tiene que ingresar datos",duration = 15, type = "error")
    if(is.null(datos.aprendizaje))
      showNotification("Tiene que crear los datos de aprendizaje y de prueba",duration = 15, type = "error")

    if(!is.null(datos) & !is.null(variable.predecir) & !is.null(datos.aprendizaje)){ #Si se tiene los datos entonces :

      #Se genera el codigo del modelo
      codigo.nn <- nn.modelo(variable.pr = variable.predecir, predictoras = input$select.var.nn,
                             size = input$size.nn, rang = input$rang.nn, decay = 5e-4,
                             maxit = input$maxit.nn, trace = input$switch.trace.nn)

      updateAceEditor(session, "fieldCodeNn", value = codigo.nn)
    }
  })

  #Cuando se cambia el codigo del modelo  nn
  observeEvent(c(input$runNn,input$fieldCodeNn),{
    if(input$fieldCodeNn != ""){
      tryCatch({ #Se corren los codigo
        isolate(eval(parse(text = input$fieldCodeNn)))
        output$txtNn <- renderPrint(print(modelo.nn))

        #Se genera el codigo de la prediccion
        codigo.nn.pred <- nn.prediccion()
        updateAceEditor(session, "fieldCodeNnPred", value = codigo.nn.pred)
      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        modelo.nn <<- NULL
        MC.nn <<- NULL
        prediccion.nn <<- NULL
        output$txtNn <- ""
        showNotification("Error al ejecutar el modelo, intente nuevamente",duration = 15,type = "error")
      })
    }
  })

  #Cuando se cambia el codigo de la prediccion de nn
  observeEvent(c(input$runNn,input$fieldCodeNn,input$fieldCodeNnPred),{
    if(input$fieldCodeNnPred != ""){
      tryCatch({ #Se corren los codigo
        isolate(eval(parse(text = input$fieldCodeNnPred)))

        #Cambia la tabla con la prediccion de nn
        output$nnPrediTable <- DT::renderDataTable(obj.predic(prediccion.nn))

        # Se genera el codigo de la matriz
        codigo.nn.mc <- nn.MC(variable.predecir)
        updateAceEditor(session, "fieldCodeNnMC", value = codigo.nn.mc)
      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        MC.nn <<- NULL
        prediccion.nn <<- NULL

        #Cambia la tabla con la prediccion de nn
        output$nnPrediTable <- DT::renderDataTable(obj.predic(prediccion.nn))

        showNotification("Error al ejecutar la prediccion, intente nuevamente",duration = 15,type = "error")
      })
    }
  })

  #Cuando se cambia el codigo de la matriz de confucion de nn
  observeEvent(c(input$runNn,input$fieldCodeNn,input$fieldCodeNnPred,input$fieldCodeNnMC),{
    if(input$fieldCodeNnMC != ""){
      tryCatch({ #Se corren los codigo
        isolate(eval(parse(text = input$fieldCodeNnMC)))
        output$txtNnMC <- renderPrint(print(MC.nn))

        isolate(eval(parse(text = plot.MC.code())))
        output$plot.nn.mc <- renderPlot(isolate(eval(parse(text = "plot.MC(MC.nn)" ))))

        # Se genera el codigo de la indices
        codigo.indices <- cod.indices()
        updateAceEditor(session, "fieldCodeNnIG", value = codigo.indices)
      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        MC.nn <<- NULL
        indices.nn <<- rep(0,8)
        output$plot.nn.mc <- renderPlot(isolate(eval(parse(text = "NULL" ))))
        output$txtNnMC <- ""
        showNotification("Error al ejecutar la matriz, intente nuevamente",duration = 15,type = "error")
      })
    }
  })

  #Cuando se cambia el codigo de los indices de nn
  observeEvent(c(input$runNn,input$fieldCodeNn,input$fieldCodeNnPred,input$fieldCodeNnMC,input$fieldCodeNnIG),{
    if(input$fieldCodeKnnIG != ""){
      tryCatch({ #Se corren los codigo
        isolate(eval(parse(text = input$fieldCodeKnnIG)))

        indices.nn <<- indices.generales(MC.nn)

        if(ncol(MC.nn) > 2){
          shinyjs::show("nnPrecGlob")
          shinyjs::show("nnErrorGlob")
          shinyjs::hide("nnPrecP")
          shinyjs::hide("nnPrecN")
          shinyjs::hide("nnFalP")
          shinyjs::hide("nnFalN")
          shinyjs::hide("nnAserP")
          shinyjs::hide("nnAserN")
        }else{
          shinyjs::show("nnPrecGlob")
          shinyjs::show("nnErrorGlob")
          shinyjs::show("nnPrecP")
          shinyjs::show("nnPrecN")
          shinyjs::show("nnFalP")
          shinyjs::show("nnFalN")
          shinyjs::show("nnAserP")
          shinyjs::show("nnAserN")
        }

        output$nnPrecGlob <- renderGauge({
          gauge(round(indices.nn[[1]],2),
                min = 0, max = 100, symbol = '%',
                label = "Precisión Global",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

        output$nnErrorGlob <- renderGauge({
          gauge(round(indices.nn[[2]],2),
                min = 0, max = 100, symbol = '%',
                label = "Error Global",
                gaugeSectors( success = c(0, 30), warning = c(31, 45), danger = c(46, 100)))
        })

        output$nnPrecP <- renderGauge({
          gauge(round(indices.nn[[3]],2) ,
                min = 0, max = 100, symbol = '%',
                label = "Precisión Positiva",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

        output$nnPrecN <- renderGauge({
          gauge(round(indices.nn[[4]],2), min = 0, max = 100,
                symbol = '%',
                label = "Precisión Negativa",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

        output$nnFalP <- renderGauge({
          gauge(round(indices.nn[[5]],2), min = 0, max = 100, symbol = '%',
                label = "Falsos Positivos",
                gaugeSectors( success = c(0, 30),
                              warning = c(31, 45),
                              danger = c(46, 100)))
        })

        output$nnFalN <- renderGauge({
          gauge(round(indices.nn[[6]],2), min = 0, max = 100, symbol = '%',
                label = "Falsos Negativos",
                gaugeSectors( success = c(0, 30),
                              warning = c(31, 45),
                              danger = c(46, 100)))
        })

        output$nnAserP <- renderGauge({
          gauge(round(indices.nn[[7]],2), min = 0, max = 100, symbol = '%',
                label = "Asertividad Positiva",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

        output$nnAserN <- renderGauge({
          gauge(round(indices.nn[[8]],2), min = 0, max = 100, symbol = '%',
                label = "Asertividad Negativa",
                gaugeSectors(success = c(70, 100),
                             warning = c(50, 69),
                             danger = c(0, 49)))
        })

      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        indices.nn <<- rep(0,8)
        showNotification("Error al ejecutar los indices, intente nuevamente",duration = 15,type = "error")
      })
    }else{
      shinyjs::hide("nnPrecGlob")
      shinyjs::hide("nnErrorGlob")
      shinyjs::hide("nnPrecP")
      shinyjs::hide("nnPrecN")
      shinyjs::hide("nnFalP")
      shinyjs::hide("nnFalN")
      shinyjs::hide("nnAserP")
      shinyjs::hide("nnAserN")
    }
  })

  ###########################################################################################################################
  ##### Funcionalidades
  ###########################################################################################################################

  # -------------------  Pagina Datos ------------------------ #

  #Cambia la tabla de datos
  output$contents <- DT::renderDT(mostrarData(),server = FALSE)

  #Cambia la tabla de datos de aprendizaje
  output$contentsAprend <- DT::renderDT(mostrarDataAprendizaje(),server = FALSE)

  #Cambia la tabla de datos de prueba
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

  # -------------------  Estadisticas Basicas ------------------------ #

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

  # -------------------  Modelos Predictivos ------------------------ #

  obj.predic <- function(predic.var = NULL) {
    real <- as.character(datos.prueba[,variable.predecir])
    predi <- as.character(predic.var)
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

  # -------------------  Reporte ------------------------ #

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

  # -------------------  Otras ------------------------ #

  close.menu <- function(mode = "datos", valor  = T){
    select <- ifelse(mode == "datos",'a[href^="#shiny-tab-parte1"]', 'a[href^="#shiny-tab-parte2"]')
    if(valor){
      shinyjs::hide(selector = "ul.menu-open");
      shinyjs::disable(selector = select)
    }else{
      shinyjs::enable(selector = select)
    }
  }

}) ## FIN SERVER




