
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
    borrar <- ls(envir = .GlobalEnv)
    borrar <- c(borrar[!(borrar %in% .GlobalEnv$foto)], "foto")
    rm(envir = .GlobalEnv, list = borrar)
    stopApp()
  })

  # Al inicio de la sesion
  observe({
    load.page(F)
    updateAceEditor(session, "fieldCodeResum", value = cod.resum())

    updateAceEditor(session, "fieldFuncNum", value = func.dya.num)
    updateAceEditor(session, "fieldFuncCat", value = func.dya.cat)

    updateAceEditor(session, "fieldCodeReport", value = def.reporte())

    shinyjs::disable(selector = 'a[href^="#shiny-tab-parte1"]')
    shinyjs::disable(selector = 'a[href^="#shiny-tab-parte2"]')

    #Cambia las tablas de datos
    actualizar.tabla()
  })

  # -------------------  Pagina Datos ------------------------ #

  #Cunado es precionado el boton de cargar datos
  observeEvent(input$loadButton, {
    codigo <- code.carga(nombre.filas = input$rowname, ruta = input$file1$datapath,
                         separador = input$sep, sep.decimal = input$dec, encabezado = input$header)
    tryCatch({
      isolate(eval(parse(text = codigo)))
      if(ncol(datos) < 1){
        showNotification(paste0("Error al cargar los Datos: Revisar separadores"), duration = 10, type = "error")
      }
    }, error = function(e) {
      showNotification(paste0("Error al cargar los Datos: ", e), duration = 10, type = "error")
      datos <<- NULL
      datos.originales <<- NULL
    })

    if(any(is.na(datos))){
      codigo.na <- code.NA(deleteNA = input$deleteNA)
      tryCatch({
        isolate(eval(parse(text = codigo.na)))
      }, error = function(e) {
        showNotification(paste0("Error al eliminar NAs: ", e), duration = 10, type = "error")
        datos <<- NULL
        datos.originales <<- NULL
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
    }, error = function(e){
      datos <<- NULL
    })

    #Cierra o abre lo s menus los menus
    close.menu("datos", is.null(datos))
    close.menu("aprendizaje", is.null(datos.aprendizaje))

    #Cambia las tablas de datos
    actualizar.tabla()
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

    #Cierra o abre lo s menus los menus
    close.menu("datos", is.null(datos))
    close.menu("aprendizaje", is.null(datos.aprendizaje))

    #Cambia la tabla de datos
    actualizar.tabla("datos")
  })

  #Segmenta los datos en aprendizaje y prueba
  observeEvent(input$segmentButton,{
    if(input$sel.predic.var != ""){
      codigo <- particion.code("datos", input$segmentacionDatosA ,input$sel.predic.var, input$semilla, input$permitir.semilla)
      updateAceEditor(session, "fieldCodeSegment", value = codigo)
      isolate(eval(parse(text = codigo)))
      nombres <- colnames.empty(datos)
      nambres.sin.pred <- nombres[-which(nombres == variable.predecir)]
      updateSelectizeInput(session, "select.var.svm.plot", choices = nambres.sin.pred)

      #Cambia los codigos de los modelos
      actualizar.codigo.knn()
      actualizar.codigo.svm()
      actualizar.codigo.dt()
      actualizar.codigo.rf()
      actualizar.codigo.boosting()
    }else{
      showNotification("Tiene que seleccionar una variable a predecir", duration = 15, type = "error")
    }

    #Cierre o abre el menu
    close.menu("aprendizaje", is.null(datos.aprendizaje))

    #Cambia las tablas de aprendizaje y de prueba
    actualizar.tabla(c("datos.aprendizaje", "datos.prueba"))
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
    if(validar.datos()){ #Si se tiene los datos entonces :
      load.page(T)
      ejecutar.knn()
      ejecutar.knn.pred()
      ejecutar.knn.mc()
      ejecutar.knn.ind()
      load.page(F)
    }
  })

  observeEvent(c(input$switch.scale.knn, input$kmax.knn,input$kernel.knn),{
    actualizar.codigo.knn()
  })

  ejecutar.knn <- function(){
    tryCatch({ #Se corren los codigo
      isolate(eval(parse(text = input$fieldCodeKnn)))
      output$txtknn <- renderPrint(print(modelo.knn))
    },
    error = function(e) { #Regresamos al estado inicial y mostramos un error
      modelo.knn <<- NULL
      MC.knn <<- NULL
      prediccion.knn <<- NULL
      output$txtknn <-  renderPrint(print(""))
      showNotification("Error al ejecutar el modelo, intente nuevamente",duration = 15,type = "error")
    })
  }

  ejecutar.knn.pred <- function(){
    tryCatch({ #Se corren los codigo
      isolate(eval(parse(text = input$fieldCodeKnnPred)))

      #Cambia la tabla con la prediccion de knn
      output$knnPrediTable <- DT::renderDataTable(obj.predic(prediccion.knn))
    },
    error = function(e) { #Regresamos al estado inicial y mostramos un error
      prediccion.knn <<- NULL
      MC.knn <<- NULL
      #Cambia la tabla con la prediccion de knn
      output$knnPrediTable <- DT::renderDataTable(NULL)
      showNotification("Error al ejecutar la prediccion, intente nuevamente",duration = 15,type = "error")
    })
  }

  ejecutar.knn.mc <- function(){
    tryCatch({ #Se corren los codigo
      isolate(eval(parse(text = input$fieldCodeKnnMC)))
      output$txtknnMC <- renderPrint(print(MC.knn))

      isolate(eval(parse(text = plot.MC.code())))
      output$plot.knn.mc <- renderPlot(isolate(eval(parse(text = "plot.MC(MC.knn)" ))))
    },
    error = function(e) { #Regresamos al estado inicial y mostramos un error
      MC.knn <<- NULL
      indices.knn <<- rep(0,8)
      output$plot.knn.mc <- renderPlot(isolate(eval(parse(text = "NULL" ))))
      output$txtknnMC <- renderPrint(print(""))
      showNotification("Error al ejecutar la matriz, intente nuevamente",duration = 15,type = "error")
    })
  }

  ejecutar.knn.ind <- function(){
    tryCatch({ #Se corren los codigo
      isolate(eval(parse(text = input$fieldCodeKnnIG)))

      indices.knn <<- indices.generales(MC.knn)
      indices.g("knn",MC.knn)

      output$knnPrecGlob <- renderGauge({
        gauge(round(indices.knn[[1]],2),
              min = 0, max = 100, symbol = '%',
              label = "Precisión Global",
              gaugeSectors(success = c(0, 100)))
      })

      output$knnErrorGlob <- renderGauge({
        gauge(round(indices.knn[[2]],2),
              min = 0, max = 100, symbol = '%',
              label = "Error Global",
              gaugeSectors( danger = c(0, 100)))
      })

      output$knnPrecP <- renderGauge({
        gauge(round(indices.knn[[3]],2) ,
              min = 0, max = 100, symbol = '%',
              label = "Precisión Positiva",
              gaugeSectors(success = c(0, 100)))
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
  }

  actualizar.codigo.knn <- function(){
    #Se genera el codigo del modelo
    cod.knn <- kkn.modelo(variable.pr = variable.predecir,
                          scale = input$switch.scale.knn,
                          kmax = input$kmax.knn, kernel = input$kernel.knn)

    #Se acualiza el codigo del modelo
    updateAceEditor(session, "fieldCodeKnn", value = cod.knn)

    #Se genera el codigo de la prediccion
    codigo.knn.pred <- kkn.prediccion()
    updateAceEditor(session, "fieldCodeKnnPred", value = codigo.knn.pred)

    # Se genera el codigo de la matriz
    codigo.knn.mc <- knn.MC(variable.predecir)
    updateAceEditor(session, "fieldCodeKnnMC", value = codigo.knn.mc)

    # Se genera el codigo de la indices
    codigo.indices <- cod.indices()
    updateAceEditor(session, "fieldCodeKnnIG", value = codigo.indices)
  }

  # -------------------  SVM ------------------------ #

  #Cuando se genera el modelo svm
  observeEvent(input$runSvm,{
    if(validar.datos()){ #Si se tiene los datos entonces :
      load.page(T)
      ejecutar.svm()
      ejecutar.svm.pred()
      ejecutar.svm.mc()
      ejecutar.svm.ind()
      load.page(F)
    }
  })

  ejecutar.svm <- function(){
    tryCatch({ #Se corren los codigo
      isolate(eval(parse(text = input$fieldCodeSvm)))
      output$txtSvm <- renderPrint(print(modelo.svm))
    },
    error = function(e) { #Regresamos al estado inicial y mostramos un error
      modelo.svm <<- NULL
      MC.svm <<- NULL
      prediccion.svm <<- NULL
      showNotification("Error al ejecutar el modelo, intente nuevamente",duration = 15,type = "error")
    })
  }

  ejecutar.svm.pred <- function(){
    tryCatch({ #Se corren los codigo
      isolate(eval(parse(text = input$fieldCodeSvmPred)))

      #Cambia la tabla con la prediccion de knn
      output$svmPrediTable <- DT::renderDataTable(obj.predic(prediccion.svm))
    },
    error = function(e) { #Regresamos al estado inicial y mostramos un error
      MC.svm <<- NULL
      prediccion.svm <<- NULL
      #Cambia la tabla con la prediccion de knn
      output$svmPrediTable <- DT::renderDataTable(NULL)
      showNotification("Error al ejecutar la prediccion, intente nuevamente",duration = 15,type = "error")
    })
  }

  ejecutar.svm.mc <- function(){
    tryCatch({ #Se corren los codigo
      isolate(eval(parse(text = input$fieldCodeSvmMC)))
      output$txtSvmMC <- renderPrint(print(MC.svm))

      isolate(eval(parse(text = plot.MC.code())))
      output$plot.svm.mc <- renderPlot(isolate(eval(parse(text = "plot.MC(MC.svm)" ))))
    },
    error = function(e) { #Regresamos al estado inicial y mostramos un error
      MC.svm <<- NULL
      indices.svm <<- rep(0,8)
      output$plot.svm.mc <- renderPlot(isolate(eval(parse(text = "NULL" ))))
      showNotification("Error al ejecutar la matriz, intente nuevamente",duration = 15,type = "error")
    })
  }

  ejecutar.svm.ind <- function(){
    tryCatch({ #Se corren los codigo
      isolate(eval(parse(text = input$fieldCodeSvmIG)))

      indices.svm <<- indices.generales(MC.svm)

      indices.g("svm", MC.svm)

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

  actualizar.codigo.svm <- function(){
    #Se genera el codigo del modelo
    codigo.svm <- svm.modelo(variable.pr = variable.predecir,
                             scale = input$switch.scale.svm,
                             kernel = input$kernel.svm)

    #Se acualiza el codigo del modelo
    updateAceEditor(session, "fieldCodeSvm", value = codigo.svm)

    #Acutaliza el codigo del grafico de clasificacion svm
    updateAceEditor(session, "fieldCodeSvmPlot", value = svm.plot(NULL))

    #Se genera el codigo de la prediccion
    codigo.svm.pred <- svm.prediccion()
    updateAceEditor(session, "fieldCodeSvmPred", value = codigo.svm.pred)

    # Se genera el codigo de la matriz
    codigo.svm.mc <- svm.MC(variable.predecir)
    updateAceEditor(session, "fieldCodeSvmMC", value = codigo.svm.mc)

    # Se genera el codigo de la indices
    codigo.indices <- cod.indices()
    updateAceEditor(session, "fieldCodeSvmIG", value = codigo.indices)
  }

  observeEvent(c(input$switch.scale.svm, input$kernel.svm),{
    actualizar.codigo.svm()
  })

  #cuando cambia el codigo del grafico de clasificacion svm
  svm.graf <- eventReactive(c(input$runSvm, input$fieldCodeSvmPlot, input$select.var.svm.plot),{
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

  # -------------------  DT ------------------------ #

  #Cuando se genera el modelo dt
  observeEvent(input$runDt,{
    if(validar.datos()){ #Si se tiene los datos entonces :
      load.page(T)
      ejecutar.dt()
      ejecutar.dt.pred()
      ejecutar.dt.mc()
      ejecutar.dt.ind()
      load.page(F)
    }
  })

  ejecutar.dt <- function(){
    tryCatch({ #Se corren los codigo
      isolate(eval(parse(text = input$fieldCodeDt)))
      output$txtDt <- renderPrint(print(modelo.dt))

      output$plot.dt <- renderPlot(isolate(eval(parse(text = input$fieldCodeDtPlot))))
    },
    error = function(e) { #Regresamos al estado inicial y mostramos un error
      modelo.dt <<- NULL
      MC.dt <<- NULL
      prediccion.dt <<- NULL
      output$plot.dt <- renderPlot(renderPlot(isolate(eval(parse(text = "NULL" )))))
      showNotification("Error al ejecutar el modelo, intente nuevamente",duration = 15,type = "error")
    })
  }

  ejecutar.dt.pred <- function(){
    tryCatch({ #Se corren los codigo
      isolate(eval(parse(text = input$fieldCodeDtPred)))
      #Cambia la tabla con la prediccion de dt
      output$dtPrediTable <- DT::renderDataTable(obj.predic(prediccion.dt))
    },
    error = function(e) { #Regresamos al estado inicial y mostramos un error
      MC.dt <<- NULL
      prediccion.dt <<- NULL
      output$dtPrediTable <- DT::renderDataTable(NULL)
      showNotification("Error al ejecutar la prediccion, intente nuevamente",duration = 15,type = "error")
    })
  }

  ejecutar.dt.mc <- function(){
    tryCatch({ #Se corren los codigo
      isolate(eval(parse(text = input$fieldCodeDtMC)))
      output$txtDtMC <- renderPrint(print(MC.dt))

      isolate(eval(parse(text = plot.MC.code())))
      output$plot.dt.mc <- renderPlot(isolate(eval(parse(text = "plot.MC(MC.dt)" ))))
    },
    error = function(e) { #Regresamos al estado inicial y mostramos un error
      MC.dt <<- NULL
      indices.dt <<- rep(0,8)
      output$plot.dt.mc <- renderPlot(isolate(eval(parse(text = "NULL" ))))
      showNotification("Error al ejecutar la matriz, intente nuevamente",duration = 15,type = "error")
    })
  }

  ejecutar.dt.ind <- function(){
    tryCatch({ #Se corren los codigo
      isolate(eval(parse(text = input$fieldCodeDtIG)))

      indices.dt <<- indices.generales(MC.dt)

      indices.g("dt", MC.dt)

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

  actualizar.codigo.dt <- function(){
    #Se genera el codigo del modelo
    codigo.dt <- dt.modelo(variable.pr = variable.predecir,
                           minsplit = input$minsplit.dt)

    #Se acualiza el codigo del modelo
    updateAceEditor(session, "fieldCodeDt", value = codigo.dt)

    #Cambia el codigo del grafico del árbol
    updateAceEditor(session, "fieldCodeDtPlot", value = dt.plot())
    output$plot.dt <- renderPlot(isolate(eval(parse(text = input$fieldCodeDtPlot ))))

    #Se genera el codigo de la prediccion
    codigo.dt.pred <- dt.prediccion()
    updateAceEditor(session, "fieldCodeDtPred", value = codigo.dt.pred)

    # Se genera el codigo de la matriz
    codigo.dt.mc <- dt.MC(variable.predecir)
    updateAceEditor(session, "fieldCodeDtMC", value = codigo.dt.mc)

    # Se genera el codigo de la indices
    codigo.indices <- cod.indices()
    updateAceEditor(session, "fieldCodeDtIG", value = codigo.indices)

  }

  observeEvent(c(input$minsplit.dt),{
    actualizar.codigo.dt()
  })

  # -------------------  RF ------------------------ #

  #Cuando se genera el modelo rf
  observeEvent(input$runRf,{
    if(validar.datos()){ #Si se tiene los datos entonces :
      load.page(T)
      ejecutar.rf()
      ejecutar.rf.pred()
      ejecutar.rf.mc()
      ejecutar.rf.ind()
      load.page(F)
    }
  })

  ejecutar.rf <- function(){
    tryCatch({ #Se corren los codigo
      isolate(eval(parse(text = input$fieldCodeRf)))
      output$txtRf <- renderPrint(print(modelo.rf))
    },
    error = function(e) { #Regresamos al estado inicial y mostramos un error
      modelo.rf <<- NULL
      MC.rf <<- NULL
      prediccion.rf <<- NULL
      showNotification("Error al ejecutar el modelo, intente nuevamente",duration = 15,type = "error")
    })
  }

  ejecutar.rf.pred <- function(){
    tryCatch({ #Se corren los codigo
      isolate(eval(parse(text = input$fieldCodeRfPred)))
      #Cambia la tabla con la prediccion de rf
      output$rfPrediTable <- DT::renderDataTable(obj.predic(prediccion.rf))
    },
    error = function(e) { #Regresamos al estado inicial y mostramos un error
      MC.rf <<- NULL
      prediccion.rf <<- NULL
      output$rfPrediTable <- DT::renderDataTable(NULL)
      showNotification("Error al ejecutar la prediccion, intente nuevamente",duration = 15,type = "error")
    })
  }

  ejecutar.rf.mc <- function(){
    tryCatch({ #Se corren los codigo
      isolate(eval(parse(text = input$fieldCodeRfMC)))
      output$txtRfMC <- renderPrint(print(MC.rf))

      isolate(eval(parse(text = plot.MC.code())))
      output$plot.rf.mc <- renderPlot(isolate(eval(parse(text = "plot.MC(MC.rf)" ))))
    },
    error = function(e) { #Regresamos al estado inicial y mostramos un error
      MC.rf <<- NULL
      indices.rf <<- rep(0,8)
      output$txtRfMC <- renderPrint(print(""))
      output$plot.rf.mc <- renderPlot(isolate(eval(parse(text = "NULL" ))))
      showNotification("Error al ejecutar la matriz, intente nuevamente",duration = 15,type = "error")
    })
  }

  ejecutar.rf.ind <- function(){
    tryCatch({ #Se corren los codigo
      isolate(eval(parse(text = input$fieldCodeRfIG)))

      indices.rf <<- indices.generales(MC.rf)

      indices.g("rf", MC.rf)

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

  actualizar.codigo.rf <- function(){
    #Se genera el codigo del modelo
    codigo.rf <- rf.modelo( variable.pr = variable.predecir,
                            ntree = input$ntree.rf)

    #Se acualiza el codigo del modelo
    updateAceEditor(session, "fieldCodeRf", value = codigo.rf)

    #Se genera el codigo de la prediccion
    codigo.rf.pred <- rf.prediccion(variable.predecir)
    updateAceEditor(session, "fieldCodeRfPred", value = codigo.rf.pred)

    #Cambia el codigo del grafico de rf
    updateAceEditor(session, "fieldCodeRfPlot", value = rf.plot())
    output$plot.rf <- renderPlot(isolate(eval(parse(text = input$fieldCodeRfPlot ))))

    # Se genera el codigo de la matriz
    codigo.rf.mc <- rf.MC(variable.predecir)
    updateAceEditor(session, "fieldCodeRfMC", value = codigo.rf.mc)

    # Se genera el codigo de la indices
    codigo.indices <- cod.indices()
    updateAceEditor(session, "fieldCodeRfIG", value = codigo.indices)
  }

  observeEvent(input$ntree.rf,{
    actualizar.codigo.rf()
  })

  # -------------------  BOOSTING ------------------------ #

  #Cuando se genera el modelo boosting
  observeEvent(input$runBoosting,{
    if(length(levels(datos[,variable.predecir])) == 2 ){
      if(validar.datos()){ #Si se tiene los datos entonces :
        load.page(T)
        ejecutar.boosting()
        ejecutar.boosting.pred()
        ejecutar.boosting.mc()
        ejecutar.boosting.ind()
        load.page(F)
      }
    }else{
      showModal(modalDialog(title = "ADA - BOOSTING","Este modelo solo se puede aplicar a variables binarias",
                            footer = modalButton("Cerrar"), easyClose = T))
    }
  })

  ejecutar.boosting <- function(){
    tryCatch({ #Se corren los codigo
      isolate(eval(parse(text = input$fieldCodeBoosting)))
      output$txtBoosting <- renderPrint(print(modelo.boosting))
    },
    error = function(e) { #Regresamos al estado inicial y mostramos un error
      modelo.boosting <<- NULL
      MC.boosting <<- NULL
      prediccion.boosting <<- NULL
      output$txtBoosting <- renderPrint(print(""))
      showNotification("Error al ejecutar el modelo, intente nuevamente",duration = 15,type = "error")
    })
  }

  ejecutar.boosting.pred <- function(){
    tryCatch({ #Se corren los codigo
      isolate(eval(parse(text = input$fieldCodeBoostingPred)))
      #Cambia la tabla con la prediccion de boosting
      output$boostingPrediTable <- DT::renderDataTable(obj.predic(prediccion.boosting))
    },
    error = function(e) { #Regresamos al estado inicial y mostramos un error
      MC.boosting <<- NULL
      prediccion.boosting <<- NULL
      output$boostingPrediTable <- DT::renderDataTable(NULL)
      showNotification("Error al ejecutar la prediccion, intente nuevamente",duration = 15,type = "error")
    })
  }

  ejecutar.boosting.mc <- function(){
    tryCatch({ #Se corren los codigo
      isolate(eval(parse(text = input$fieldCodeBoostingMC)))
      output$txtBoostingMC <- renderPrint(print(MC.boosting))

      isolate(eval(parse(text = plot.MC.code())))
      output$plot.boosting.mc <- renderPlot(isolate(eval(parse(text = "plot.MC(MC.boosting)" ))))
    },
    error = function(e) { #Regresamos al estado inicial y mostramos un error
      MC.boosting <<- NULL
      indices.boosting <<- rep(0,8)
      output$plot.boosting.mc <- renderPlot(isolate(eval(parse(text = "NULL" ))))
      output$txtBoostingMC <- renderPrint(print(""))
      showNotification("Error al ejecutar la matriz, intente nuevamente",duration = 15,type = "error")
    })
  }

  ejecutar.boosting.ind <- function(){
    tryCatch({ #Se corren los codigo
      isolate(eval(parse(text = input$fieldCodeBoostingIG)))

      indices.boosting <<- indices.generales(MC.boosting)

      indices.g("boosting",MC.boosting)

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

  actualizar.codigo.boosting <- function(){
    #Se genera el codigo del modelo
    codigo.boosting <- boosting.modelo(variable.pr = variable.predecir,
                                       iter = input$iter.boosting,
                                       nu = input$nu.boosting,
                                       type = input$tipo.boosting)

    #Se acualiza el codigo del modelo
    updateAceEditor(session, "fieldCodeBoosting", value = codigo.boosting)

    #Se genera el codigo de la prediccion
    codigo.boosting.pred <- boosting.prediccion(variable.predecir)
    updateAceEditor(session, "fieldCodeBoostingPred", value = codigo.boosting.pred)

    #Cambia el codigo del grafico del modelo
    updateAceEditor(session, "fieldCodeBoostingPlot", value = boosting.plot())
    output$plot.boosting <- renderPlot(isolate(eval(parse(text = input$fieldCodeBoostingPlot))))

    #Cambia el codigo del grafico de importancia
    updateAceEditor(session, "fieldCodeBoostingPlotImport", value = boosting.plot.import())
    output$plot.boosting.import <- renderPlot(isolate(eval(parse(text = input$fieldCodeBoostingPlotImport ))))

    # Se genera el codigo de la matriz
    codigo.boosting.mc <- boosting.MC(variable.predecir)
    updateAceEditor(session, "fieldCodeBoostingMC", value = codigo.boosting.mc)

    # Se genera el codigo de la indices
    codigo.indices <- cod.indices()
    updateAceEditor(session, "fieldCodeBoostingIG", value = codigo.indices)
  }

  observeEvent(c(input$iter.boosting, input$nu.boosting, input$tipo.boosting),{
    actualizar.codigo.boosting()
  })

  ###########################################################################################################################
  ##### Funcionalidades
  ###########################################################################################################################

  # -------------------  Pagina Datos ------------------------ #

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

  # -------------------  Otros  ------------------------ #

  #Crea la tabla de comparacion entre prediccion y datos reales (datos de prueba)
  obj.predic <- function(predic.var = NULL) {
    real <- as.character(datos.prueba[,variable.predecir])
    predi <- as.character(predic.var)
    df <- cbind(real,predi,ifelse(real == predi,
                                  rep("<span style='color:green'><b>acertó</b></span>",length(real)),
                                  rep("<span style='color:red'><b>falló</b></span>",length(real))))
    colnames(df) <- c( "Datos Reales", "Predicción", " ")
    sketch <- htmltools::withTags(table(
      tableHeader(c("Datos Reales", "Predicción", " "))
    ))
    return(DT::datatable(df, selection = 'none', editable = FALSE,escape = FALSE, container = sketch,
                         extensions = c('Responsive'),
                         options = list(dom = 'frtip',pageLength = 10) ))
  }

  #Crea una tabla dependiendo de los datos ingresados
  renderizar.tabla.datos <- function(data,editable = TRUE, extensions = c('Buttons'), dom = 'Bfrtip', pageLength = 5, buttons = T, filename = NA){
    if(buttons)
      buttons <- list(list(extend = 'csv', filename = filename, text = 'Descargar'))
    else
      buttons <- NULL
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
    if(any("datos" %in% x)) #Cambia la tabla de datos
      output$contents <- DT::renderDT(renderizar.tabla.datos(datos,editable = T,
                                                             pageLength = 10,
                                                             buttons = T,
                                                             filename = "datos"), server = F)

    if(any("datos.aprendizaje" %in% x)) #Cambia la tabla de datos de aprendizaje
      output$contentsAprend <- DT::renderDT(renderizar.tabla.datos(datos.aprendizaje,editable = T,
                                                                   pageLength = 5,
                                                                   buttons = T,
                                                                   filename = "datos.aprendizaje"), server = F)

    if(any("datos.prueba" %in% x)) #Cambia la tabla de datos de prueba
      output$contentsPrueba <- DT::renderDT(renderizar.tabla.datos(datos.prueba, editable = T,
                                                                   pageLength = 5,
                                                                   buttons = T,
                                                                   filename = "datos.prueba"), server = F)
  }

  #Cierra el menu
  close.menu <- function(mode = "datos", valor  = T){
    select <- ifelse(mode == "datos",'a[href^="#shiny-tab-parte1"]', 'a[href^="#shiny-tab-parte2"]')
    if(valor){
      shinyjs::hide(selector = "ul.menu-open");
      shinyjs::disable(selector = select)
    }else{
      shinyjs::enable(selector = select)
    }
  }

  #Mostar o ocultar la paginina de cargado
  load.page <- function(value = NA){
    if(value){
      shinyjs::show("loaderWrapper")
    }else{
      Sys.sleep(1)
      shinyjs::hide("loaderWrapper")
    }
  }

  #Mostar o ocultar los indices
  indices.g <- function(id = "knn", mc = NA){
    if(!is.null(mc)){
      if(ncol(mc) > 2){
        shinyjs::show(paste0(id,"PrecGlob"))
        shinyjs::show(paste0(id,"ErrorGlob"))
        shinyjs::hide(paste0(id,"PrecP"))
        shinyjs::hide(paste0(id,"PrecN"))
        shinyjs::hide(paste0(id,"FalP"))
        shinyjs::hide(paste0(id,"FalN"))
        shinyjs::hide(paste0(id,"AserP"))
        shinyjs::hide(paste0(id,"AserN"))
      }else{
        shinyjs::show(paste0(id,"PrecGlob"))
        shinyjs::show(paste0(id,"ErrorGlob"))
        shinyjs::show(paste0(id,"PrecP"))
        shinyjs::show(paste0(id,"PrecN"))
        shinyjs::show(paste0(id,"FalP"))
        shinyjs::show(paste0(id,"FalN"))
        shinyjs::show(paste0(id,"AserP"))
        shinyjs::show(paste0(id,"AserN"))
      }
    }else{
      shinyjs::hide(paste0(id,"PrecGlob"))
      shinyjs::hide(paste0(id,"ErrorGlob"))
      shinyjs::hide(paste0(id,"PrecP"))
      shinyjs::hide(paste0(id,"PrecN"))
      shinyjs::hide(paste0(id,"FalP"))
      shinyjs::hide(paste0(id,"FalN"))
      shinyjs::hide(paste0(id,"AserP"))
      shinyjs::hide(paste0(id,"AserN"))
    }
  }

  #Validacion comun para todos los modelos
  validar.datos <- function(){
    #Validaciones
    if(is.null(variable.predecir))
      showNotification("Tiene que seleccionar una variable a predecir",duration = 10, type = "error")
    if(is.null(datos))
      showNotification("Tiene que ingresar datos",duration = 10, type = "error")
    if(is.null(datos.aprendizaje))
      showNotification("Tiene que crear los datos de aprendizaje y de prueba",duration = 10, type = "error")

    return(!is.null(datos) & !is.null(variable.predecir) & !is.null(datos.aprendizaje))
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

}) ## FIN SERVER




