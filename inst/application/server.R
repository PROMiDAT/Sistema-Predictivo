
shinyServer(function(input, output, session) {

  ###########################################################################################################################
  ##### Configuraciones iniciales
  ###########################################################################################################################

  source('global.R', local = T)
  options(shiny.maxRequestSize=200*1024^2)
  options(DT.options = list(aLengthMenu = c(10, 30, 50), iDisplayLength = 10, scrollX = TRUE))


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
      print(paste0("ERROR EN CARGAR: ", e))
      datos <<- NULL
      datos.originales <<- NULL
      return(NULL)
    })

    updateSelectizeInput(session, "sel.normal", choices = colnames(var.numericas(datos)))
    updateSelectizeInput(session, "select.var", choices = colnames(var.numericas(datos)))
    updateSelectInput(session, "sel.distribucion.num", choices = colnames(var.numericas(datos)))
    updateSelectInput(session, "sel.distribucion.cat", choices = colnames(var.categoricas(datos)))
    updateSelectInput(session, "sel.resumen", choices = colnames(datos))
    updateSelectInput(session, "sel.predic.var", choices = colnames(var.categoricas(datos)))

    datos.prueba <<- NULL
    datos.aprendizaje <<- NULL

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

    updateSelectizeInput(session, "sel.normal", choices = colnames(var.numericas(datos)))
    updateSelectizeInput(session, "select.var", choices = colnames(var.numericas(datos)))
    updateSelectInput(session, "sel.distribucion.num", choices = colnames(var.numericas(datos)))
    updateSelectInput(session, "sel.distribucion.cat", choices = colnames(var.categoricas(datos)))
    updateSelectInput(session, "sel.resumen", choices = colnames(datos))
    updateSelectInput(session, "sel.predic.var", choices = colnames(var.categoricas(datos)))
    datos.prueba <<- NULL
    datos.aprendizaje <<- NULL

    updateAceEditor(session, "fieldModelCor", value = modelo.cor())
    tryCatch({
      isolate(eval(parse(text = modelo.cor())))
      output$txtcor <- renderPrint(print(correlacion))
    }, error = function(e) {
      return(datos <- NULL)
    })
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
    return(DT::datatable(datos, selection = 'none', editable = TRUE, extensions = 'Buttons', container = sketch,
                         options = list(dom = 'Bfrtip', buttons = list(list(extend = 'csv', filename = "datos", text = 'Descargar')))))
  })

  #Crea los select box del panel de trasnformar datos
  update.trans <- reactive({
    inFile <- c(input$loadButton)
    n <- ncol(datos)
    res <-  data.frame(Variables = colnames(datos), Tipo = c(1:n), Activa = c(1:n))
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

  observeEvent(input$segmentButton,{
    codigo <- particion.code("datos", input$segmentacionDatosA ,input$sel.predic.var)
    updateAceEditor(session, "fieldCodeSegment", value = codigo)
    isolate(eval(parse(text = codigo)))
  })

  observe({
    updateAceEditor(session, "fieldCodeResum", value = cod.resum())

    updateAceEditor(session, "fieldFuncNum", value = func.dya.num)
    updateAceEditor(session, "fieldFuncCat", value = func.dya.cat)

    #updateAceEditor(session, "fieldCodeCentr", value = func.centros)
    # updateAceEditor(session, "fieldFuncHoriz", value = func.horiz)
    # updateAceEditor(session, "fieldFuncVert", value = func.vert)
    # updateAceEditor(session, "fieldFuncRadar", value = func.radar)
    #
    # updateAceEditor(session, "fieldFuncJambu", value = def.func.jambu())
    # updateAceEditor(session, "fieldFuncKhoriz", value = func.khoriz)
    # updateAceEditor(session, "fieldFuncKvert", value = func.kvert)
    # updateAceEditor(session, "fieldFuncKradar", value = func.kradar)

    updateAceEditor(session, "fieldCodeReport", value = def.reporte())
  })


  ###########################################################################################################################
  ##### Funcionalidades
  ###########################################################################################################################

  #Cambia la tabla de datos
  output$contents <- DT::renderDT(mostrarData())

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
  output$plot.cat = renderPlot(obj.dya.cat())

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




