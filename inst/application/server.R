
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
  renderizar.tabla.datos <- function(data, editable = TRUE, extensions = c('Buttons'), dom = 'Bfrtip', pageLength = 5, buttons = T, filename = NA){
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

  #Validacion comun para todos los modelos
  validar.datos <- function(){
    #Validaciones
    if(is.null(variable.predecir)){
      showNotification("Tiene que seleccionar una variable a predecir",duration = 10, type = "error")
    }
    if(is.null(datos)){
      showNotification("Tiene que ingresar datos",duration = 10, type = "error")
    }
    if(is.null(datos.aprendizaje)){
      showNotification("Tiene que crear los datos de aprendizaje y de prueba",duration = 10, type = "error")
    }
    return(!is.null(datos) & !is.null(variable.predecir) & !is.null(datos.aprendizaje))
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
  isolate(eval(parse(text = default.func.num())))
  isolate(eval(parse(text = default.func.cat())))
  updateAceEditor(session, "fieldCodeResum", value = cod.resum())
  updateAceEditor(session, "fieldModelCor", value = modelo.cor())
  updateAceEditor(session, "fieldFuncNum", value = default.func.num())
  updateAceEditor(session, "fieldFuncCat", value = def.code.cat())

  # Valores Reactivos -------------------------------------------------------------------------------------------------------

  updatePlot <- reactiveValues(calc.normal = default.calc.normal(), normal = NULL, disp = NULL,
                               cor = NULL, dya.num = NULL, dya.cat = NULL, poder.cat = NULL)

  # Pagina de Cargar y Transformar Datos ------------------------------------------------------------------------------------

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
  borrar.modelos <- function(flag.datos = TRUE){

    # -------------------  DATA ------------------------ #
    if(flag.datos){
      datos.prueba <<- NULL
      datos.aprendizaje <<- NULL
      variable.predecir <<- NULL
    }

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

    #borra los datos de modelos
    borrar.modelos()

    #Cierra o abre lo s menus los menus
    close.menu("parte1", is.null(datos))
    close.menu("parte2", is.null(datos.aprendizaje))
    close.menu("comparar", is.null(datos.aprendizaje))
    close.menu("poderPred", is.null(datos.aprendizaje))

    #Cambia las tablas de datos
    actualizar.tabla()

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

    #borra los datos de modelos
    borrar.modelos()

    #Cierra o abre lo s menus los menus
    close.menu("parte1", is.null(datos))
    close.menu("parte2", is.null(datos.aprendizaje))
    close.menu("comparar", is.null(datos.aprendizaje))
    close.menu("poderPred", is.null(datos.aprendizaje))

    #Cambia las tablas de datos
    actualizar.tabla()
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


  # Pagina de Segmentar Datos -----------------------------------------------------------------------------------------------

  #Crea los datos de aprendizaje y prueba
  segmentar.datos <- function(codigo){
    tryCatch({
      isolate(eval(parse(text = codigo)))
      updateAceEditor(session, "fieldCodeSegment", value = codigo)
    },error = function(e){
      showNotification(paste0("Error al dividir los datos : ", e), duration = 15, type = "error")
    })
  }

  # Actualiza los selecctores relacionados con los datos de prueba y aprendizaje
  acualizar.selecctores.seg <- function(){
    nombres <- colnames.empty(var.numericas(datos))
    nambres.sin.pred <- nombres[-which(nombres == variable.predecir)]
    updateSelectizeInput(session, "select.var.svm.plot", choices = nambres.sin.pred)
    choices <- as.character(unique(datos[,variable.predecir]))
    updateSelectInput(session, "roc.sel", choices = choices, selected = choices[1])
    cat.sin.pred <- colnames.empty(var.categoricas(datos))
    cat.sin.pred <- cat.sin.pred[cat.sin.pred != input$sel.predic.var]
    updateSelectInput(session, "sel.distribucion.poder", choices = cat.sin.pred)
  }

  #Segmenta los datos en aprendizaje y prueba
  observeEvent(input$segmentButton,{
    if(input$sel.predic.var != ""){
      codigo <- particion.code("datos", input$segmentacionDatosA ,
                               input$sel.predic.var,
                               input$semilla,
                               input$permitir.semilla)

      segmentar.datos(codigo)

      acualizar.selecctores.seg()

      #Cambia los codigos de los modelos
      default.codigo.knn()
      default.codigo.svm()
      default.codigo.dt()
      deafult.codigo.rf()
      deault.codigo.boosting()
    }else{
      showNotification("Tiene que seleccionar una variable a predecir", duration = 15, type = "error")
    }

    #Cierre o abre el menu
    close.menu("parte2", is.null(datos.aprendizaje))
    close.menu("comparar", is.null(datos.aprendizaje))
    close.menu("poderPred", is.null(datos.aprendizaje))

    #Cambia las tablas de aprendizaje y de prueba
    actualizar.tabla(c("datos.aprendizaje", "datos.prueba"))
    borrar.modelos(FALSE)
  })

  #Habilitada o deshabilitada la semilla
  observeEvent(input$permitir.semilla,{
    if(input$permitir.semilla){
      shinyjs::enable("semilla")
    }else{
      shinyjs::disable("semilla")
    }
  })

  #Cuando cambia la barra de proporcion de datos de prueba (Segmentar Datos)
  observeEvent(input$segmentacionDatosA,{
    updateSliderInput(session,"segmentacionDatosT",value = 100 - input$segmentacionDatosA)
  })

  #Cuando cambia la barra de proporcion de datos de aprendizaje (Segmentar Datos)
  observeEvent(input$segmentacionDatosT,{
    updateSliderInput(session,"segmentacionDatosA",value = 100 - input$segmentacionDatosT)
  })


  # Pagina de Resumen -------------------------------------------------------------------------------------------------------

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

  # Pagina del Test de Normalidad -------------------------------------------------------------------------------------------

  #Hace el grafico de la pagina de test de normalidad
  observeEvent(c(input$loadButton, input$transButton), {
    output$plot.normal <- renderPlot({
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

  #Ejecuta el codigo en el campo del codigo
  observeEvent(input$run.normal, {
    updatePlot$normal <- input$fieldCodeNormal
  })

  #Ejecuta el codigo cuando cambian los parametros
  observeEvent(c(input$sel.normal, input$col.normal), {
    updatePlot$normal <- default.normal(data = "datos", vars = input$sel.normal, color = input$col.normal)
  })

  #Hace la tabla comparativa de la pagina de test de normalidad
  observeEvent(c(input$loadButton, input$transButton), {
    output$calculo.normal <- DT::renderDataTable({
      tryCatch({
        codigo <- updatePlot$calc.normal
        res <- isolate(eval(parse(text = codigo)))
        updateAceEditor(session, "fieldCalcNormal", value = codigo)
        return(res)
      }, error = function(e) {
        showNotification(paste0("ERROR AL CALCULAR TEST DE NORMALIDAD: ", e), duration = 10, type = "error")
      })
    })
  })

  #Ejecuta la tabla comparativa
  observeEvent(input$run.calc.normal, {
    updatePlot$calc.normal <- input$fieldCalcNormal
  })

  # Pagina de Dispersion ----------------------------------------------------------------------------------------------------

  #Hace el grafico de dispersion
  observeEvent(c(input$loadButton, input$transButton), {
    output$plot.disp <- renderPlot({
      tryCatch({
        cod.disp <<- updatePlot$disp
        updateAceEditor(session, "fieldCodeDisp", value = cod.disp)
        res <- isolate(eval(parse(text = cod.disp)))
        if(!is.null(cod.disp) && cod.disp != ""){
          codigo.reporte[[paste0("normalidad.", paste(input$select.var, collapse = "."))]] <<-
            paste0("## Dispersión \n```{r}\n", cod.disp, "\n```")
        }
        return(res)
      }, error = function(e) {
        showNotification(paste0("ERROR AL GENERAR DISPERSIÓN: ", e), duration = 10, type = "error")
      })
    })
  })

  #Ejecuta el codigo del grafico
  observeEvent(input$run.disp, {
    updatePlot$disp <- input$fieldCodeDisp
  })

  #Ejecuta el codigo cuando cambian los parametros
  observeEvent(c(input$select.var, input$col.disp), {
    if(length(input$select.var) < 2) {
      updatePlot$disp <- ""
    } else {
      updatePlot$disp <<- default.disp(data = "datos", vars = input$select.var, color = input$col.disp)
    }
  })

  # Pagina de Distribucion --------------------------------------------------------------------------------------------------

  #Hace el grafico de Distribucion numerico
  observeEvent(c(input$loadButton, input$transButton), {
    output$plot.num <- renderPlot({
      tryCatch({
        cod.dya.num  <<- updatePlot$dya.num
        res <- isolate(eval(parse(text = cod.dya.num)))
        updateAceEditor(session, "fieldCodeNum", value = cod.dya.num)
        codigo.reporte[[paste0("dya.num.", input$sel.distribucion.num)]] <<-
          paste0("## Distribución y atipicidad \n```{r}\n",
                 cod.dya.num,
                 "\n```")
        return(res)
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
  })

  #Ejecuta el codigo del grafico numerico
  observeEvent(input$run.dya.num, {
    updatePlot$dya.num <- input$fieldCodeNum
  })

  #Ejecuta el codigo cuando cambian los parametros
  observeEvent(c(input$sel.distribucion.num, input$col.dist), {
    updatePlot$dya.num <<- def.code.num(data = "datos", color = paste0("'", input$col.dist, "'"),
                                        variable = paste0("'", input$sel.distribucion.num, "'"))
  })

  #Crea la tabla de atipicos
  output$mostrar.atipicos = DT::renderDataTable({
    atipicos <- boxplot.stats(datos[, input$sel.distribucion.num])
    datos <- datos[datos[, input$sel.distribucion.num] %in% atipicos$out, input$sel.distribucion.num, drop = F]
    return(datos[order(datos[, input$sel.distribucion.num]), , drop = F])
  }, options = list(dom = 't', scrollX = TRUE, scrollY = "10vh"))

  #Hace el grafico de Distribucion categorico
  observeEvent(c(input$loadButton, input$transButton), {
    output$plot.cat <- renderPlot({
      tryCatch({
        cod.dya.cat  <<- updatePlot$dya.cat
        res <- isolate(eval(parse(text = cod.dya.cat)))
        updateAceEditor(session, "fieldCodeCat", value = cod.dya.cat)
        codigo.reporte[[paste0("dya.cat.", input$sel.distribucion.cat)]] <<-
          paste0("## Distribución \n```{r}\n", cod.dya.cat, "\n```")
        return(res)
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
  })

  #Ejecuta el codigo del grafico categorico
  observeEvent(input$run.dya.cat, {
    updatePlot$dya.cat <- input$fieldCodeCat
  })

  #Ejecuta el codigo cuando cambian los parametros
  observeEvent(input$sel.distribucion.cat, {
    updatePlot$dya.cat <<- def.code.cat(data = "datos", variable = paste0("'", input$sel.distribucion.cat, "'"))
  })

  # Pagina de Correlacion ---------------------------------------------------------------------------------------------------

  #Hace el grafico de correlacion
  observeEvent(c(input$loadButton, input$transButton, input$fieldModelCor), {
    output$plot.cor <- renderPlot({
      tryCatch({
        cod.cor <<- updatePlot$cor
        res <- isolate(eval(parse(text = cod.cor)))
        updateAceEditor(session, "fieldCodeCor", value = cod.cor)
        codigo.reporte[["correlacion"]] <<- paste0("## Correlación \n```{r}\n", cod.cor, "\n```")
        return(res)
      }, error = function(e) {
        showNotification(paste0("ERROR EN Correlacion: ", e),
                         duration = 10,
                         type = "error")
      })
    })
  })

  #Ejecuta el codigo del grafico
  observeEvent(input$run.code.cor, {
    updatePlot$cor <- input$fieldCodeCor
  })

  #Ejecuta el codigo cuando cambian los parametros
  observeEvent(c(input$cor.metodo, input$cor.tipo), {
    updatePlot$cor <- correlaciones(metodo = input$cor.metodo, tipo = input$cor.tipo)
  })

  # Pagina de Poder Predictivo ----------------------------------------------------------------------------------------------

  #Hace el grafico de poder predictivo categorico
  observeEvent(input$segmentButton,{
    output$plot.dist.poder <- renderPlot({
      tryCatch({
        cod.poder.cat <<- updatePlot$poder.cat
        res <- isolate(eval(parse(text = cod.poder.cat)))
        updateAceEditor(session, "fieldCodePoderCat", value = cod.poder.cat)
        if( ncol(var.categoricas(datos)) > 1 ){
          codigo.reporte[["poder.cat"]] <<- paste0("## Poder Predictivo Variables Categóricas \n```{r}\n", cod.poder.cat, "\n```")
        }
        return(res)
      }, error = function(e) {
        output$plot.dist.poder <- renderPlot(NULL)
        showNotification(paste0("Error en Poder Predictivo: ", e),
                         duration = 10,
                         type = "error")
      })
    })
  })

  #Ejecuta el codigo del grafico
  observeEvent(input$run.code.poder.cat, {
    updatePlot$poder.cat <- input$fieldCodePoderCat
  })

  #Ejecuta el codigo cuando cambian los parametros
  observeEvent(input$sel.distribucion.poder, {
    if(input$sel.distribucion.poder != ""){
      updatePlot$poder.cat <- plot.code.dist.porc(input$sel.distribucion.poder,
                                             input$sel.distribucion.poder,
                                             variable.predecir, variable.predecir)
    }else{
      updatePlot$poder.cat <- ""
    }
  })

  #Hace el grafico de poder predictivo numerico
  output$plot.pairs.poder <- renderPlot({
    tryCatch({
      cod.poder.num <<- updatePlot$poder.num
      res <- isolate(eval(parse(text = cod.poder.num)))
      updateAceEditor(session, "fieldCodePoderNum", value = cod.poder.num)
      if(ncol(var.numericas(datos)) >= 1){
        codigo.reporte[["poder.num"]] <<- paste0("## Poder Predictivo Variables Numéricas \n```{r}\n", cod.poder.num, "\n```")
      }
      return(res)
    }, error = function(e) {
      output$plot.pairs.poder <- renderPlot(NULL)
      showNotification(paste0("Error en Poder Predictivo: ", e),
                       duration = 10,
                       type = "error")
    })
  })

  #Ejecuta el codigo del grafico
  observeEvent(c(input$run.code.poder.num,input$segmentButton), {
    if(input$fieldCodePoderNum != "")
      updatePlot$poder.num <- input$fieldCodePoderNum
    else
      updatePlot$poder.num <- pairs.poder()
  })

  # Pagina de KNN -----------------------------------------------------------------------------------------------------------

  #Acualiza el codigo a la version por defecto
  default.codigo.knn <- function(){
    #Se acualiza el codigo del modelo
    updateAceEditor(session, "fieldCodeKnn", value = kkn.modelo(variable.pr = variable.predecir,
                                                                scale = input$switch.scale.knn,
                                                                kmax = input$kmax.knn,
                                                                kernel = input$kernel.knn))
    #Se genera el codigo de la prediccion
    updateAceEditor(session, "fieldCodeKnnPred", value = kkn.prediccion())
    # Se genera el codigo de la matriz
    updateAceEditor(session, "fieldCodeKnnMC", value = knn.MC(variable.predecir))
    # Se genera el codigo de la indices
    updateAceEditor(session, "fieldCodeKnnIG", value = cod.indices())
  }

  # PAGINA DE SVM -----------------------------------------------------------------------------------------------------------

  #Acualiza el codigo a la version por defecto
  default.codigo.svm <- function(){
    #Se acualiza el codigo del modelo
    updateAceEditor(session, "fieldCodeSvm", value = svm.modelo(variable.pr = variable.predecir,
                                                                scale = input$switch.scale.svm,
                                                                kernel = input$kernel.svm))

    #Acutaliza el codigo del grafico de clasificacion svm
    updateAceEditor(session, "fieldCodeSvmPlot", value = svm.plot(NULL))

    #Se genera el codigo de la prediccion
    updateAceEditor(session, "fieldCodeSvmPred", value = svm.prediccion())

    # Se genera el codigo de la matriz
    updateAceEditor(session, "fieldCodeSvmMC", value = svm.MC(variable.predecir))

    # Se genera el codigo de la indices
    updateAceEditor(session, "fieldCodeSvmIG", value = cod.indices())
  }

  # PAGINA DE DT ------------------------------------------------------------------------------------------------------------

  #Acualiza el codigo a la version por defecto
  default.codigo.dt <- function(){

    #Se acualiza el codigo del modelo
    updateAceEditor(session, "fieldCodeDt", value = dt.modelo(variable.pr = variable.predecir,
                                                              minsplit = input$minsplit.dt))

    #Cambia el codigo del grafico del árbol
    updateAceEditor(session, "fieldCodeDtPlot", value = dt.plot())

    #Se genera el codigo de la prediccion
    updateAceEditor(session, "fieldCodeDtPred", value = dt.prediccion())

    # Se genera el codigo de la matriz
    updateAceEditor(session, "fieldCodeDtMC", value = dt.MC(variable.predecir))

    # Se genera el codigo de la indices
    updateAceEditor(session, "fieldCodeDtIG", value = cod.indices())
  }

  # PAGINA DE RF ------------------------------------------------------------------------------------------------------------

  #Acualiza el codigo a la version por defecto
  deafult.codigo.rf <- function(){

    #Se acualiza el codigo del modelo
    updateAceEditor(session, "fieldCodeRf", value = rf.modelo( variable.pr = variable.predecir,
                                                               ntree = input$ntree.rf))

    #Se genera el codigo de la prediccion
    updateAceEditor(session, "fieldCodeRfPred", value = rf.prediccion(variable.predecir))

    #Cambia el codigo del grafico de rf
    updateAceEditor(session, "fieldCodeRfPlot", value = rf.plot())

    # Se genera el codigo de la matriz
    updateAceEditor(session, "fieldCodeRfMC", value = codigo.rf.mc <- rf.MC(variable.predecir))

    # Se genera el codigo de la indices
    updateAceEditor(session, "fieldCodeRfIG", value = cod.indices())
  }

  # PAGINA DE BOOSTING ------------------------------------------------------------------------------------------------------

  #Acualiza el codigo a la version por defecto
  deault.codigo.boosting <- function(){
    #Se acualiza el codigo del modelo
    updateAceEditor(session, "fieldCodeBoosting", value = boosting.modelo(variable.pr = variable.predecir,
                                                                          iter = input$iter.boosting,
                                                                          nu = input$nu.boosting,
                                                                          type = input$tipo.boosting))

    #Se genera el codigo de la prediccion
    updateAceEditor(session, "fieldCodeBoostingPred", value = boosting.prediccion(variable.predecir))

    #Cambia el codigo del grafico del modelo
    updateAceEditor(session, "fieldCodeBoostingPlot", value = boosting.plot())

    #Cambia el codigo del grafico de importancia
    updateAceEditor(session, "fieldCodeBoostingPlotImport", value = boosting.plot.import())

    # Se genera el codigo de la matriz
    updateAceEditor(session, "fieldCodeBoostingMC", value = boosting.MC(variable.predecir))

    # Se genera el codigo de la indices
    updateAceEditor(session, "fieldCodeBoostingIG", value = cod.indices())
  }

  # Termina la Sesion -------------------------------------------------------------------------------------------------------

  session$onSessionEnded(function() {
    rm(envir = .GlobalEnv, list = ls(envir = .GlobalEnv))
    stopApp()
  })

})
