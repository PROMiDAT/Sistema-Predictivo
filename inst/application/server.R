
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

  #Crea la tabla de comparacion entre prediccion y datos reales (datos de prueba)
  obj.predic <- function(predic.var = NULL) {
    real <- as.character(datos.prueba[,variable.predecir])
    predi <- as.character(predic.var)
    df <- cbind(real,predi,ifelse(real == predi,
                                  rep("<span style='color:green'><b>Acertó</b></span>",length(real)),
                                  rep("<span style='color:red'><b>Falló</b></span>",length(real))))
    colnames(df) <- c( "Datos Reales", "Predicción", " ")
    sketch <- htmltools::withTags(table(
      tableHeader(c("Datos Reales", "Predicción", " "))
    ))
    return(DT::datatable(df, selection = 'none',
                         editable = FALSE,
                         escape = FALSE,
                         container = sketch,
                         extensions = c('Responsive'),
                         options = list(dom = 'frtip',pageLength = 10) ))
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
    updateSelectizeInput(session, "select.var.svm.plot", choices = nombres)
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

    codigo.reporte["modelo.knn"] <<- NULL
    output$txtknn <-  renderPrint(invisible(NULL))

    #Se genera el codigo de la prediccion
    updateAceEditor(session, "fieldCodeKnnPred", value = kkn.prediccion())
    codigo.reporte["pred.knn"] <<- NULL
    output$knnPrediTable <- DT::renderDataTable(NULL)

    # Se genera el codigo de la matriz
    updateAceEditor(session, "fieldCodeKnnMC", value = knn.MC(variable.predecir))
    codigo.reporte["mc.knn"] <<- NULL
    # Se genera el codigo de la indices
    updateAceEditor(session, "fieldCodeKnnIG", value = cod.indices())
    codigo.reporte["ind.knn"] <<- NULL
  }

  #Limpia los datos segun el proceso donde se genera el error
  limpia.knn <- function(capa = NULL){
    for (i in 1:capa) {
      switch( i,
              {
                modelo.knn <<- NULL
                output$txtknn <-  renderPrint(invisible(NULL))
                codigo.reporte[["modelo.knn"]] <<- NULL
              },
              {
                prediccion.knn <<- NULL
                codigo.reporte[["pred.knn"]] <<- NULL
                output$knnPrediTable <- DT::renderDataTable(NULL)
              },
              {
                MC.knn <<- NULL
                codigo.reporte["mc.knn"] <<- NULL
                output$plot.knn.mc <- renderPlot(NULL)
                output$txtknnMC <- renderPrint(invisible(NULL))
              },
              {
                indices.knn <<- rep(0,8)
              }
      )
    }
  }

  # Cuando se genera el modelo knn
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

  #Si las opciones cambian
  observeEvent(c(input$switch.scale.knn, input$kmax.knn,input$kernel.knn),{
    default.codigo.knn()
  })

  #Genera el modelo
  ejecutar.knn <- function(){
    tryCatch({ # Se corren los codigo
      isolate(eval(parse(text = input$fieldCodeKnn)))
      output$txtknn <- renderPrint(print(modelo.knn))
      codigo.reporte[["modelo.knn"]] <<- paste0("## KNN \n```{r}\n", input$fieldCodeKnn, "\nmodelo.knn\n```")
    },
    error = function(e) { #Regresamos al estado inicial y mostramos un error
      limpia.knn(1)
      showNotification(paste0("Error al ejecutar el modelo knn, intente nuevamente : ",e),duration = 15,type = "error")
    })
  }

  #Genera la prediccion
  ejecutar.knn.pred <- function(){
    tryCatch({ #Se corren los codigo
      isolate(eval(parse(text = input$fieldCodeKnnPred)))
      score.knn <<- predict(modelo.knn, datos.prueba, type = "prob")

      #Cambia la tabla con la prediccion de knn
      output$knnPrediTable <- DT::renderDataTable(obj.predic(prediccion.knn))
      codigo.reporte[["pred.knn"]] <<- paste0("## KNN \n```{r}\n", input$fieldCodeKnn, "\nmodelo.knn\n```")
    },
    error = function(e) { #Regresamos al estado inicial y mostramos un error
      limpia.knn(2)
      showNotification(paste0("Error al ejecutar la prediccion, intente nuevamente : ",e),duration = 15,type = "error")
    })
  }

  #Genera la matriz de confusion
  ejecutar.knn.mc <- function(){
    tryCatch({ #Se corren los codigo
      isolate(eval(parse(text = input$fieldCodeKnnMC)))
      output$txtknnMC <- renderPrint(print(MC.knn))

      isolate(eval(parse(text = plot.MC.code())))
      output$plot.knn.mc <- renderPlot(isolate(eval(parse(text = "plot.MC(MC.knn)" ))))
    },
    error = function(e) { #Regresamos al estado inicial y mostramos un error
      limpia.knn(3)
      showNotification("Error al ejecutar la matriz, intente nuevamente",duration = 15,type = "error")
    })
  }

  #Genera los indices
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
      limpia.knn(4)
      showNotification("Error al ejecutar los indices, intente nuevamente",duration = 15,type = "error")
    })
  }

  # PAGINA DE SVM -----------------------------------------------------------------------------------------------------------

  #Acualiza el codigo a la version por defecto
  default.codigo.svm <- function(){
    #Se acualiza el codigo del modelo
    updateAceEditor(session, "fieldCodeSvm", value = svm.modelo(variable.pr = variable.predecir,
                                                                scale = input$switch.scale.svm,
                                                                kernel = input$kernel.svm))

    #Acutaliza el codigo del grafico de clasificacion svm
    updateAceEditor(session, "fieldCodeSvmPlot", value = "")

    #Se genera el codigo de la prediccion
    updateAceEditor(session, "fieldCodeSvmPred", value = svm.prediccion())

    # Se genera el codigo de la matriz
    updateAceEditor(session, "fieldCodeSvmMC", value = svm.MC(variable.predecir))

    # Se genera el codigo de la indices
    updateAceEditor(session, "fieldCodeSvmIG", value = cod.indices())
  }

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

  #Si las opciones cambian
  observeEvent(c(input$switch.scale.svm, input$kernel.svm),{
    default.codigo.svm()
  })

  #cuando cambia el codigo del grafico de clasificacion svm
  svm.graf <- eventReactive(c(input$runSvm, input$fieldCodeSvmPlot, input$select.var.svm.plot),{
    if(length(input$select.var.svm.plot) == 2){
      v <- colnames(datos)
      v <- v[v != variable.predecir]
      v <- v[!(v %in% input$select.var.svm.plot)]
      if(length(v) == 0){
        v <- input$select.var.svm.plot
      }
      updateAceEditor(session, "fieldCodeSvmPlot", value = svm.plot(input$select.var.svm.plot, v))
      return(isolate(eval(parse(text = input$fieldCodeSvmPlot ))))
    }else{
      updateAceEditor(session, "fieldCodeSvmPlot", value = "")
      return(isolate(eval(parse(text = "NULL" ))))
    }
  })
  output$plot.svm <- renderPlot({svm.graf()})

  #Genera el modelo
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

  #Genera la prediccion
  ejecutar.svm.pred <- function(){
    tryCatch({ #Se corren los codigo
      isolate(eval(parse(text = input$fieldCodeSvmPred)))

      modelo.svm.roc <- svm(as.formula(paste0(variable.predecir, "~.")),
                            data = datos.aprendizaje,
                            scale =T,
                            kernel = input$kernel.svm,
                            probability = T)

      score.svm <<- predict(modelo.svm.roc, datos.prueba, probability = T)

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

  #Genera la matriz de confusion
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

  #Genera los indices
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

  #Si las opciones cambian
  observeEvent(c(input$minsplit.dt),{
    default.codigo.dt()
  })

  #Genera el modelo
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

  #Genera la prediccion
  ejecutar.dt.pred <- function(){
    tryCatch({ #Se corren los codigo
      isolate(eval(parse(text = input$fieldCodeDtPred)))

      score.dt <<- predict(modelo.dt, datos.prueba, type='prob')

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

  #Genera la matriz de confusion
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

  #Genera los indices
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

  #Si las opciones cambian
  observeEvent(input$ntree.rf,{
    deafult.codigo.rf()
  })

  #Genera el modelo
  ejecutar.rf <- function(){
    tryCatch({ #Se corren los codigo
      isolate(eval(parse(text = input$fieldCodeRf)))
      output$txtRf <- renderPrint(print(modelo.rf))
      output$plot.rf <- renderPlot(isolate(eval(parse(text = input$fieldCodeRfPlot ))))
    },
    error = function(e) { #Regresamos al estado inicial y mostramos un error
      modelo.rf <<- NULL
      MC.rf <<- NULL
      prediccion.rf <<- NULL
      showNotification("Error al ejecutar el modelo, intente nuevamente",duration = 15,type = "error")
    })
  }

  #Genera la prediccion
  ejecutar.rf.pred <- function(){
    tryCatch({ #Se corren los codigo
      isolate(eval(parse(text = input$fieldCodeRfPred)))

      score.rf <<- predict(modelo.rf,datos.prueba[,-which(colnames(datos.prueba) == variable.predecir)], type = "prob")

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

  #Genera la matriz de confusion
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

  #Genera los indices
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

  #Cuando se genera el modelo boosting
  observeEvent(input$runBoosting,{
    if(length(levels(datos[,variable.predecir])) == 2 ){
      if(validar.datos()){ #Si se tiene los datos entonces :
        load.page(T)
        ejecutar.boosting.full(TRUE)
        load.page(F)
      }
    }else{
      showModal(modalDialog(title = "ADA - BOOSTING","Este modelo solo se puede aplicar a variables binarias",
                            footer = modalButton("Cerrar"), easyClose = T))
    }
  })

  #Si las opciones cambian o actualizar el codigo
  observeEvent(c(input$iter.boosting, input$nu.boosting, input$tipo.boosting),{
    deault.codigo.boosting()
  })

  #Genera el modelo
  ejecutar.boosting <- function(error = TRUE){
    tryCatch({ #Se corren los codigo
      isolate(eval(parse(text = input$fieldCodeBoosting)))
      output$txtBoosting <- renderPrint(print(modelo.boosting))

      tryCatch({
        output$plot.boosting <- renderPlot(isolate(eval(parse(text = input$fieldCodeBoostingPlot))))
      },error = function(e){
        output$plot.boosting <- renderPlot(NULL)
      })

      tryCatch({
        output$plot.boosting.import <- renderPlot(isolate(eval(parse(text = input$fieldCodeBoostingPlotImport ))))
      },error = function(e){
        output$plot.boosting.import <- renderPlot(NULL)
      })

    },
    error = function(e) { #Regresamos al estado inicial y mostramos un error
      modelo.boosting <<- NULL
      MC.boosting <<- NULL
      prediccion.boosting <<- NULL
      output$txtBoosting <- renderPrint(NULL)
      if(error){
        showNotification("Error al ejecutar el modelo, intente nuevamente",duration = 15,type = "error")
      }
    })
  }

  #Genera la prediccion
  ejecutar.boosting.pred <- function(error = TRUE){
    tryCatch({ #Se corren los codigo
      isolate(eval(parse(text = input$fieldCodeBoostingPred)))

      score.booting <<- predict(modelo.boosting, datos.prueba[,-which(colnames(datos.prueba) == variable.predecir)], type = "prob")

      #Cambia la tabla con la prediccion de boosting
      output$boostingPrediTable <- DT::renderDataTable(obj.predic(prediccion.boosting))
    },
    error = function(e) { #Regresamos al estado inicial y mostramos un error
      MC.boosting <<- NULL
      prediccion.boosting <<- NULL
      output$boostingPrediTable <- DT::renderDataTable(NULL)
      if(error){
        showNotification("Error al ejecutar la prediccion, intente nuevamente",duration = 15,type = "error")
      }
    })
  }

  #Genera la matriz de confusion
  ejecutar.boosting.mc <- function(error = TRUE){
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
      if(error){
        showNotification("Error al ejecutar la matriz, intente nuevamente",duration = 15,type = "error")
      }
    })
  }

  #Genera los indices
  ejecutar.boosting.ind <- function(error = TRUE){
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
      if(error){
        showNotification("Error al ejecutar los indices, intente nuevamente",duration = 15,type = "error")
      }
    })
  }

  ejecutar.boosting.full <- function(error = TRUE){
    ejecutar.boosting(error)
    ejecutar.boosting.pred(error)
    ejecutar.boosting.mc(error)
    ejecutar.boosting.ind(error)
  }

  # Tabala Comparativa ------------------------------------------------------------------------------------------------------

  tabla.comparativa <- function(){
    tryCatch({
      matrices <- list("KNN" = MC.knn, "SVM" = MC.svm, "ÁRBOLES" = MC.dt, "BOSQUES" = MC.rf, "ADA-BOOSTING" = MC.boosting)
      areas <- list("KNN" = area.knn, "SVM" = area.svm, "ÁRBOLES" = area.dt, "BOSQUES" = area.rf, "ADA-BOOSTING" = area.boosting)
      matrices <- matrices[c("sel.knn","sel.svm","sel.dt","sel.rf","sel.boosting") %in% input$select.models]
      areas <- areas[c("sel.knn","sel.svm","sel.dt","sel.rf","sel.boosting") %in% input$select.models]
      cant.class <- length(unique(datos[,variable.predecir]))
      names.class <- as.character(unique(datos[,variable.predecir]))

      if(length(matrices) == 0){
        return(data.frame())
      }

      df <- NULL
      for (i in 1:length(matrices)) {
        if(is.null(matrices[[i]])){
          df <- rbind(df,c(names(matrices)[i],NA,rep(NA, cant.class),NA))
        }else{
          df <- rbind(df,c(names(matrices)[i],round(c((sum(diag(matrices[[i]])) / sum(matrices[[i]])) * 100,
                                                      diag(matrices[[i]])/rowSums(matrices[[i]]) * 100,
                                                      areas[[i]]), 2)))
        }
      }
      colnames(df) <- c("Modelo","Precisión Global",names.class,"Área de ROC")
      return(df)
    }, error =  function(){
      return(data.frame())
    })
  }

  calcular.areas <- function(){
    clase <- datos.prueba[,variable.predecir]
    if(length(unique(clase)) == 2){
      if(is.numeric(score.knn)){
        area.knn <<- areaROC(score.knn[,input$roc.sel],clase)
      }
      if(is.factor(score.svm)){
        area.svm <<- areaROC(attributes(score.svm)$probabilities[,input$roc.sel],clase)
      }
      if(is.numeric(score.dt)){
        area.dt <<- areaROC(score.dt[,input$roc.sel],clase)
      }
      if(is.numeric(score.rf)){
        area.rf <<- areaROC(score.rf[,input$roc.sel],clase)
      }
      if(is.numeric(score.booting)){
        area.booting <<- areaROC(score.booting[,which(levels(clase) == input$roc.sel)],clase)
      }
    }
  }

  observeEvent(c(input$select.models.roc, input$roc.sel),{
    if(!is.null(datos.prueba)){
      calcular.areas()
      output$plot.roc <- renderPlot(plotROC(input$select.models.roc))
    }else{
      output$plot.roc <- renderPlot(NULL)
    }
  })

  observeEvent(c(input$runKnn,input$runSvm,input$runDt,input$runRf,input$runBoosting,input$select.models,input$roc.sel),{
    if(!is.null(datos.prueba)){
      calcular.areas()
      output$TablaComp <- DT::renderDataTable(DT::datatable(tabla.comparativa(), selection = 'none',
                                                            editable = FALSE, extensions = c('Responsive'),
                                                            options = list(dom = 'frtip',
                                                                           pageLength = 10,
                                                                           buttons = NULL,
                                                                           scrollY = T)))
    }
  })

  # Termina la Sesion -------------------------------------------------------------------------------------------------------

  session$onSessionEnded(function() {
    rm(envir = .GlobalEnv, list = ls(envir = .GlobalEnv))
    stopApp()
  })

})
