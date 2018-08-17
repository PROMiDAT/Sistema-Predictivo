
shinyServer(function(input, output, session) {

  # Funciones Utilitarias ---------------------------------------------------------------------------------------------------

  # Mostar o ocultar la paginina de cargado
  load.page <- function(value = NA) {
    if (value) {
      shinyjs::show("loaderWrapper")
    } else {
      Sys.sleep(1)
      shinyjs::hide("loaderWrapper")
    }
  }

  # Crea una tabla dependiendo de los datos ingresados
  renderizar.tabla.datos <- function(data, editable = TRUE, extensions = c("Buttons"), dom = "Bfrtip", pageLength = 5, buttons = T, filename = NA) {
    if (buttons) {
      buttons <- list(list(extend = "csv", filename = filename, text = "Descargar"))
    } else {
      buttons <- NULL
    }
    nombre.columnas <- c("ID", colnames(data))
    tipo.columnas <- c("", sapply(
      colnames(data),
      function(i) ifelse(class(data[, i]) %in% c("numeric", "integer"), "Numérico", "Categórico")
    ))

    sketch <- htmltools::withTags(table(
      tableHeader(nombre.columnas),
      tableFooter(tipo.columnas)
    ))

    return(DT::datatable(data,
      selection = "none", editable = editable, container = sketch, extensions = extensions,
      options = list(dom = dom, pageLength = pageLength, buttons = buttons, scrollY = T)
    ))
  }

  # Acualiza las distintas tablas
  actualizar.tabla <- function(x = c("datos", "datos.aprendizaje", "datos.prueba")) {
    if (any("datos" %in% x)) { # Cambia la tabla de datos
      output$contents <- DT::renderDT(renderizar.tabla.datos(datos,
        editable = T,
        pageLength = 10,
        buttons = T,
        filename = "datos"
      ), server = F)
    }

    if (any("datos.aprendizaje" %in% x)) { # Cambia la tabla de datos de aprendizaje
      output$contentsAprend <- DT::renderDT(renderizar.tabla.datos(datos.aprendizaje,
        editable = T,
        pageLength = 5,
        buttons = T,
        filename = "datos.aprendizaje"
      ), server = F)
    }

    if (any("datos.prueba" %in% x)) { # Cambia la tabla de datos de prueba
      output$contentsPrueba <- DT::renderDT(renderizar.tabla.datos(datos.prueba,
        editable = T,
        pageLength = 5,
        buttons = T,
        filename = "datos.prueba"
      ), server = F)
    }
  }

  # Cierra un menu segun su tabName
  close.menu <- function(tabname = NA, valor = T) {
    select <- paste0("a[href^='#shiny-tab-", tabname, "']")
    if (valor) {
      shinyjs::hide(selector = "ul.menu-open")
      shinyjs::disable(selector = select)
    } else {
      shinyjs::enable(selector = select)
    }
  }

  # Validacion comun para todos los modelos

  validar.datos <- function(print = TRUE) {
    # Validaciones
    if (is.null(variable.predecir) & print) {
      showNotification("Tiene que seleccionar una variable a predecir", duration = 10, type = "error")
    }
    if (is.null(datos) & print) {
      showNotification("Tiene que ingresar datos", duration = 10, type = "error")
    }
    if (is.null(datos.aprendizaje) & print) {
      showNotification("Tiene que crear los datos de aprendizaje y de prueba", duration = 10, type = "error")
    }
    return(!is.null(datos) & !is.null(variable.predecir) & !is.null(datos.aprendizaje))
  }

  # Crea la tabla de comparacion entre prediccion y datos reales (datos de prueba)
  obj.predic <- function(predic.var = NULL) {
    real <- as.character(datos.prueba[, variable.predecir])
    predi <- as.character(predic.var)
    df <- cbind(real, predi, ifelse(real == predi,
      rep("<span style='color:green'><b>Acertó</b></span>", length(real)),
      rep("<span style='color:red'><b>Falló</b></span>", length(real))
    ))
    colnames(df) <- c("Datos Reales", "Predicción", " ")
    sketch <- htmltools::withTags(table(
      tableHeader(c("Datos Reales", "Predicción", " "))
    ))
    return(DT::datatable(df,
      selection = "none",
      editable = FALSE,
      escape = FALSE,
      container = sketch,
      extensions = c("Responsive"),
      options = list(dom = "frtip", pageLength = 10)
    ))
  }

  # Mostar o ocultar los indices
  indices.g <- function(id = "knn", mc = NA) {
    if (!is.null(mc)) {
      shinyjs::show(paste0(id, "PrecGlob"))
      shinyjs::show(paste0(id, "ErrorGlob"))
    } else {
      shinyjs::hide(paste0(id, "PrecGlob"))
      shinyjs::hide(paste0(id, "ErrorGlob"))
    }
  }

  # Genera los gauges
  fill.gauges <- function(ids, titulos, indices) {
    for (i in 1:length(ids)) {
      eval(parse(text = new.gauge(ids[i], indices[[i]], titulos[i])))
    }
  }

  # Configuraciones iniciales -----------------------------------------------------------------------------------------------

  source("global.R", local = T)
  options(shiny.maxRequestSize = 200 * 1024^2, DT.options = list(aLengthMenu = c(10, 30, 50), iDisplayLength = 10, scrollX = TRUE))
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

  updatePlot <- reactiveValues(
    calc.normal = default.calc.normal(), normal = NULL, disp = NULL,
    cor = NULL, dya.num = NULL, dya.cat = NULL, poder.pred = NULL,
    poder.cat = NULL, poder.num = NULL, poder.dens = NULL
  )

  # Pagina de Cargar y Transformar Datos ------------------------------------------------------------------------------------

  # Carga datos
  cargar.datos <- function(codigo.carga = "") {
    tryCatch({
      isolate(eval(parse(text = codigo.carga)))
      if (ncol(datos) < 1) {
        showNotification(paste0("Error al cargar los Datos: Revisar separadores"), duration = 10, type = "error")
        return(NULL)
      }
    },
    error = function(e) {
      showNotification(paste0("Error al cargar los Datos: ", e), duration = 10, type = "error")
      datos <<- NULL
      datos.originales <<- NULL
      return(NULL)
    }
    )
  }

  # Limpiado datos
  limpiar.datos <- function() {
    if (any(is.na(datos))) {
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

  # Transforma los datos
  transformar.datos <- function() {
    var.noactivas <- c()
    code.res <- "datos <<- datos.originales \n"
    for (var in colnames(datos.originales)) {
      if (input[[paste0("box", var, contador)]]) {
        if (input[[paste0("sel", var, contador)]] == "categorico" & class(datos.originales[, var]) %in% c("numeric", "integer")) {
          code.res <- paste0(code.res, code.trans(var, "categorico"), "\n")
        }
        if (input[[paste0("sel", var, contador)]] == "numerico" & !(class(datos.originales[, var]) %in% c("numeric", "integer"))) {
          code.res <- paste0(code.res, code.trans(var, "numerico"), "\n")
        }
        if (input[[paste0("sel", var, contador)]] == "disyuntivo") {
          code.res <- paste0(code.res, code.trans(var, "disyuntivo"), "\n")
        }
      } else {
        var.noactivas <- c(var.noactivas, var)
      }
    }

    isolate(eval(parse(text = code.res)))
    if (length(var.noactivas) > 0) {
      isolate(eval(parse(text = code.desactivar(var.noactivas))))
    }

    code.res <- paste0(code.res, "\n", code.desactivar(var.noactivas))

    return(code.res)
  }

  # Actualizar los distintos selectores
  acualizar.selecctores <- function() {
    updateSelectizeInput(session, "sel.normal", choices = colnames.empty(var.numericas(datos)))
    updateSelectizeInput(session, "select.var", choices = colnames.empty(var.numericas(datos)))
    updateSelectInput(session, "sel.distribucion.num", choices = colnames.empty(var.numericas(datos)))
    updateSelectInput(session, "sel.distribucion.cat", choices = colnames.empty(var.categoricas(datos)))
    updateSelectInput(session, "sel.resumen", choices = colnames.empty(datos))
    updateSelectInput(session, "sel.predic.var", choices = colnames.empty(var.categoricas(datos)))
  }

  # Crea las correlaciones
  ejecutar.modelo.cor <- function() {
    tryCatch({
      isolate(eval(parse(text = modelo.cor())))
      output$txtcor <- renderPrint(print(correlacion))
    }, error = function(e) {
      return(datos <- NULL)
    })
  }

  # Borra los datos de los modelos
  borrar.modelos <- function(flag.datos = TRUE) {

    # -------------------  DATA ------------------------ #
    if (flag.datos) {
      datos.prueba <<- NULL
      datos.aprendizaje <<- NULL
      variable.predecir <<- NULL
    }

    # -------------------  KNN ------------------------ #

    modelo.knn <<- NULL
    MC.knn <<- NULL
    prediccion.knn <<- NULL
    indices.knn <<- rep(0, 8)
    score.knn <<- NULL
    area.knn <<- NA

    # -------------------  SVM ------------------------ #

    modelo.svm <<- NULL
    MC.svm <<- NULL
    prediccion.svm <<- NULL
    indices.svm <<- rep(0, 8)
    score.svm <<- NULL

    # -------------------  DT ------------------------ #

    modelo.dt <<- NULL
    MC.dt <<- NULL
    prediccion.dt <<- NULL
    indices.dt <<- rep(0, 8)
    score.dt <<- NULL
    svm.roc <<- NULL

    # -------------------  RF ------------------------ #

    modelo.rf <<- NULL
    MC.rf <<- NULL
    prediccion.rf <<- NULL
    indices.rf <<- rep(0, 8)
    score.rf <<- NULL

    # -------------------  BOOSTING ------------------------ #

    modelo.boosting <<- NULL
    MC.boosting <<- NULL
    prediccion.boosting <<- NULL
    indices.boosting <<- rep(0, 8)
    score.booting <<- NULL
  }

  # Cunado es precionado el boton de cargar datos
  observeEvent(input$loadButton, {
    codigo.reporte <<- list()
    codigo.carga <- code.carga(
      nombre.filas = input$rowname, ruta = input$file1$datapath,
      separador = input$sep, sep.decimal = input$dec, encabezado = input$header
    )

    # Carga los datos
    cargar.datos(codigo.carga)

    # Limpia los datos
    codigo.na <- limpiar.datos()

    # Actualiza el codigo
    updateAceEditor(session, "fieldCodeData", value = paste0(codigo.carga, "\n", codigo.na))

    # Actualiza los selectores que dependen de los datos
    acualizar.selecctores()

    # modelo correlacion
    ejecutar.modelo.cor()

    # borra los datos de modelos
    borrar.modelos()

    # Cierra o abre lo s menus los menus
    close.menu("parte1", is.null(datos))
    close.menu("parte2", is.null(datos.aprendizaje))
    close.menu("comparar", is.null(datos.aprendizaje))
    close.menu("poderPred", is.null(datos.aprendizaje))

    # Cambia las tablas de datos
    actualizar.tabla()
  }, priority = 4)

  # Cunado es precionado el boton de transformar datos
  observeEvent(input$transButton, {

    # transforma los datos
    code.res <- transformar.datos()

    # Actualiza el codigo
    updateAceEditor(session, "fieldCodeTrans", value = code.res)

    # Actualiza los selectores que dependen de los datos
    acualizar.selecctores()

    # modelo correlacion
    ejecutar.modelo.cor()

    # borra los datos de modelos
    borrar.modelos()

    # Cierra o abre lo s menus los menus
    close.menu("parte1", is.null(datos))
    close.menu("parte2", is.null(datos.aprendizaje))
    close.menu("comparar", is.null(datos.aprendizaje))
    close.menu("poderPred", is.null(datos.aprendizaje))

    # Cambia las tablas de datos
    actualizar.tabla()
  }, priority = 4)

  # Crea los select box del panel de trasnformar datos
  update.trans <- eventReactive(input$loadButton, {
    contador <<- contador + 1
    if (!is.null(datos) && ncol(datos) > 0) {
      res <- data.frame(Variables = colnames(datos), Tipo = c(1:ncol(datos)), Activa = c(1:ncol(datos)))
      res$Tipo <- sapply(colnames(datos), function(i) paste0(
          '<select id="sel', i, contador, '"> <option value="categorico">Categórico</option>
                                                             <option value="numerico" ', ifelse(class(datos[, i]) %in% c("numeric", "integer"),
            ' selected="selected"', ""
          ),
          '>Numérico</option> <option value="disyuntivo">Disyuntivo</option> </select>'
        ))
      res$Activa <- sapply(colnames(datos), function(i) paste0('<input type="checkbox" id="box', i, contador, '" checked/>'))
    } else {
      res <- as.data.frame(NULL)
      showNotification("Tiene que cargar los datos", duration = 10, type = "error")
    }
    return(res)
  })

  # Cambia la tabla de con las opciones del panel de transformar
  output$transData <- DT::renderDataTable(update.trans(),
    escape = FALSE, selection = "none", server = FALSE,
    options = list(dom = "t", paging = FALSE, ordering = FALSE), rownames = F,
    callback = JS("table.rows().every(function(i, tab, row) {
                                                        var $this = $(this.node());
                                                        $this.attr('id', this.data()[0]);
                                                        $this.addClass('shiny-input-checkbox');});
                                                        Shiny.unbindAll(table.table().node());
                                                        Shiny.bindAll(table.table().node());")
  )


  # Pagina de Segmentar Datos -----------------------------------------------------------------------------------------------

  # Crea los datos de aprendizaje y prueba
  segmentar.datos <- function(codigo) {
    tryCatch({
      isolate(eval(parse(text = codigo)))
      updateAceEditor(session, "fieldCodeSegment", value = codigo)
    }, error = function(e) {
      showNotification(paste0("Error al dividir los datos : ", e), duration = 15, type = "error")
    })
  }

  # Actualiza los selecctores relacionados con los datos de prueba y aprendizaje
  acualizar.selecctores.seg <- function() {
    nombres <- colnames.empty(var.numericas(datos))
    updateSelectizeInput(session, "select.var.svm.plot", choices = nombres)
    choices <- as.character(unique(datos[, variable.predecir]))
    updateSelectInput(session, "roc.sel", choices = choices, selected = choices[1])
    cat.sin.pred <- colnames.empty(var.categoricas(datos))
    cat.sin.pred <- cat.sin.pred[cat.sin.pred != input$sel.predic.var]
    updateSelectInput(session, "sel.distribucion.poder", choices = cat.sin.pred)
    updateSelectInput(session, "sel.density.poder", choices = nombres)
    updateAceEditor(session, "fieldCodePoderPred", value = plot.code.poder.pred(variable.predecir))
    updatePlot$poder.pred <<- plot.code.poder.pred(variable.predecir)
  }

  # Segmenta los datos en aprendizaje y prueba
  observeEvent(input$segmentButton, {
    if (input$sel.predic.var != "") {
      codigo <- particion.code(
        "datos", input$segmentacionDatosA,
        input$sel.predic.var,
        input$semilla,
        input$permitir.semilla
      )
      knn.stop.excu <<- FALSE

      segmentar.datos(codigo)

      acualizar.selecctores.seg()
      borrar.modelos(FALSE)

      # Cambia los codigos de los modelos
      load.page(T)
      default.codigo.knn(k.def = TRUE)
      knn.full()
      default.codigo.svm()
      svm.full()
      default.codigo.dt()
      dt.full()
      deafult.codigo.rf()
      rf.full()
      deault.codigo.boosting()
      boosting.full()
      load.page(F)
    } else {
      showNotification("Tiene que seleccionar una variable a predecir", duration = 15, type = "error")
    }

    # Cierre o abre el menu
    close.menu("parte2", is.null(datos.aprendizaje))
    close.menu("comparar", is.null(datos.aprendizaje))
    close.menu("poderPred", is.null(datos.aprendizaje))
    # Cambia las tablas de aprendizaje y de prueba
    actualizar.tabla(c("datos.aprendizaje", "datos.prueba"))
  })

  # Habilitada o deshabilitada la semilla
  observeEvent(input$permitir.semilla, {
    if (input$permitir.semilla) {
      shinyjs::enable("semilla")
    } else {
      shinyjs::disable("semilla")
    }
  })

  # Cuando cambia la barra de proporcion de datos de prueba (Segmentar Datos)
  observeEvent(input$segmentacionDatosA, {
    updateSliderInput(session, "segmentacionDatosT", value = 100 - input$segmentacionDatosA)
  })

  # Cuando cambia la barra de proporcion de datos de aprendizaje (Segmentar Datos)
  observeEvent(input$segmentacionDatosT, {
    updateSliderInput(session, "segmentacionDatosA", value = 100 - input$segmentacionDatosT)
  })

  # Pagina de Resumen -------------------------------------------------------------------------------------------------------

  # Cambia la tabla con el summary en la pagina de resumen
  output$resumen.completo <- DT::renderDataTable(obj.resum(), options = list(dom = "ft", scrollX = TRUE), rownames = F)

  # Se crea una tabla summary
  obj.resum <- eventReactive(c(input$loadButton, input$transButton), {
    codigo.reporte[["resumen"]] <<- c(paste0("## Resumen Numérico \n```{r} \nsummary(datos) \n```"))
    data.frame(unclass(summary(datos)), check.names = FALSE, stringsAsFactors = FALSE)
  })

  # Cambia los cuadros de summary por varibale
  output$resumen <- renderUI({
    if (input$sel.resumen %in% colnames(var.numericas(datos))) {
      HTML(resumen.numerico(datos, input$sel.resumen))
    } else {
      HTML(resumen.categorico(datos, input$sel.resumen))
    }
  })

  # Pagina del Test de Normalidad -------------------------------------------------------------------------------------------

  # Hace el grafico de la pagina de test de normalidad
  observeEvent(c(input$loadButton, input$transButton), {
    output$plot.normal <- renderPlot({
      tryCatch({
        cod.normal <<- updatePlot$normal
        res <- isolate(eval(parse(text = cod.normal)))
        updateAceEditor(session, "fieldCodeNormal", value = cod.normal)
        codigo.reporte[[paste0("normalidad.", input$sel.normal)]] <<- paste0("## Test de Normalidad \n```{r}\n", cod.normal, "\n```")
        return(res)
      }, error = function(e) {
        showNotification(paste0("ERROR AL GENERAR TEST DE NORMALIDAD: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
  })

  # Ejecuta el codigo en el campo del codigo
  observeEvent(input$run.normal, {
    updatePlot$normal <- input$fieldCodeNormal
  })

  # Ejecuta el codigo cuando cambian los parametros
  observeEvent(c(input$sel.normal, input$col.normal), {
    updatePlot$normal <- default.normal(data = "datos", vars = input$sel.normal, color = input$col.normal)
  })

  # Hace la tabla comparativa de la pagina de test de normalidad
  observeEvent(c(input$loadButton, input$transButton), {
    output$calculo.normal <- DT::renderDataTable({
      tryCatch({
        codigo <- updatePlot$calc.normal
        res <- isolate(eval(parse(text = codigo)))
        updateAceEditor(session, "fieldCalcNormal", value = codigo)
        return(res)
      }, error = function(e) {
        showNotification(paste0("ERROR AL CALCULAR TEST DE NORMALIDAD: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
  })

  # Ejecuta la tabla comparativa
  observeEvent(input$run.calc.normal, {
    updatePlot$calc.normal <- input$fieldCalcNormal
  })

  # Pagina de Dispersion ----------------------------------------------------------------------------------------------------

  # Hace el grafico de dispersion
  observeEvent(c(input$loadButton, input$transButton), {
    output$plot.disp <- renderPlot({
      tryCatch({
        cod.disp <<- updatePlot$disp
        updateAceEditor(session, "fieldCodeDisp", value = cod.disp)
        res <- isolate(eval(parse(text = cod.disp)))
        if (!is.null(cod.disp) && cod.disp != "") {
          codigo.reporte[[paste0("dispersion.", paste(input$select.var, collapse = "."))]] <<-
            paste0("## Dispersión \n```{r}\n", cod.disp, "\n```")
        }
        return(res)
      }, error = function(e) {
        showNotification(paste0("ERROR AL GENERAR DISPERSIÓN: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
  })

  # Ejecuta el codigo del grafico
  observeEvent(input$run.disp, {
    updatePlot$disp <- input$fieldCodeDisp
  })

  # Ejecuta el codigo cuando cambian los parametros
  observeEvent(c(input$select.var, input$col.disp), {
    if (length(input$select.var) < 2) {
      updatePlot$disp <- ""
    } else {
      updatePlot$disp <<- default.disp(data = "datos", vars = input$select.var, color = input$col.disp)
    }
  })

  # Pagina de Distribucion --------------------------------------------------------------------------------------------------

  # Hace el grafico de Distribucion numerico
  observeEvent(c(input$loadButton, input$transButton), {
    output$plot.num <- renderPlot({
      tryCatch({
        cod.dya.num <<- updatePlot$dya.num
        res <- isolate(eval(parse(text = cod.dya.num)))
        updateAceEditor(session, "fieldCodeNum", value = cod.dya.num)
        codigo.reporte[[paste0("dya.num.", input$sel.distribucion.num)]] <<-
          paste0("## Distribución y atipicidad \n```{r}\n", cod.dya.num,"\n```")
        return(res)
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
  })

  # Ejecuta el codigo del grafico numerico
  observeEvent(input$run.dya.num, {
    updatePlot$dya.num <- input$fieldCodeNum
  })

  # Ejecuta el codigo cuando cambian los parametros
  observeEvent(c(input$sel.distribucion.num, input$col.dist), {
    updatePlot$dya.num <<- def.code.num(
      data = "datos", color = paste0("'", input$col.dist, "'"),
      variable = paste0("'", input$sel.distribucion.num, "'")
    )
  })

  # Crea la tabla de atipicos
  output$mostrar.atipicos <- DT::renderDataTable({
    atipicos <- boxplot.stats(datos[, input$sel.distribucion.num])
    datos <- datos[datos[, input$sel.distribucion.num] %in% atipicos$out, input$sel.distribucion.num, drop = F]
    return(datos[order(datos[, input$sel.distribucion.num]), , drop = F])
  }, options = list(dom = "t", scrollX = TRUE, scrollY = "10vh"))

  # Hace el grafico de Distribucion categorico
  observeEvent(c(input$loadButton, input$transButton), {
    output$plot.cat <- renderPlot({
      tryCatch({
        cod.dya.cat <<- updatePlot$dya.cat
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

  # Ejecuta el codigo del grafico categorico
  observeEvent(input$run.dya.cat, {
    updatePlot$dya.cat <- input$fieldCodeCat
  })

  # Ejecuta el codigo cuando cambian los parametros
  observeEvent(input$sel.distribucion.cat, {
    updatePlot$dya.cat <<- def.code.cat(data = "datos", variable = paste0("'", input$sel.distribucion.cat, "'"))
  })

  # Pagina de Correlacion ---------------------------------------------------------------------------------------------------

  # Hace el grafico de correlacion
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
        return(NULL)
      })
    })
  })

  # Ejecuta el codigo del grafico
  observeEvent(input$run.code.cor, {
    updatePlot$cor <- input$fieldCodeCor
  })

  # Ejecuta el codigo cuando cambian los parametros
  observeEvent(c(input$cor.metodo, input$cor.tipo), {
    updatePlot$cor <- correlaciones(metodo = input$cor.metodo, tipo = input$cor.tipo)
  })

  # Pagina de Poder Predictivo ----------------------------------------------------------------------------------------------

  # Hace el grafico de poder predictivo distribucion de la variable predictora
  observeEvent(input$segmentButton, {
    output$plot.pred.poder <- renderPlot({
      tryCatch({
        cod.poder <<- updatePlot$poder.pred
        res <- isolate(eval(parse(text = cod.poder)))
        updateAceEditor(session, "fieldCodePoderPred", value = cod.poder)
        codigo.reporte[["poder.pred"]] <<- paste0("## Distribución Variable Discriminante \n```{r}\n", cod.poder, "\n```")
        return(res)
      }, error = function(e) {
        showNotification(paste0("Error en Poder Predictivo: ", e),
                         duration = 10,
                         type = "error")
        return(NULL)
      })
    })
  })

  # Ejecuta el codigo del grafico
  observeEvent(input$run.code.poder.pred, {
    updatePlot$poder.pred <- input$fieldCodePoderPred
  })

  # Hace el grafico de poder predictivo categorico
  observeEvent(input$segmentButton, {
    output$plot.dist.poder <- renderPlot({
      tryCatch({
        cod.poder.cat <<- updatePlot$poder.cat
        res <- isolate(eval(parse(text = cod.poder.cat)))
        updateAceEditor(session, "fieldCodePoderCat", value = cod.poder.cat)
        if (ncol(var.categoricas(datos)) > 1) {
          codigo.reporte[[paste0("poder.cat.",input$sel.distribucion.poder)]] <<- paste0("## Distribución Según Variable Discriminante \n```{r}\n", cod.poder.cat, "\n```")
        }
        return(res)
      }, error = function(e) {
        showNotification(paste0("Error en Poder Predictivo: ", e), duration = 10,type = "error")
        return(NULL)
      })
    })
  })

  # Ejecuta el codigo del grafico
  observeEvent(input$run.code.poder.cat, {
    updatePlot$poder.cat <- input$fieldCodePoderCat
  })

  # Ejecuta el codigo cuando cambian los parametros
  observeEvent(input$sel.distribucion.poder, {
    if (input$sel.distribucion.poder != "") {
      updatePlot$poder.cat <- plot.code.dist.porc(
        input$sel.distribucion.poder,
        input$sel.distribucion.poder,
        variable.predecir, variable.predecir
      )
    } else {
      updatePlot$poder.cat <- ""
    }
  })

  # Hace el grafico de poder predictivo numerico
  observeEvent(input$segmentButton,{
    output$plot.pairs.poder <- renderPlot({
      tryCatch({
        cod.poder.num <<- updatePlot$poder.num
        res <- isolate(eval(parse(text = cod.poder.num)))
        updateAceEditor(session, "fieldCodePoderNum", value = cod.poder.num)
        if (ncol(var.numericas(datos)) >= 1) {
          codigo.reporte[["poder.num"]] <<- paste0("## Poder Predictivo Variables Numéricas \n```{r}\n", cod.poder.num, "\n```")
        }
        return(res)
      }, error = function(e) {
        showNotification(paste0("Error en Poder Predictivo: ", e),
          duration = 10,
          type = "error")
        return(NULL)
      })
    })
  })

  # Ejecuta el codigo del grafico
  observeEvent(c(input$run.code.poder.num,input$segmentButton), {
    if (input$fieldCodePoderNum != "") {
      updatePlot$poder.num <- input$fieldCodePoderNum
    } else {
      updatePlot$poder.num <- pairs.poder()
    }
  })

  # Hace el grafico de poder predictivo densidad de variables numericas
  observeEvent(input$segmentButton, {
    output$plot.density.poder <- renderPlot({
      tryCatch({
        cod.poder.den <<- updatePlot$poder.dens
        res <- isolate(eval(parse(text = cod.poder.den)))
        updateAceEditor(session, "fieldCodePoderDens", value = cod.poder.den)
        if (ncol(var.numericas(datos)) > 1) {
          codigo.reporte[[paste0("poder.den.",input$sel.density.poder)]] <<- paste0("## Densidad Según Variable Discriminante\n```{r}\n", cod.poder.den, "\n```")
        }
        return(res)
      }, error = function(e) {
        showNotification(paste0("Error en Poder Predictivo: ", e),
                         duration = 10,
                         type = "error")
        return(NULL)
      })
    })
  })

  # Ejecuta el codigo del grafico
  observeEvent(input$run.code.poder.dens,{
    updatePlot$poder.dens <- input$fieldCodePoderDens
  })

  # Ejecuta el codigo cuando cambian los parametros
  observeEvent(input$sel.density.poder, {
    if (input$sel.density.poder != "") {
      updatePlot$poder.dens <- plot.numerico.dens( input$sel.density.poder)
    } else {
      updatePlot$poder.dens <- ""
    }
  })

  # Pagina de KNN -----------------------------------------------------------------------------------------------------------

  # Cuando se genera el modelo knn
  observeEvent(input$runKnn, {
    if (validar.datos()) { # Si se tiene los datos entonces :

      cod.knn.modelo <<- input$fieldCodeKnn
      cod.knn.pred <<- input$fieldCodeKnnPred
      cod.knn.mc <<- input$fieldCodeKnnMC
      cod.knn.ind <<- input$fieldCodeKnnIG

      load.page(T)
      knn.full()
      load.page(F)
    }
  })

  # Si las opciones cambian
  observeEvent(c(input$switch.scale.knn, input$kmax.knn, input$kernel.knn), {
    if (validar.datos(print = FALSE) & knn.stop.excu) {
      load.page(T)
      default.codigo.knn()
      knn.full()
      load.page(F)
    }else{
      knn.stop.excu <<- TRUE
    }
  })

  # Acualiza el codigo a la version por defecto
  default.codigo.knn <- function(k.def = FALSE) {
    if(!is.null(datos.aprendizaje) & k.def){
      k.value <- ifelse(k.def, round(sqrt(nrow(datos.aprendizaje))), input$kmax.knn)
      updateNumericInput(session,"kmax.knn",value = k.value)
    }else{
      k.value <- input$kmax.knn
    }

    # Se acualiza el codigo del modelo
    codigo <- kkn.modelo(
      variable.pr = variable.predecir,
      scale = input$switch.scale.knn,
      kmax = k.value,
      kernel = input$kernel.knn
    )
    updateAceEditor(session, "fieldCodeKnn", value = codigo)
    cod.knn.modelo <<- codigo

    # Se genera el codigo de la prediccion
    codigo <- kkn.prediccion()
    updateAceEditor(session, "fieldCodeKnnPred", value = codigo)
    cod.knn.pred <<- codigo

    # Se genera el codigo de la matriz
    codigo <- knn.MC(variable.predecir)
    updateAceEditor(session, "fieldCodeKnnMC", value = codigo)
    cod.knn.mc <<- codigo

    # Se genera el codigo de la indices
    codigo <- cod.indices()
    updateAceEditor(session, "fieldCodeKnnIG", value = codigo)
    cod.knn.ind <<- codigo
  }

  # Limpia los datos segun el proceso donde se genera el error
  limpia.knn <- function(capa = NULL) {
    for (i in capa:4) {
      switch(i, {
        modelo.knn <<- NULL
        output$txtknn <- renderPrint(invisible(""))
        codigo.reporte[["modelo.knn"]] <<- NULL
      }, {
        prediccion.knn <<- NULL
        codigo.reporte[["pred.knn"]] <<- NULL
        output$knnPrediTable <- DT::renderDataTable(NULL)
      }, {
        MC.knn <<- NULL
        codigo.reporte["mc.knn"] <<- NULL
        output$plot.knn.mc <- renderPlot(NULL)
        output$txtknnMC <- renderPrint(invisible(NULL))
      }, {
        indices.knn <<- rep(0, 10)
        codigo.reporte[["ind.knn"]] <<- NULL
      })
    }
  }

  # Ejecuta el modelo, prediccion, mc e indices de knn
  knn.full <- function() {
    ejecutar.knn()
    ejecutar.knn.pred()
    ejecutar.knn.mc()
    ejecutar.knn.ind()
  }

  # Genera el modelo
  ejecutar.knn <- function() {
    tryCatch({
      eval(parse(text = cod.knn.modelo))
      updateAceEditor(session, "fieldCodeKnn", value = cod.knn.modelo)
      output$txtknn <- renderPrint(modelo.knn)
      codigo.reporte[["modelo.knn"]] <<- paste0("## Generación del modelo KNN\n```{r}\n", cod.knn.modelo, "\nmodelo.knn\n```")
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.knn(1)
      showNotification(paste0("Error al ejecutar el modelo knn, intente nuevamente : ", e), duration = 15, type = "error")
    }
    )
  }

  # Genera la prediccion
  ejecutar.knn.pred <- function() {
    tryCatch({ # Se corren los codigo
      eval(parse(text = cod.knn.pred))
      score.knn <<- predict(modelo.knn, datos.prueba, type = "prob")

      # Cambia la tabla con la prediccion de knn
      output$knnPrediTable <- DT::renderDataTable(obj.predic(prediccion.knn))
      codigo.reporte[["pred.knn"]] <<- paste0("## Predicción del Modelo KNN\n```{r}\n", cod.knn.pred,
                                              "\ndt.to.data.frame.predict(obj.predic(prediccion.knn))\n```")
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.knn(2)
      showNotification(paste0("Error al ejecutar la prediccion, intente nuevamente : ", e), duration = 15, type = "error")
    }
    )
  }

  # Genera la matriz de confusion
  ejecutar.knn.mc <- function() {
    tryCatch({ # Se corren los codigo
      eval(parse(text = cod.knn.mc))
      output$txtknnMC <- renderPrint(print(MC.knn))

      eval(parse(text = plot.MC.code()))
      output$plot.knn.mc <- renderPlot(isolate(eval(parse(text = "plot.MC(MC.knn)"))))
      codigo.reporte[["mc.knn"]] <<- paste0("## Matriz de confusión del Modelo KNN\n```{r}\n", cod.knn.mc,
                                            "\nMC.knn\n```\n```{r}\nplot.MC(MC.knn)\n```")
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.knn(3)
      showNotification("Error al ejecutar la matriz, intente nuevamente", duration = 15, type = "error")
    }
    )
  }

  # Genera los indices
  ejecutar.knn.ind <- function() {
    tryCatch({ # Se corren los codigo
      isolate(eval(parse(text = cod.knn.ind)))

      indices.knn <<- indices.generales(MC.knn)
      indices.g("knn", MC.knn)

      codigo.reporte[["ind.knn"]] <<- paste0("## Índices Generales \n```{r}\n", cod.knn.ind, "\nindices.knn\n```")

      nombres <- c("knnPrecGlob", "knnErrorGlob")
      titulos <- c("Precisión Global", "Error Global")

      fill.gauges(nombres, titulos, indices.knn)

      # Cambia la tabla con la indices de knn
      output$knnIndPrecTable <- shiny::renderTable(xtable(indices.prec.table(indices.knn,"KNN")),
                                                  bordered = T, width = "100%", align = "c", digits = 2)

      output$knnIndErrTable <- shiny::renderTable(xtable(indices.error.table(indices.knn,"KNN")),
                                                  bordered = T, width = "100%", align = "c", digits = 2)

    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.knn(4)
      showNotification("Error al ejecutar los indices, intente nuevamente", duration = 15, type = "error")
    }
    )
  }

  # PAGINA DE SVM -----------------------------------------------------------------------------------------------------------

  # Cuando se genera el modelo svm
  observeEvent(input$runSvm, {
    if (validar.datos()) { # Si se tiene los datos entonces :
      cod.svm.modelo <<- input$fieldCodeSvm
      cod.svm.pred <<- input$fieldCodeSvmPred
      cod.svm.mc <<- input$fieldCodeSvmMC
      cod.svm.ind <<- input$fieldCodeSvmIG

      load.page(T)
      svm.full()
      load.page(F)
    }
  })

  # Si las opciones cambian
  observeEvent(c(input$switch.scale.svm, input$kernel.svm), {
    if (validar.datos(print = FALSE)) {
      load.page(T)
      default.codigo.svm()
      svm.full()
      load.page(F)
    }
  })

  # Acualiza el codigo a la version por defecto
  default.codigo.svm <- function() {
    # Se acualiza el codigo del modelo
    codigo <- svm.modelo(
      variable.pr = variable.predecir,
      scale = input$switch.scale.svm,
      kernel = input$kernel.svm
    )
    updateAceEditor(session, "fieldCodeSvm", value = codigo)
    cod.svm.modelo <<- codigo

    # Acutaliza el codigo del grafico de clasificacion svm
    updateAceEditor(session, "fieldCodeSvmPlot", value = "")

    # Se genera el codigo de la prediccion
    codigo <- svm.prediccion()
    updateAceEditor(session, "fieldCodeSvmPred", value = codigo)
    cod.svm.pred <<- codigo

    # Se genera el codigo de la matriz
    codigo <- svm.MC(variable.predecir)
    updateAceEditor(session, "fieldCodeSvmMC", value = codigo)
    cod.svm.mc <<- codigo

    # Se genera el codigo de la indices
    codigo <- cod.indices()
    updateAceEditor(session, "fieldCodeSvmIG", value = codigo)
    cod.svm.ind <<- codigo
  }

  # Limpia los datos segun el proceso donde se genera el error
  limpia.svm <- function(capa = NULL) {
    for (i in capa:4) {
      switch(i, {
        modelo.svm <<- NULL
        output$txtSvm <- renderPrint(invisible(""))
        #output$plot.svm <- renderPlot(NULL)
        codigo.reporte[["modelo.svm"]] <<- NULL
        codigo.reporte[grepl("svm.plot.", names(codigo.reporte))] <<- NULL
      }, {
        prediccion.svm <<- NULL
        codigo.reporte[["pred.svm"]] <<- NULL
        output$svmPrediTable <- DT::renderDataTable(NULL)
      }, {
        MC.svm <<- NULL
        codigo.reporte["mc.svm"] <<- NULL
        output$txtSvmMC <- renderPrint(invisible(""))
        output$plot.svm.mc <- renderPlot(NULL)
      }, {
        indices.svm <<- rep(0, 10)
        codigo.reporte["ind.svm"] <<- NULL
      })
    }
  }

  # Ejecuta el modelo, prediccion, mc e indices de svm
  svm.full <- function() {
    ejecutar.svm()
    ejecutar.svm.pred()
    ejecutar.svm.mc()
    ejecutar.svm.ind()
  }

  # Genera el modelo
  ejecutar.svm <- function() {
    tryCatch({ # Se corren los codigo
      isolate(eval(parse(text = cod.svm.modelo)))
      output$txtSvm <- renderPrint(print(modelo.svm))
      updateAceEditor(session, "fieldCodeSvm", value = cod.svm.modelo)
      codigo.reporte[["modelo.svm"]] <<- paste0("## Generación del modelo SVM\n```{r}\n", cod.svm.modelo, "\nmodelo.svm\n```")
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.svm(1)
      showNotification("Error al ejecutar el modelo, intente nuevamente", duration = 15, type = "error")
    }
    )
  }

  # Genera la prediccion
  ejecutar.svm.pred <- function() {
    tryCatch({ # Se corren los codigo
      isolate(eval(parse(text = cod.svm.pred)))

      modelo.svm.roc <- svm(as.formula(paste0(variable.predecir, "~.")),
        data = datos.aprendizaje,
        scale = T,
        kernel = input$kernel.svm,
        probability = T
      )
      score.svm <<- predict(modelo.svm.roc, datos.prueba, probability = T)

      # Cambia la tabla con la prediccion de knn
      output$svmPrediTable <- DT::renderDataTable(obj.predic(prediccion.svm))
      codigo.reporte[["pred.svm"]] <<- paste0("## Predicción del Modelo SVM\n```{r}\n", cod.knn.pred,
                                              "\ndt.to.data.frame.predict(obj.predic(prediccion.svm))\n```")
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.svm(2)
      showNotification("Error al ejecutar la prediccion, intente nuevamente", duration = 15, type = "error")
    }
    )
  }

  # Genera la matriz de confusion
  ejecutar.svm.mc <- function() {
    tryCatch({ # Se corren los codigo
      isolate(eval(parse(text = cod.svm.mc)))
      output$txtSvmMC <- renderPrint(print(MC.svm))
      isolate(eval(parse(text = plot.MC.code())))
      output$plot.svm.mc <- renderPlot(isolate(eval(parse(text = "plot.MC(MC.svm)"))))
      codigo.reporte[["mc.svm"]] <<- paste0("## Matriz de Confusión del Modelo SVM\n```{r}\n", cod.svm.mc,
                                            "\nMC.svm\n```\n```{r}\nplot.MC(MC.svm)\n```")
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.svm(3)
      showNotification("Error al ejecutar la matriz, intente nuevamente", duration = 15, type = "error")
    }
    )
  }

  # Genera los indices
  ejecutar.svm.ind <- function() {
    tryCatch({ # Se corren los codigo
      isolate(eval(parse(text = cod.svm.ind)))
      indices.svm <<- indices.generales(MC.svm)
      indices.g("svm", MC.svm)

      codigo.reporte[["ind.svm"]] <<- paste0("## Índices Generales \n```{r}\n", cod.svm.ind, "\nindices.svm\n```")

      nombres <- c("svmPrecGlob", "svmErrorGlob")
      titulos <- c("Precisión Global", "Error Global")

      fill.gauges(nombres, titulos, indices.svm)

      # Cambia la tabla con la indices de svm
      output$svmIndPrecTable <- shiny::renderTable(xtable(indices.prec.table(indices.svm,"SVM")),
                                                   bordered = T, width = "100%", align = "c", digits = 2)

      output$svmIndErrTable <- shiny::renderTable(xtable(indices.error.table(indices.svm,"SVM")),
                                                  bordered = T, width = "100%", align = "c", digits = 2)
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.knn(4)
      showNotification("Error al ejecutar los indices, intente nuevamente", duration = 15, type = "error")
    }
    )
  }

  # cuando cambia el codigo del grafico de clasificacion svm
  svm.graf <- eventReactive(c(input$runSvm, input$fieldCodeSvmPlot, input$select.var.svm.plot), {
    if (length(input$select.var.svm.plot) == 2) {
      v <- colnames(datos)
      v <- v[v != variable.predecir]
      v <- v[!(v %in% input$select.var.svm.plot)]
      if (length(v) == 0) {
        v <- input$select.var.svm.plot
      }
      updateAceEditor(session, "fieldCodeSvmPlot", value = svm.plot(input$select.var.svm.plot, v))
      codigo.reporte[[paste0("svm.plot.",paste0(input$select.var.svm.plot, collapse = "."))]] <<- paste0("\n```{r}\n", svm.plot(input$select.var.svm.plot,v), "\n```")
      return(eval(parse(text = input$fieldCodeSvmPlot)))
    } else {
      updateAceEditor(session, "fieldCodeSvmPlot", value = "")
      return(NULL)
    }
  })
  output$plot.svm <- renderPlot(svm.graf())

  # PAGINA DE DT ------------------------------------------------------------------------------------------------------------

  # Cuando se genera el modelo dt
  observeEvent(input$runDt, {
    if (validar.datos()) { # Si se tiene los datos entonces :
      cod.dt.modelo <<- input$fieldCodeDt
      cod.dt.pred <<- input$fieldCodeDtPred
      cod.dt.mc <<- input$fieldCodeDtMC
      cod.dt.ind <<- input$fieldCodeDtIG

      load.page(T)
      dt.full()
      load.page(F)
    }
  })

  # Si las opciones cambian
  observeEvent(c(input$minsplit.dt), {
    if (validar.datos(print = FALSE)) {
      load.page(T)
      default.codigo.dt()
      dt.full()
      load.page(F)
    }
  })

  # Acualiza el codigo a la version por defecto
  default.codigo.dt <- function() {

    # Se acualiza el codigo del modelo
    codigo <- dt.modelo(variable.pr = variable.predecir,
                        minsplit = input$minsplit.dt)
    updateAceEditor(session, "fieldCodeDt", value = codigo)
    cod.dt.modelo <<- codigo

    # Cambia el codigo del grafico del árbol
    updateAceEditor(session, "fieldCodeDtPlot", value = dt.plot())

    # Se genera el codigo de la prediccion
    codigo <- dt.prediccion()
    updateAceEditor(session, "fieldCodeDtPred", value = codigo)
    cod.dt.pred <<- codigo

    # Se genera el codigo de la matriz
    codigo <- dt.MC(variable.predecir)
    updateAceEditor(session, "fieldCodeDtMC", value = codigo)
    cod.dt.mc <<- codigo

    # Se genera el codigo de la indices
    codigo <- cod.indices()
    updateAceEditor(session, "fieldCodeDtIG", value = codigo)
    cod.dt.ind <<- codigo
  }

  #Plotear el arbol
  plotear.arbol <- function(){
    tryCatch({
      output$plot.dt <- renderPlot(isolate(eval(parse(text = input$fieldCodeDtPlot))))
      cod <- ifelse(input$fieldCodeDtPlot == "", dt.plot(), input$fieldCodeDtPlot)
      codigo.reporte[["modelo.dt.graf"]] <<- paste0("\n```{r}\n", cod, "\n```")
    },
    error = function(e){
      output$plot.dt <- renderPlot(NULL)
      codigo.reporte[["modelo.dt.graf"]] <<- NULL
    })
  }

  # Limpia los datos segun el proceso donde se genera el error
  limpia.dt <- function(capa = NULL) {
    for (i in capa:4) {
      switch(i, {
        modelo.dt <<- NULL
        output$txtDt <- renderPrint(invisible(""))
        output$plot.dt <- renderPlot(NULL)
        codigo.reporte[["modelo.dt"]] <<- NULL
        codigo.reporte[["modelo.dt.graf"]] <<- NULL
      }, {
        prediccion.dt <<- NULL
        codigo.reporte[["pred.dt"]] <<- NULL
        output$dtPrediTable <- DT::renderDataTable(NULL)
      }, {
        MC.dt <<- NULL
        codigo.reporte["mc.dt"] <<- NULL
        output$plot.dt.mc <- renderPlot(NULL)
        output$txtDtMC <- renderPrint(invisible(NULL))
      }, {
        indices.dt <<- rep(0, 10)
        codigo.reporte[["ind.dt"]] <<- NULL
      })
    }
  }

  # Ejecuta el modelo, prediccion, mc e indices de dt
  dt.full <- function() {
    ejecutar.dt()
    ejecutar.dt.pred()
    ejecutar.dt.mc()
    ejecutar.dt.ind()
  }

  # Genera el modelo
  ejecutar.dt <- function() {
    tryCatch({ # Se corren los codigo
      isolate(eval(parse(text = cod.dt.modelo)))
      output$txtDt <- renderPrint(print(modelo.dt))
      codigo.reporte[["modelo.dt"]] <<- paste0("## Generación del modelo Árboles de Decisión\n```{r}\n", cod.dt.modelo, "\nmodelo.dt\n```")
      plotear.arbol()
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.dt(1)
      showNotification("Error al ejecutar el modelo, intente nuevamente", duration = 15, type = "error")
    }
    )
  }

  # Genera la prediccion
  ejecutar.dt.pred <- function() {
    tryCatch({ # Se corren los codigo
      isolate(eval(parse(text = cod.dt.pred)))
      score.dt <<- predict(modelo.dt, datos.prueba, type = "prob")
      # Cambia la tabla con la prediccion de dt
      output$dtPrediTable <- DT::renderDataTable(obj.predic(prediccion.dt))
      codigo.reporte[["pred.dt"]] <<- paste0("## Predicción del Modelo Árboles de Decisión\n```{r}\n", cod.dt.pred,
                                             "\ndt.to.data.frame.predict(obj.predic(prediccion.dt))\n```")
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.dt(2)
      showNotification("Error al ejecutar la prediccion, intente nuevamente", duration = 15, type = "error")
    }
    )
  }

  # Genera la matriz de confusion
  ejecutar.dt.mc <- function() {
    tryCatch({ # Se corren los codigo
      isolate(eval(parse(text = cod.dt.mc)))
      output$txtDtMC <- renderPrint(print(MC.dt))
      isolate(eval(parse(text = plot.MC.code())))
      output$plot.dt.mc <- renderPlot(isolate(eval(parse(text = "plot.MC(MC.dt)"))))
      codigo.reporte[["mc.dt"]] <<- paste0("## Matriz de Confusión del Modelo Árboles de Decisión\n```{r}\n", cod.dt.mc,
                                           "\nMC.dt\n```\n```{r}\nplot.MC(MC.dt)\n```")
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.dt(3)
      showNotification("Error al ejecutar la matriz, intente nuevamente", duration = 15, type = "error")
    }
    )
  }

  # Genera los indices
  ejecutar.dt.ind <- function() {
    tryCatch({ # Se corren los codigo
      isolate(eval(parse(text = cod.dt.ind)))
      indices.dt <<- indices.generales(MC.dt)
      indices.g("dt", MC.dt)

      codigo.reporte[["ind.dt"]] <<- paste0("## Índices Generales \n```{r}\n", cod.dt.ind, "\nindices.dt\n```")

      nombres <- c("dtPrecGlob", "dtErrorGlob")
      titulos <- c("Precisión Global", "Error Global")

      fill.gauges(nombres, titulos, indices.dt)

      # Cambia la tabla con la indices de dt
      output$dtIndPrecTable <- shiny::renderTable(xtable(indices.prec.table(indices.dt,"Árboles de Decisión")),
                                                   bordered = T, width = "100%", align = "c", digits = 2)

      output$dtIndErrTable <- shiny::renderTable(xtable(indices.error.table(indices.dt,"Árboles de Decisión")),
                                                  bordered = T, width = "100%", align = "c", digits = 2)
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.dt(4)
      showNotification("Error al ejecutar los indices, intente nuevamente", duration = 15, type = "error")
    }
    )
  }

  # PAGINA DE RF ------------------------------------------------------------------------------------------------------------

  # Cuando se genera el modelo rf
  observeEvent(input$runRf, {
    if (validar.datos()) { # Si se tiene los datos entonces :
      cod.rf.modelo <<- input$fieldCodeRf
      cod.rf.pred <<- input$fieldCodeRfPred
      cod.rf.mc <<- input$fieldCodeRfMC
      cod.rf.ind <<- input$fieldCodeRfIG
      load.page(T)
      rf.full()
      load.page(F)
    }
  })

  # Si las opciones cambian
  observeEvent(input$ntree.rf, {
    if (validar.datos(print = FALSE)) {
      load.page(T)
      deafult.codigo.rf()
      rf.full()
      load.page(F)
    }
  })

  # Acualiza el codigo a la version por defecto
  deafult.codigo.rf <- function() {

    # Se acualiza el codigo del modelo
    codigo <- rf.modelo(
      variable.pr = variable.predecir,
      ntree = input$ntree.rf
    )
    updateAceEditor(session, "fieldCodeRf", value = codigo)
    cod.rf.modelo <<- codigo

    # Se genera el codigo de la prediccion
    codigo <- rf.prediccion(variable.predecir)
    updateAceEditor(session, "fieldCodeRfPred", value = codigo)
    cod.rf.pred <<- codigo

    # Cambia el codigo del grafico de rf
    updateAceEditor(session, "fieldCodeRfPlot", value = rf.plot())

    # Se genera el codigo de la matriz
    codigo <- rf.MC(variable.predecir)
    updateAceEditor(session, "fieldCodeRfMC", value = codigo)
    cod.rf.mc <<- codigo

    # Se genera el codigo de la indices
    codigo <- cod.indices()
    updateAceEditor(session, "fieldCodeRfIG", value = codigo)
    cod.rf.ind <<- codigo
  }

  # Limpia los datos segun el proceso donde se genera el error
  limpia.rf <- function(capa = NULL) {
    for (i in capa:4) {
      switch(i, {
        modelo.rf <<- NULL
        output$txtRf <- renderPrint(invisible(""))
        codigo.reporte[["modelo.rf"]] <<- NULL
        codigo.reporte[["modelo.rf.graf"]] <<- NULL
      }, {
        prediccion.rf <<- NULL
        codigo.reporte[["pred.rf"]] <<- NULL
        output$rfPrediTable <- DT::renderDataTable(NULL)
      }, {
        MC.rf <<- NULL
        codigo.reporte["mc.rf"] <<- NULL
        output$plot.rf.mc <- renderPlot(NULL)
        output$txtRfMC <- renderPrint(invisible(NULL))
      }, {
        indices.rf <<- rep(0, 10)
        codigo.reporte[["ind.rf"]] <<- NULL
      })
    }
  }

  # Grafico de importancia
  plotear.rf.imp <- function() {
    tryCatch({
      output$plot.rf <- renderPlot(isolate(eval(parse(text = input$fieldCodeRfPlot))))
      cod <- ifelse(input$fieldCodeRfPlot == "", rf.plot(), input$fieldCodeRfPlot)
      codigo.reporte[["modelo.rf.graf"]] <<- paste0("## Importancia de las Variables\n```{r}\n", cod , "\n```")
    }, error = function(e) {
      output$plot.rf <- renderPlot(NULL)
      codigo.reporte[["modelo.rf.graf"]] <<- NULL
    })
  }

  # Ejecuta el modelo, prediccion, mc e indices de rf
  rf.full <- function() {
    ejecutar.rf()
    ejecutar.rf.pred()
    ejecutar.rf.mc()
    ejecutar.rf.ind()
  }

  # Genera el modelo
  ejecutar.rf <- function() {
    tryCatch({ # Se corren los codigo
      isolate(eval(parse(text = cod.rf.modelo)))
      output$txtRf <- renderPrint(print(modelo.rf))
      codigo.reporte[["modelo.rf"]] <<- paste0("## Generación del Modelo Bosques Aleatorios\n```{r}\n", cod.rf.modelo, "\nmodelo.rf\n```")
      plotear.rf.imp()
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.rf(1)
      showNotification("Error al ejecutar el modelo, intente nuevamente", duration = 15, type = "error")
    }
    )
  }

  # Genera la prediccion
  ejecutar.rf.pred <- function() {
    tryCatch({ # Se corren los codigo
      isolate(eval(parse(text = cod.rf.pred)))
      score.rf <<- predict(modelo.rf, datos.prueba[, -which(colnames(datos.prueba) == variable.predecir)], type = "prob")
      # Cambia la tabla con la prediccion de rf
      output$rfPrediTable <- DT::renderDataTable(obj.predic(prediccion.rf))
      codigo.reporte[["pred.rf"]] <<- paste0("## Predicción del Modelo Bosques Aleatorios\n```{r}\n", cod.rf.pred,
                                             "\ndt.to.data.frame.predict(obj.predic(prediccion.rf))\n```")
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.rf(2)
      showNotification("Error al ejecutar la prediccion, intente nuevamente", duration = 15, type = "error")
    }
    )
  }

  # Genera la matriz de confusion
  ejecutar.rf.mc <- function() {
    tryCatch({ # Se corren los codigo
      isolate(eval(parse(text = cod.rf.mc)))
      output$txtRfMC <- renderPrint(print(MC.rf))
      isolate(eval(parse(text = plot.MC.code())))
      output$plot.rf.mc <- renderPlot(isolate(eval(parse(text = "plot.MC(MC.rf)"))))
      codigo.reporte[["mc.rf"]] <<- paste0("## Matriz de Confusión del Modelo Bosques Aleatorios\n```{r}\n", cod.rf.mc,
                                           "\nMC.rf\n```\n```{r}\nplot.MC(MC.rf)\n```")
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.rf(3)
      showNotification("Error al ejecutar la matriz, intente nuevamente", duration = 15, type = "error")
    }
    )
  }

  # Genera los indices
  ejecutar.rf.ind <- function() {
    tryCatch({ # Se corren los codigo
      isolate(eval(parse(text = cod.rf.ind)))
      indices.rf <<- indices.generales(MC.rf)
      indices.g("rf", MC.rf)

      codigo.reporte[["ind.rf"]] <<- paste0("## Índices Generales\n```{r}\n", cod.rf.ind, "\nindices.rf\n```")

      nombres <- c("rfPrecGlob", "rfErrorGlob")
      titulos <- c("Precisión Global", "Error Global")

      fill.gauges(nombres, titulos, indices.rf)

      # Cambia la tabla con la indices de rf
      output$rfIndPrecTable <- shiny::renderTable(xtable(indices.prec.table(indices.rf,"Bosques Aleatorios")),
                                                  bordered = T, width = "100%", align = "c", digits = 2)

      output$rfIndErrTable <- shiny::renderTable(xtable(indices.error.table(indices.rf,"Bosques Aleatorios")),
                                                 bordered = T, width = "100%", align = "c", digits = 2)

    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.rf(4)
      showNotification("Error al ejecutar los indices, intente nuevamente", duration = 15, type = "error")
    }
    )
  }

  # PAGINA DE BOOSTING ------------------------------------------------------------------------------------------------------

  # Cuando se genera el modelo boosting
  observeEvent(input$runBoosting, {
    if (length(levels(datos[, variable.predecir])) == 2) {
      if (validar.datos()) { # Si se tiene los datos entonces :
        cod.b.modelo <<- input$fieldCodeBoosting
        cod.b.pred <<- input$fieldCodeBoostingPred
        cod.b.mc <<- input$fieldCodeBoostingMC
        cod.b.ind <<- input$fieldCodeBoostingIG
        load.page(T)
        boosting.full()
        load.page(F)
      }
    } else {
      showModal(modalDialog(
        title = "ADA - BOOSTING", "Este modelo solo se puede aplicar a variables binarias",
        footer = modalButton("Cerrar"), easyClose = T
      ))
    }
  })

  # Si las opciones cambian o actualizar el codigo
  observeEvent(c(input$iter.boosting, input$nu.boosting, input$tipo.boosting), {
    if (validar.datos(print = FALSE) & length(levels(datos[, variable.predecir])) == 2) {
      load.page(T)
      deault.codigo.boosting()
      boosting.full()
      load.page(F)
    }
  })

  # Acualiza el codigo a la version por defecto
  deault.codigo.boosting <- function() {
    # Se acualiza el codigo del modelo
    codigo <- boosting.modelo(
      variable.pr = variable.predecir,
      iter = input$iter.boosting,
      nu = input$nu.boosting,
      type = input$tipo.boosting
    )
    updateAceEditor(session, "fieldCodeBoosting", value = codigo)
    cod.b.modelo <<- codigo

    # Se genera el codigo de la prediccion
    codigo <- boosting.prediccion(variable.predecir)
    updateAceEditor(session, "fieldCodeBoostingPred", value = codigo)
    cod.b.pred <<- codigo

    # Cambia el codigo del grafico del modelo
    updateAceEditor(session, "fieldCodeBoostingPlot", value = boosting.plot())

    # Cambia el codigo del grafico de importancia
    updateAceEditor(session, "fieldCodeBoostingPlotImport", value = boosting.plot.import())

    # Se genera el codigo de la matriz
    codigo <- boosting.MC(variable.predecir)
    updateAceEditor(session, "fieldCodeBoostingMC", value = codigo)
    cod.b.mc <<- codigo

    # Se genera el codigo de la indices
    codigo <- cod.indices()
    updateAceEditor(session, "fieldCodeBoostingIG", value = codigo)
    cod.b.ind <<- codigo
  }

  # Limpia los datos segun el proceso donde se genera el error
  limpia.boosting <- function(capa = NULL) {
    for (i in capa:4) {
      switch(i, {
        modelo.boosting <<- NULL
        output$txtBoosting <- renderPrint(invisible(""))
        output$plot.boosting <- renderPlot(NULL)
        output$plot.boosting.import <- renderPlot(NULL)
        codigo.reporte[["modelo.b"]] <<- NULL
        codigo.reporte[["modelo.b.error"]] <<- NULL
        codigo.reporte[["modelo.b.imp"]] <<- NULL
      }, {
        prediccion.boosting <<- NULL
        codigo.reporte[["pred.b"]] <<- NULL
        output$boostingPrediTable <- DT::renderDataTable(NULL)
      }, {
        MC.boosting <<- NULL
        codigo.reporte["mc.b"] <<- NULL
        output$plot.boosting.mc <- renderPlot(NULL)
        output$txtBoostingMC <- renderPrint(invisible(NULL))
      }, {
        indices.boosting <<- rep(0, 4)
        codigo.reporte[["ind.b"]] <<- NULL
      })
    }
  }

  # Ejecuta el modelo, prediccion, mc e indices de knn
  boosting.full <- function() {
    if (length(levels(datos[, variable.predecir])) == 2) {
      ejecutar.boosting()
      ejecutar.boosting.pred()
      ejecutar.boosting.mc()
      ejecutar.boosting.ind()
    }
  }

  # Grafico de boosting
  plotear.boosting <- function() {
    tryCatch({
      output$plot.boosting <- renderPlot(isolate(eval(parse(text = input$fieldCodeBoostingPlot))))
      cod <- ifelse(input$fieldCodeBoostingPlot == "",boosting.plot(),input$fieldCodeBoostingPlot)
      codigo.reporte[["modelo.b.error"]] <<- paste0("## Evolución del Error\n```{r}\n", cod, "\n```")
    }, error = function(e) {
      output$plot.boosting <- renderPlot(NULL)
      codigo.reporte[["modelo.b.error"]] <<- NULL
    })
  }

  # Grafico de importancia
  plotear.boosting.imp <- function() {
    tryCatch({
      output$plot.boosting.import <- renderPlot(isolate(eval(parse(text = input$fieldCodeBoostingPlotImport))))
      cod <- ifelse(input$fieldCodeBoostingPlotImport == "",boosting.plot.import(),input$fieldCodeBoostingPlotImport)
      codigo.reporte[["modelo.b.imp"]] <<- paste0("## Importancia de las Variables \n```{r}\n", cod , "\n```")
    }, error = function(e) {
      output$plot.boosting.import <- renderPlot(NULL)
      codigo.reporte[["modelo.b.imp"]] <<- NULL
    })
  }

  # Genera el modelo
  ejecutar.boosting <- function() {
    tryCatch({ # Se corren los codigo
      isolate(eval(parse(text = cod.b.modelo)))
      output$txtBoosting <- renderPrint(print(modelo.boosting))
      plotear.boosting()
      plotear.boosting.imp()
      codigo.reporte[["modelo.b"]] <<- paste0("## Generación del Modelo ADA-BOOSTING\n```{r}\n", cod.b.modelo, "\nmodelo.boosting\n```")
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.boosting(1)
      showNotification("Error al ejecutar el modelo, intente nuevamente", duration = 15, type = "error")
    }
    )
  }

  # Genera la prediccion
  ejecutar.boosting.pred <- function() {
    tryCatch({ # Se corren los codigo
      isolate(eval(parse(text = cod.b.pred)))
      score.booting <<- predict(modelo.boosting, datos.prueba[, -which(colnames(datos.prueba) == variable.predecir)], type = "prob")
      # Cambia la tabla con la prediccion de boosting
      output$boostingPrediTable <- DT::renderDataTable(obj.predic(prediccion.boosting))
      codigo.reporte[["pred.b"]] <<- paste0("## Predicción del Modelo ADA-BOOSTING\n```{r}\n", cod.b.pred,
                                            "\ndt.to.data.frame.predict(obj.predic(prediccion.boosting))\n```")
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.boosting(2)
      showNotification("Error al ejecutar la prediccion, intente nuevamente", duration = 15, type = "error")
    }
    )
  }

  # Genera la matriz de confusion
  ejecutar.boosting.mc <- function() {
    tryCatch({ # Se corren los codigo
      isolate(eval(parse(text = cod.b.mc)))
      output$txtBoostingMC <- renderPrint(print(MC.boosting))
      isolate(eval(parse(text = plot.MC.code())))
      output$plot.boosting.mc <- renderPlot(isolate(eval(parse(text = "plot.MC(MC.boosting)"))))
      codigo.reporte[["mc.b"]] <<- paste0("## Matriz de Confusión del Modelo ADA-BOOSTING\n```{r}\n", cod.b.mc,
                                          "\nMC.boosting\n```\n\n```{r}\nplot.MC(MC.boosting)\n```")
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.boosting(3)
      showNotification("Error al ejecutar la matriz, intente nuevamente", duration = 15, type = "error")
    }
    )
  }

  # Genera los indices
  ejecutar.boosting.ind <- function() {
    tryCatch({ # Se corren los codigo
      isolate(eval(parse(text = cod.b.ind)))
      indices.boosting <<- indices.generales(MC.boosting)
      indices.g("boosting", MC.boosting)

      codigo.reporte[["ind.b"]] <<- paste0("## Índices Generales\n```{r}\n", cod.b.ind, "\nMC.boosting\n```")

      nombres <- c("boostingPrecGlob", "boostingErrorGlob")
      titulos <- c("Precisión Global", "Error Global")

      fill.gauges(nombres, titulos, indices.boosting)

      # Cambia la tabla con la indices de boosting
      output$boostingIndPrecTable <- shiny::renderTable(xtable(indices.prec.table(indices.boosting,"ADA-BOOSTING")),
                                                  bordered = T, width = "100%", align = "c", digits = 2)

      output$boostingIndErrTable <- shiny::renderTable(xtable(indices.error.table(indices.boosting,"ADA-BOOSTING")),
                                                 bordered = T, width = "100%", align = "c", digits = 2)
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.boosting(4)
      showNotification("Error al ejecutar los indices, intente nuevamente", duration = 15, type = "error")
    }
    )
  }

  # TABLA COMPARATIVA -------------------------------------------------------------------------------------------------------

  tabla.comparativa <- function() {
    tryCatch({
      matrices <- list("KNN" = MC.knn, "SVM" = MC.svm, "ÁRBOLES" = MC.dt, "BOSQUES" = MC.rf, "ADA-BOOSTING" = MC.boosting)
      areas <- list("KNN" = area.knn, "SVM" = area.svm, "ÁRBOLES" = area.dt, "BOSQUES" = area.rf, "ADA-BOOSTING" = area.boosting)
      matrices <- matrices[c("sel.knn", "sel.svm", "sel.dt", "sel.rf", "sel.boosting") %in% input$select.models]
      areas <- areas[c("sel.knn", "sel.svm", "sel.dt", "sel.rf", "sel.boosting") %in% input$select.models]
      cant.class <- length(unique(datos[, variable.predecir]))
      names.class <- as.character(unique(datos[, variable.predecir]))

      if (length(matrices) == 0) {
        return(data.frame())
      }

      df <- NULL
      for (i in 1:length(matrices)) {
        if (is.null(matrices[[i]])) {
          df <- rbind(df, c(names(matrices)[i], NA, rep(NA, cant.class), NA))
        } else {
          df <- rbind(df, c(names(matrices)[i], round(c(
            (sum(diag(matrices[[i]])) / sum(matrices[[i]])) * 100,
            diag(matrices[[i]]) / rowSums(matrices[[i]]) * 100,
            areas[[i]]
          ), 2)))
        }
      }
      colnames(df) <- c("Modelo", "Precisión Global", names.class, "Área de ROC")
      return(df)
    }, error = function() {
      return(data.frame())
    })
  }

  calcular.areas <- function() {
    clase <- datos.prueba[, variable.predecir]
    if (length(unique(clase)) == 2) {
      if (is.numeric(score.knn)) {
        area.knn <<- areaROC(score.knn[, input$roc.sel], clase)
      }
      if (is.factor(score.svm)) {
        area.svm <<- areaROC(attributes(score.svm)$probabilities[, input$roc.sel], clase)
      }
      if (is.numeric(score.dt)) {
        area.dt <<- areaROC(score.dt[, input$roc.sel], clase)
      }
      if (is.numeric(score.rf)) {
        area.rf <<- areaROC(score.rf[, input$roc.sel], clase)
      }
      if (is.numeric(score.booting)) {
        area.boosting <<- areaROC(score.booting[, which(levels(clase) == input$roc.sel)], clase)
      }
    }
  }

  observeEvent(c(input$select.models.roc, input$roc.sel), {
    if (!is.null(datos.prueba) & length(levels(datos[,variable.predecir])) == 2 ) {
      calcular.areas()
      output$plot.roc <- renderPlot(plotROC(input$select.models.roc))
      codigo.reporte[["roc"]] <<- paste0("## Curva ROC \n```{r}\nplotROC(input$select.models.roc)\n```")
    } else {
      output$plot.roc <- renderPlot(NULL)
      codigo.reporte[["roc"]] <<- NULL
    }
  })

  observeEvent(c(input$runKnn, input$runSvm, input$runDt, input$runRf, input$runBoosting, input$select.models, input$roc.sel), {
    if (!is.null(datos.prueba)) {
      calcular.areas()
      output$TablaComp <- DT::renderDataTable(DT::datatable(tabla.comparativa(),
        selection = "none",
        editable = FALSE,
        extensions = c("Responsive"),
        options = list(
          dom = "frtip",
          pageLength = 10,
          buttons = NULL,
          scrollY = T
        )
      ))
      codigo.reporte[["tabla.comparativa"]] <<- paste0("## Tabla Comparativa \n```{r}\ncalcular.areas()\ntabla.comparativa()\n```")
    }
  })

  # PAGINA DE REPORTE -------------------------------------------------------------------------------------------------------

  observeEvent(input$btnReporte, {
    updateAceEditor(session, "fieldCodeReport", value = def.reporte(titulo = input$textTitulo, nombre = input$textNombre, input))
  })

  obj.reporte <- eventReactive(input$fieldCodeReport, {
    updateAceEditor(session, "fieldCodeReport", value = input$fieldCodeReport)
    return(isolate(HTML(knit2html(text = input$fieldCodeReport, fragment.only = T, quiet = T))))
  })

  output$knitDoc <- renderUI(obj.reporte())

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
      out <- rmarkdown::render(src,  params = NULL, rmarkdown::word_document(), encoding = "utf8")
      file.rename(out, paste('data-', Sys.Date(), '.docx', sep=''))
      files <- c(paste('data-', Sys.Date(), '.docx', sep=''), files)

      zip::zip(file, files)
    }
  )


  # Termina la Sesion -------------------------------------------------------------------------------------------------------

  session$onSessionEnded(function() {
    rm(envir = .GlobalEnv, list = ls(envir = .GlobalEnv))
    stopApp()
  })

})
