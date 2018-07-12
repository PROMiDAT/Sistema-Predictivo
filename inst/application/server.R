
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) { 
  options(shiny.maxRequestSize=200*1024^2)
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  obj.resum <- eventReactive(c(input$loadButton, input$transButton), {
    data.frame(unclass(summary(datos)), check.names = FALSE, stringsAsFactors = FALSE)
  })
  
  observeEvent(input$loadButton, {
    codigo <- code.carga(nombre.filas = input$columname, ruta = input$file1$datapath,
                         separador = input$sep, sep.decimal = input$dec, encabezado = input$header)
    updateAceEditor(session, "fieldCodeData", value = codigo)
    updateAceEditor(session, "fieldCodeResum", value = "summary(datos)")
    
    
    tryCatch({
      isolate(eval(parse(text = codigo)))
    }, error = function(e) {
      print(paste0("ERROR EN CARGAR: ", e))
      datos <<- NULL
      datos.originales <<- NULL
      return(NULL)
    })
    updateSelectInput(session, "sel.resumen", choices = colnames(datos))
  })
  
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
      } else {
        var.noactivas <- c(var.noactivas, var)
      }
    }
    
    isolate(eval(parse(text = code.res)))
    if(length(var.noactivas) > 0)
      isolate(eval(parse(text = code.desactivar(var.noactivas))))
    
    updateAceEditor(session, "fieldCodeTrans", value = code.res)
    browser()
    updateSelectInput(session, "sel.resumen", choices = colnames(datos))
  })
  
  update <- reactive({
    inFile <- c(input$loadButton, input$transButton)
    datos
  })
  
  output$resumen = renderUI({
    if(input$sel.resumen %in% colnames(var.numericas(datos))){
      HTML(resumen.numerico(datos, input$sel.resumen))
    } else {
      HTML(resumen.categorico(datos, input$sel.resumen))
    }
  })
  
  output$contents = DT::renderDT(update(), selection = 'none', server = FALSE, editable = TRUE)
  
  update.trans <- reactive({
    inFile <- c(input$loadButton)
    n <- ncol(datos)
    res <-  data.frame(Variables = colnames(datos), Tipo = c(1:n), Activa = c(1:n))
    res$Tipo <- sapply(colnames(datos), function(i) paste0('<select id="sel', i, '"> <option value="categorico">Categórico</option>
                                                           <option value="numerico" ', ifelse(class(datos[, i]) %in% c("numeric","integer"), ' selected="selected"', ''),
                                                           '>Numérico</option> </select>'))
    res$Activa <- sapply(colnames(datos), function(i) paste0('<input type="checkbox" id="box', i, '" checked/>'))
    return(res)
  })
  
  output$transData = DT::renderDataTable(update.trans(), escape = FALSE, selection = 'none', server = FALSE,
                                         options = list(dom = 't', paging = FALSE, ordering = FALSE), rownames = F,
                                         callback = JS("table.rows().every(function(i, tab, row) {
                                                       var $this = $(this.node());
                                                       $this.attr('id', this.data()[0]);
                                                       $this.addClass('shiny-input-checkbox');});
                                                       Shiny.unbindAll(table.table().node());
                                                       Shiny.bindAll(table.table().node());"))
  
  
  
  
  output$resumen.completo = DT::renderDataTable({
    return(obj.resum())
  }, options = list(dom = 'ft', scrollX = TRUE), rownames = F)
  
  observeEvent(c(input$switch.scale, input$slider.npc), {
    tryCatch({
      updateAceEditor(session, "fieldCodePCAModelo", value = def.pca.model(scale.unit = input$switch.scale, npc = input$slider.npc))
      isolate(eval(parse(text = def.pca.model(scale.unit = input$switch.scale, npc = input$slider.npc))))
    }, error = function(e) {
      print(paste0("ERROR EN PCA: ", e))
      return(NULL)
    })
  })
})
