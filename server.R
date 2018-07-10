#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) { 
  options(shiny.maxRequestSize=200*1024^2)
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  observeEvent(input$loadButton, {
    codigo <- code.carga(nombre.filas = input$columname, ruta = input$file1$datapath,
                         separador = input$sep, sep.decimal = input$dec, encabezado = input$header)
    updateAceEditor(session, "fieldCodeData", value = codigo)
    
    tryCatch({
      isolate(eval(parse(text = codigo)))
    }, error = function(e) {
      print(paste0("ERROR EN CARGAR: ", e))
      datos <<- NULL
      datos.originales <<- NULL
      return(NULL)
    })
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
  })
  
  update <- reactive({
    inFile <- c(input$loadButton, input$transButton)
    datos
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
