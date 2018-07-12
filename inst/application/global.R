datos <<- NULL
datos.originales <<- NULL


resumen.numerico <- function(data, variable){
  browser()
  salida <- ""
  datos.numericos <- list(Q1 = list(id = "q1", Label = "Primer Cuartil",
                                    Value = format(quantile(data[, variable], .25), scientific = FALSE), color = "green"),
                          Mediana = list(id = "mediana", Label = "Mediana",
                                         Value = format(median(data[, variable]), scientific = FALSE), color = "orange"),
                          Q3 = list(id = "q3", Label = "Tercer Cuartil",
                                    Value = format(quantile(data[, variable], .75), scientific = FALSE), color = "maroon"),
                          Minimo = list(id = "minimo", Label = "Mínimo",
                                        Value = format(min(data[, variable]), scientific = FALSE), color = "red"),
                          Promedio = list(id = "promedio", Label = "Promedio",
                                          Value = format(mean(data[, variable]), scientific = FALSE), color = "blue"),
                          Maximo = list(id = "maximo", Label = "Máximo",
                                        Value = format(max(data[, variable]), scientific = FALSE), color = "purple"),
                          DS <- list(id = "ds", Label = "Desviación Estandar",
                                     Value = format(max(data[, variable]), scientific = FALSE), color = "yellow"))
  
  for (calculo in datos.numericos) {
    salida <- paste0(salida, "<div class='shiny-html-output col-sm-6 shiny-bound-output' id='", calculo$id,
                     "'> <div class='small-box bg-", calculo$color,"'> <div class='inner'>",
                     "<h3>", calculo$Value, "</h3> <p>", calculo$Label, "</p></div> <div class='icon-large'> <i class='",
                     calculo$icon, "'></i></div></div></div>")
  }
  return(salida)
}

resumen.categorico <- function(data, variable){
  browser()
  salida <- ""
  color <- c("red","yellow","aqua","navy","teal","olive","purple","maroon","black","blue","lime","orange","light-blue","green","fuchsia")
  datos.categoricos <- summary(data[, variable])
  for (i in 1:length(datos.categoricos)) {
    salida <- paste0(salida, "<div class='shiny-html-output col-sm-6 shiny-bound-output' id='", variable, i,
                     "'> <div class='small-box bg-", sample(color, 1), "'> <div class='inner'>",
                     "<h3>", datos.categoricos[i], "</h3> <p>", levels(data[, variable])[i],
                     "</p></div> <div class='icon-large'> <i class=''></i></div></div></div>")
  }
  return(salida)
}


var.numericas <- function(data){
  if(is.null(data)) return(NULL)
  res <- base::subset(data, select = sapply(data, class) %in% c('numeric', 'integer'))
  return(res)
}

var.categoricas <- function(data){
  if(is.null(data)) return(NULL)
  res <- base::subset(data, select = !sapply(data, class) %in% c('numeric', 'integer'))
  return(res)
}

datos.disyuntivos <- function(data, vars){
  if(is.null(data)) return(NULL)
  cualitativas <- base::subset(data, select = colnames(data) %in% c(vars))
  data <- data[, !colnames(data) %in% vars]
  for (variable in colnames(cualitativas)) {
    for (categoria in unique(cualitativas[, variable])) {
      nueva.var <- as.numeric(cualitativas[, variable] == categoria)
      data <- cbind(data, nueva.var)
      colnames(data)[length(colnames(data))] <- paste0(variable, '.', categoria)
    }
  }
  return(data)
}

code.carga <- function(nombre.filas = T, ruta = NULL, separador = ";", sep.decimal = ",", encabezado = T){
  if(nombre.filas){
    return(paste0("datos.originales <<- read.table('", ruta, "', header=", 
                  encabezado, ", sep='", separador, "', dec = '", sep.decimal, "', row.names = 1) \ndatos <<- datos.originales"))
  } else {
    return(paste0("datos.originales <<- read.table('", ruta, "', header=", encabezado, ", sep='", separador, "', dec = '", sep.decimal, 
                  "') \ndatos <<- datos.originales"))
  }
}

code.trans <- function(variables, nuevo.tipo){
  res <- ""
  if(nuevo.tipo == "categorico"){
    for (variable in variables) {
      res <- paste0(res, "datos[, '", variable, "'] <<- as.factor(datos[, '", variable, "'])")
    }
  } else {
    res <- paste0("datos <<- datos.disyuntivos(datos, '", variables,"')")
  }
  return(res)
}

code.desactivar <- function(variables){
  return(paste0("datos <<- subset(datos, select = -c(", paste(variables, collapse = ","), "))"))
}