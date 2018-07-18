
###########################################################################################################################
##### FUNCIONES GLOBALES
###########################################################################################################################

#Colores de ggplot2
gg_color_hue <- function(n) {
  hues <- seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


colnames.empty <- function(res){
  res <- colnames(res)
  if(is.null(res))
    return("")
  return(res)
}

#Obtiene solo las variables numericas
var.numericas <- function(data){
  if(is.null(data)) return(NULL)
  res <- base::subset(data, select = sapply(data, class) %in% c('numeric', 'integer'))
  return(res)
}

#Obtiene solo las variables categoricas
var.categoricas <- function(data){
  if(is.null(data)) return(NULL)
  res <- base::subset(data, select = !sapply(data, class) %in% c('numeric', 'integer'))
  return(res)
}

#Transforma las variables a disyuntivas
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

#Genera el codigo para cargar datos
code.carga <- function(nombre.filas = T, ruta = NULL, separador = ";", sep.decimal = ",", encabezado = T){
  browser()
  if(!is.null(ruta)){
    ruta <-  gsub("\\", "/", ruta, fixed=TRUE)
  }
  if(nombre.filas){
    return(paste0("datos.originales <<- read.table('", ruta, "', header=",
                  encabezado, ", sep='", separador, "', dec = '", sep.decimal, "', row.names = 1) \ndatos <<- datos.originales"))
  } else {
    return(paste0("datos.originales <<- read.table('", ruta, "', header=", encabezado, ", sep='", separador, "', dec = '", sep.decimal,
                  "') \ndatos <<- datos.originales"))
  }
}

#Genera el codigo para transformar datos
code.trans <- function(variable, nuevo.tipo){
  if(nuevo.tipo == "categorico"){
    return(paste0("datos[, '", variable, "'] <<- as.factor(datos[, '", variable, "'])"))
  } else if(nuevo.tipo == "numerico") {
    return(paste0("datos[, '", variable, "'] <<- as.numeric(datos[, '", variable, "'])"))
  } else {
    es.factor <- ifelse(class(datos.originales[, variable]) %in% c('numeric', 'integer'),
                        paste0("datos[, '", variable, "'] <<- as.factor(datos[, '", variable, "']) \n"), "")
    return(paste0(es.factor, "datos <<- datos.disyuntivos(datos, '", variable,"')"))
  }
}

#Desactiva las variables seleccionadas de los datos
code.desactivar <- function(variables){
  return(paste0("datos <<- subset(datos, select = -c(", paste(variables, collapse = ","), "))"))
}

#Genera el resumen numerico de una variable
resumen.numerico <- function(data, variable){
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

#Genera el resumen categorico de una variable
resumen.categorico <- function(data, variable){
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

default.normal <- function(data = "datos", vars = NULL, color = "#00FF22AA"){
  if(is.null(vars)){
    return(NULL)
  } else {
    return(paste0("hist(", data, "[, '", vars, "'], col = '", color,
                  "', border=F, main = paste0('Test de normalidad de la variable ','", vars,"'), axes=F, freq = F)
axis(1, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025)
axis(2, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025)
curve(dnorm(x, mean = mean(", data, "[, '", vars, "']), sd = sd(", data, "[, '", vars, "'])), add=T, col='blue', lwd=2)
legend('bottom', legend = 'Curva Normal', col = 'blue', lty=1, cex=1.5)"))
  }
}

default.disp <- function(data = "datos", vars = NULL, color = "#FF0000AA"){
  if(length(vars) < 2) {
    return(NULL)
  } else if(length(vars) == 2) {
    return(paste0("ggplot(data = ", data, ", aes(x = ", vars[1], ", y = ", vars[2], ", label = rownames(", data, "))) +
                  geom_point(color = '", color, "', size = 3) + geom_text(vjust = -0.7)"))
  } else{
    return(paste0("scatterplot3d(", data, "[, '", vars[1], "'], ", data, "[, '",
                  vars[2], "'], ", data, "[, '", vars[3], "'], pch = 16, color = '", color, "')"))
  }
}

def.model <- function(data = "datos", cant = "as.numeric(input$cant.cluster)", dist.method = "euclidean", hc.method = "complete"){
  return(paste0("hc.modelo <<- hclust(dist(var.numericas(", data, "), method = '", dist.method, "'), method = '", hc.method, "')
                centros <<- calc.centros(var.numericas(", data, "), hc.modelo, ", cant, ")"))
}

#Calcula la matriz de correlacion
modelo.cor <- function(data = "datos"){
  return(paste0("correlacion <<- cor(var.numericas(", data, "))"))
}

#Crea el código de la particion en testing y learning data
particion.code <- function(data = "datos", p = "0.5", variable = NULL){
  variable.predecir <<- variable
  return(paste0("particion <- createDataPartition(datos$",variable,", p = ",p/100,", list = FALSE)\n
datos.prueba <<- datos[-particion,]\ndatos.aprendizaje <<- datos[particion,]"))
}

#Crea el modelo KNN
kkn.modelo <- function(variable.pr = NULL, predictoras = ".", scale = TRUE,kmax = 7, kernel = "optimal"){
  if(all(predictoras == ""))
    predictoras <- "."
  predictoras <- paste0(predictoras, collapse = "+")
  codigo <- paste0("modelo.knn <<- train.kknn(",variable.pr,"~",predictoras,", data = datos.aprendizaje,scale =",scale,", kmax=",kmax,", kernel = '",kernel,"')")
  return(codigo)
}

kkn.prediccion <- function() {
  return(paste0("prediccion.knn <<- predict(modelo.knn, datos.prueba)"))
}

knn.MC <- function(variable.p){
  return(paste0("knn.MC <- table(datos.prueba$",variable.p,", prediccion.knn)"))
}

correlaciones <- function(metodo = 'circle', tipo = "lower"){
  return(paste0("corrplot(correlacion, method='", metodo,"', shade.col=NA, tl.col='black',
                tl.srt=20, addCoef.col='black', order='AOE', type = '", tipo, "')"))
}

def.code.num <- function(data = "datos", variable = "input$sel.distribucion", color = 'input$col.dist'){
  return(paste0("distribucion.numerico(", data, "[, ", variable, "], ", variable, ", color = ", color,")"))
}

def.code.cat <- function(data = "datos", variable = "input$sel.distribucion", color = 'input$col.dist'){
  return(paste0("distribucion.categorico(", data, "[, ", variable,"], color = ", color, ")"))
}

default.func.num <- function(){
  return(paste0("distribucion.numerico <<- function(var, nombre.var, color){
                nf <- graphics::layout(mat = matrix(c(1, 2), 2, 1, byrow=TRUE),  height = c(3,1))
                par(mar=c(3.1, 3.1, 1.1, 2.1))
                hist(var, col = color, border=F, main = paste0('Distribución y atipicidad de la variable ', nombre.var), axes=F)
                axis(1, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025)
                axis(2, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025)
                boxplot(var, col = color, boxcol = color, boxlty = 1, boxlwd = 3, boxwex = 1.5,
                edcol = color, medlty = 1, medlwd = 8, medcol = color, whiskcol = color, whisklty = 3,
                staplecol = color, staplelty = 1, staplelwd = 3, horizontal=TRUE, outline=TRUE,
                frame=F, whisklwd = 2.5, outpch = 20, outcex = 1.5, outcol = 'red', axes=F)
}"))
}

default.func.cat <- function(){
  return(paste0("distribucion.categorico <<- function(var, color = 'input$col.dist'){
                colores <- sapply(c(1:length(levels(var))), function(i) rgb(sample(0:255, 1), sample(0:255, 1), sample(0:255, 1), 180, maxColorValue = 255))
                data <- data.frame(label = levels(var), value = summary(var))
                ggplot(data, aes(label, value)) +
                geom_bar(stat = 'identity', fill = colores) +
                geom_text(aes(label = value, y = value), vjust = -0.5, size = 4) +
                theme_minimal() +
                labs(title = 'Distribución', y = 'Cantidad de casos', x = 'Categorias')
}"))
}

def.reporte <- function(){
  return(paste0("---
                title: 'Untitled'
                author: 'PROMIDAT'
                date: ", Sys.Date(), "
                output:
                html_document:
                df_print: paged
                ---

                ```{r setup, include=FALSE}
                knitr::opts_chunk$set(echo = TRUE)
                ```

                # Carga de Paquetes
                ```{r message=FALSE, warning=FALSE}
                library(promises)
                library(ggplot2)
                library(FactoMineR)
                library(factoextra)
                library(reshape)
                library(corrplot)
                library(dendextend)
                library(scatterplot3d)
                library(stringr)
                ```

                # Funciones

                ```{r}
                var.numericas <- function(data){
                if(is.null(data)) return(NULL)
                res <- subset(data, select = sapply(data, class) %in% c('numeric', 'integer'))
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

                ", default.func.num(), "

                ", default.func.cat(), "
                ```"))
}

cod.resum <- function(data = "datos"){
  return(paste0("summary(", data, ")"))
}

###########################################################################################################################
##### VARIABLES GLOBALES
###########################################################################################################################


datos <<- NULL  # Los datos cargados pueden estar transformados
datos.originales <<- NULL # Los datos cargados originales
correlacion <<- NULL

datos.prueba <<- NULL
datos.aprendizaje <<- NULL
variable.predecir <<- NULL
prediccion.knn <<- NULL
# def.colores <<- gg_color_hue(10)

cod.disp <- default.disp()
cod.cor <- correlaciones()

cod.dya.cat <- def.code.cat()
cod.dya.num <- def.code.num()
func.dya.num <- default.func.num()
func.dya.cat <- default.func.cat()


# ---

modelo.knn <<- NULL

# cod.mod.knn <- knn.modelo()



