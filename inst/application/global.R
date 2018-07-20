
###########################################################################################################################
##### FUNCIONES GLOBALES
###########################################################################################################################


#Colores de ggplot2
gg_color_hue <- function(n) {
  hues <- seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

#Obtiene los nombres de columnas o regresa un string vacio
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

#Codigo del calculo de los indices
cod.indices <- function(){
  return('indices.generales <- function(MC) {
  if (1 == dim(MC)[2]) {
    MC <- cbind(MC, 0)
  }

  precision.global <- (sum(diag(MC)) / sum(MC)) * 100
  error.global <- (1 - (sum(diag(MC)) / sum(MC))) * 100
  precision.positiva <- ifelse(ncol(MC) == 2, MC[2, 2] / sum(MC[2, ]) , 0) * 100
  precision.negativa <- ifelse(ncol(MC) == 2, MC[1, 1] / sum(MC[1, ]) , 0) * 100
  falsos.positivos   <- ifelse(ncol(MC) == 2, MC[1, 2] / sum(MC[1, ]) , 0) * 100
  falsos.negativos   <- ifelse(ncol(MC) == 2, MC[2, 1] / sum(MC[2, ]) , 0) * 100
  asertividad.positiva <- ifelse(ncol(MC) == 2,  MC[2, 2] / sum(MC[, 2]) , 0) * 100
  asertividad.negativa <- ifelse(ncol(MC) == 2, MC[1, 1] / sum(MC[, 1]) , 0) * 100

  res <- list( precision.global = precision.global,
               error.global = error.global,
               precision.positiva = precision.positiva,
               precision.negativa = precision.negativa,
               falsos.positivos = falsos.positivos,
               falsos.negativos = falsos.negativos,
               asertividad.positiva = asertividad.positiva,
               asertividad.negativa = asertividad.negativa)
  return(res)
}')
}

# -------------------  Pagina Datos ------------------------ #

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

#Eliminar NAs
code.NA <- function(deleteNA = T) {
  res <- ifelse(deleteNA, "datos <<- na.omit(datos)",
                paste0("for (variable in colnames(datos)) {\n",
                       "  if(any(is.na(datos[, variable]))){\n",
                       "    ifelse(class(datos[, variable]) %in% c('numeric', 'integer'),\n",
                       "           datos[, variable][is.na(datos[, variable])] <<- mean(datos[, variable], na.rm = T),\n",
                       "           datos[, variable][is.na(datos[, variable])] <<- modeest::mfv(datos[, variable], na.rm = T))",
                       "\n   }\n}"))
  return(res)
}

#Crea el código de la particion en testing y learning data
particion.code <- function(data = "datos", p = "0.5", variable = NULL, semilla = 5){
  variable.predecir <<- variable
  semilla <- ifelse(!is.numeric(semilla), 5, semilla)
  return(paste0("set.seed(",semilla,")\nparticion <- createDataPartition(datos$",variable,", p = ",p/100,", list = FALSE)\n
datos.prueba <<- datos[-particion,]\ndatos.aprendizaje <<- datos[particion,]"))
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

# -------------------  Estadisticas Basicas ------------------------ #

#Resumen Completo
cod.resum <- function(data = "datos"){
  return(paste0("summary(", data, ")"))
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

#Calcula la matriz de correlacion
modelo.cor <- function(data = "datos"){
  return(paste0("correlacion <<- cor(var.numericas(", data, "))"))
}

#Codigo de la generacion de correlaciones
correlaciones <- function(metodo = 'circle', tipo = "lower"){
  return(paste0("corrplot(correlacion, method='", metodo,"', shade.col=NA, tl.col='black',
                tl.srt=20, addCoef.col='black', order='AOE', type = '", tipo, "')"))
}

#Codigo de la genracion de la curva normal (test de normalidad)
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

#Codigo del grafico de dispersion
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

# -------------------  KNN ------------------------ #

#Crea el modelo KNN
kkn.modelo <- function(variable.pr = NULL, predictoras = ".", scale = TRUE,kmax = 7, kernel = "optimal"){
  if(all(predictoras == ""))
    predictoras <- "."
  predictoras <- paste0(predictoras, collapse = "+")
  codigo <- paste0("modelo.knn <<- train.kknn(",variable.pr,"~",predictoras,", data = datos.aprendizaje,scale =",scale,", kmax=",kmax,", kernel = '",kernel,"')")
  return(codigo)
}

#Codigo de la prediccion de knn
kkn.prediccion <- function() {
  return(paste0("prediccion.knn <<- predict(modelo.knn, datos.prueba)"))
}

#Codigo de la matriz de confucion de knn
knn.MC <- function(variable.p){
  return(paste0("MC.knn <<- table(datos.prueba$",variable.p,", prediccion.knn)"))
}

# -------------------  Bayes ------------------------ #

#Crea el modelo bayes
bayes.modelo <- function(variable.pr = NULL, predictoras = "."){
  if(all(predictoras == ""))
    predictoras <- "."
  predictoras <- paste0(predictoras, collapse = "+")
  codigo <- paste0("modelo.bayes <<- naiveBayes(",variable.pr,"~",predictoras,", data = datos.aprendizaje)")
  return(codigo)
}

#Codigo de la prediccion de bayes
bayes.prediccion <- function(variable.pr = NULL) {
  return(paste0("prediccion.bayes <<- predict(modelo.bayes, datos.prueba[,-which(colnames(datos.prueba) == '",variable.pr,"')])"))
}

#Codigo de la matriz de confucion de bayes
bayes.MC <- function(variable.p){
  return(paste0("MC.bayes <<- table(datos.prueba$",variable.p,", prediccion.bayes)"))
}

# -------------------  SVM ------------------------ #

#Crea el modelo SVM
svm.modelo <- function(variable.pr = NULL, predictoras = ".", scale = TRUE, kernel = "linear"){
  if(all(predictoras == ""))
    predictoras <- "."
  predictoras <- paste0(predictoras, collapse = "+")
  codigo <- paste0("modelo.svm <<- svm(",variable.pr,"~",predictoras,", data = datos.aprendizaje, scale =",scale,", kernel = '",kernel,"')")
  return(codigo)
}

#Codigo de la prediccion de svm
svm.prediccion <- function() {
  return(paste0("prediccion.svm <<- predict(modelo.svm, datos.prueba)"))
}

#Codigo de la matriz de confucion de svm
svm.MC <- function(variable.p){
  return(paste0("MC.svm <<- table(datos.prueba$",variable.p,", prediccion.svm)"))
}

#Codigo del grafico de svm
svm.plot <- function(variables, resto){
  if(is.null(variables)) return("")
  l <- c()
  for(i in 1:length(resto)){
    l <- c(l , paste0(resto[i],"=",i))
  }
  l <- paste0("list(",paste0(l,collapse = ","),")")
  return(paste0("plot(modelo.svm, datos, ",variables[1],"~",variables[2],", slice = ",l,")"))
}


# -------------------  DT ------------------------ #

#Crea el modelo DT
dt.modelo <- function(variable.pr = NULL, predictoras = ".", minsplit =  20){
  if(all(predictoras == ""))
    predictoras <- "."
  predictoras <- paste0(predictoras, collapse = "+")
  codigo <- paste0("modelo.dt <<- rpart(",variable.pr,"~",predictoras,", data = datos.aprendizaje, control = rpart.control(minsplit = ",minsplit,"))")
  return(codigo)
}

#Codigo de la prediccion de DT
dt.prediccion <- function() {
  return(paste0("prediccion.dt <<- predict(modelo.dt, datos.prueba, type='class')"))
}

#Codigo de la matriz de confucion de dt
dt.MC <- function(variable.p){
  return(paste0("MC.dt <<- table(datos.prueba$",variable.p,", prediccion.dt)"))
}


# -------------------  RF ------------------------ #

#Crea el modelo RF
rf.modelo <- function(variable.pr = NULL, predictoras = ".", ntree = 500){
  if(all(predictoras == ""))
    predictoras <- "."
  predictoras <- paste0(predictoras, collapse = "+")
  ntree <- ifelse(!is.numeric(ntree), 500, ntree)
  codigo <- paste0("modelo.rf <<- randomForest(",variable.pr,"~",predictoras,", data = datos.aprendizaje,importance = TRUE, ntree =",ntree,")")
  return(codigo)
}

#Codigo de la prediccion de rf
rf.prediccion <- function(variable.pr = NULL) {
  return(paste0("prediccion.rf <<- predict(modelo.rf,datos.prueba[,-which(colnames(datos.prueba) == '",variable.pr,"')])"))
}

#Codigo de la matriz de confucion de rf
rf.MC <- function(variable.p){
  return(paste0("MC.rf <<- table(datos.prueba$",variable.p,", prediccion.rf)"))
}

# -------------------  BOOSTING ------------------------ #

#Crea el modelo BOOSTING
boosting.modelo <- function(variable.pr = NULL, predictoras = ".", iter = 50, nu = 1, type = "discrete"){
  if(all(predictoras == ""))
    predictoras <- "."
  predictoras <- paste0(predictoras, collapse = "+")
  iter <- ifelse(!is.numeric(iter), 50, iter)
  nu <- ifelse(!is.numeric(nu), 1, nu)
  codigo <- paste0("modelo.boosting <<- ada(",variable.pr,"~",predictoras,", data = datos.aprendizaje, iter = ",iter,", nu = ",nu,", type = '",type,"')")
  return(codigo)
}

#Codigo de la prediccion de boosting
boosting.prediccion <- function(variable.pr = NULL) {
  return(paste0("prediccion.boosting <<- predict(modelo.boosting, datos.prueba[,-which(colnames(datos.prueba) == '",variable.pr,"')])"))
}

#Codigo de la matriz de confucion de boosting
boosting.MC <- function(variable.p){
  return(paste0("MC.boosting <<- table(datos.prueba$",variable.p,", prediccion.boosting)"))
}

# -------------------  Reporte ------------------------ #

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

plot.MC.code <- function(cm) {
return("
plot.MC <<- function(cm){
  par(mar=c(2,2,2,2))
       plot(c(1, 500), c(1, 500), type = 'n', xlab='', ylab='', xaxt='n', yaxt='n')
       title('Matriz de Confusión', cex.main=2)

       start <- 70
       len <- 500 - start

       n.class <- ncol(cm) + 2
       names.class <- c(colnames(cm),'Precisión', 'Error')
       prec.cat <- diag(cm)/rowSums(cm)
       error.cat <- 1 - prec.cat

       ancho <- len / n.class
       alto  <- len / (n.class-2)
       x2 <- (x1 <- start) + ancho
       y2 <- (y1 <- len) - alto

       text(310, 485, 'Predición', cex=1.3, font=2)
       text(start-55, 250, 'Real', cex=1.3, srt=90, font=2)

       for (i in 0:(n.class-3)) {
       for (j in 0:(n.class-1)) {
       x1.aux <- x1 + j*(ancho + 3)
       y1.aux <- y1 - i*(alto + 5)
       x2.aux <- x2 + j*(ancho + 3)
       y2.aux <- y2 - i*(alto + 5)
       if(j < (n.class-2)){
       rect(x1.aux, y1.aux, x2.aux, y2.aux, col=ifelse(i==j,'#3f72af','#11999e'))
       text(mean(c(x1.aux,x2.aux)) , mean(c(y1.aux,y2.aux)), cm[(i+1),(j+1)], cex=1.6, font=2, col='white')
       }
       if (j == (n.class-2)){
       rect(x1.aux, y1.aux, x2.aux, y2.aux, col= '#e4f9f5' )
       text(mean(c(x1.aux,x2.aux)) , mean(c(y1.aux,y2.aux)), paste0(round(prec.cat[i+1] * 100,2), '%'), cex=1.6, font=2, col='black')
       }
       if (j == (n.class-1)){
       rect(x1.aux, y1.aux, x2.aux, y2.aux, col= '#e4f9f5' )
       text(mean(c(x1.aux,x2.aux)) , mean(c(y1.aux,y2.aux)), paste0(round(error.cat[i+1] * 100,2), '%'), cex=1.6, font=2, col='black')
       }
       }
       text( mean( c((x2 + i*(ancho + 3)) , (x1 + i*(ancho + 3)) )), y1 + 20, names.class[i+1], cex=1.2)
       text( x1-20,mean(c((y1 - i*(alto + 5)) , (y2 - i*(alto + 5))  )), names.class[i+1], cex=1.2)
       }
       text( mean( c((x2 + (i+1)*(ancho + 3)) , (x1 + (i+1)*(ancho + 3)) )), y1 + 20, names.class[i+2], cex=1.2)
       text( mean( c((x2 + (i+2)*(ancho + 3)) , (x1 + (i+2)*(ancho + 3)) )), y1 + 20, names.class[i+3], cex=1.2)
       text( mean( c((x2 + (i+3)*(ancho + 3)) , (x1 + (i+3)*(ancho + 3)) )), y1 + 20, names.class[i+4], cex=1.2)
}")
}

###########################################################################################################################
##### VARIABLES GLOBALES
###########################################################################################################################

# -------------------  Datos ------------------------ #

datos <<- NULL  # Los datos cargados pueden estar transformados
datos.originales <<- NULL # Los datos cargados originales
datos.prueba <<- NULL
datos.aprendizaje <<- NULL
variable.predecir <<- NULL

# -------------------  Estadisticas Basicas ------------------------ #


correlacion <<- NULL

cod.disp <- default.disp()
cod.cor <- correlaciones()

cod.dya.cat <- def.code.cat()
cod.dya.num <- def.code.num()
func.dya.num <- default.func.num()
func.dya.cat <- default.func.cat()

# -------------------  KNN ------------------------ #

modelo.knn <<- NULL
MC.knn <<- NULL
prediccion.knn <<- NULL
indices.knn <<- rep(0,8)

# -------------------  Bayes ------------------------ #

modelo.bayes <<- NULL
MC.bayes <<- NULL
prediccion.bayes <<- NULL
indices.bayes <<- rep(0,8)

# -------------------  SVM ------------------------ #

modelo.svm <<- NULL
MC.svm <<- NULL
prediccion.svm <<- NULL
indices.svm <<- rep(0,8)

# -------------------  DT ------------------------ #

modelo.dt <<- NULL
MC.dt <<- NULL
prediccion.dt <<- NULL
indices.dt <<- rep(0,8)

# -------------------  RF ------------------------ #

modelo.rf <<- NULL
MC.rf <<- NULL
prediccion.rf <<- NULL
indices.rf <<- rep(0,8)

# -------------------  BOOSTING ------------------------ #

modelo.boosting <<- NULL
MC.boosting <<- NULL
prediccion.boosting <<- NULL
indices.boosting <<- rep(0,8)



