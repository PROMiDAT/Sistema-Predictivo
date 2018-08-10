
# FUNCIONES GLOBALES --------------------------------------------------------------------------------------------------------

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
  precision.positiva <- ifelse(ncol(MC) == 2, MC[2, 2] / sum(MC[2,]),0) * 100
  precision.negativa <- ifelse(ncol(MC) == 2, MC[1, 1] / sum(MC[1,]),0) * 100
  falsos.positivos   <- ifelse(ncol(MC) == 2, MC[1, 2] / sum(MC[1,]),0) * 100
  falsos.negativos   <- ifelse(ncol(MC) == 2, MC[2, 1] / sum(MC[2,]),0) * 100
  asertividad.positiva <- ifelse(ncol(MC) == 2,  MC[2, 2] / sum(MC[,2]),0) * 100
  asertividad.negativa <- ifelse(ncol(MC) == 2, MC[1, 1] / sum(MC[,1]),0) * 100

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

#Hace el grafico de la curba de roc
plotROCInd <- function(prediccion,real,adicionar=FALSE,color="red",ylim = c(0,1.5)) {
  pred <- prediction(prediccion,real)
  perf <- performance(pred,"tpr","fpr")
  plot(perf,col=color, add=adicionar, main="Curva ROC", ylim = ylim)
  segments(0,0,1,1, col='black')
  grid()
}

plotROC <- function(sel) {
  plot.new()
  modelos <- list("KNN" = score.knn, "SVM" = score.svm, "ÁRBOLES" = score.dt,
                  "BOSQUES" = score.rf, "ADA-BOOSTING" = score.booting)
  clase <- datos.prueba[,variable.predecir]
  col <- gg_color_hue(5)

  if(is.numeric(modelos[[1]]) & "sel.knn" %in% sel)
    plotROCInd(score.knn[,input$roc.sel],clase,T,col[1])
  if(is.factor(modelos[[2]]) & "sel.svm" %in% sel)
    plotROCInd(attributes(score.svm)$probabilities[,input$roc.sel],clase,T,col[2])
  if(is.numeric(modelos[[3]]) & "sel.dt" %in% sel)
    plotROCInd(score.dt[,input$roc.sel],clase,T,col[3])
  if(is.numeric(modelos[[4]]) & "sel.rf" %in% sel)
    plotROCInd(score.rf[,input$roc.sel],clase,T,col[4])
  if(is.numeric(modelos[[5]]) & "sel.boosting" %in% sel)
    plotROCInd(score.booting[,which(levels(clase) == input$roc.sel)],clase,T,col[5])
  legend(x=0.88, y=0.25, legend = names(modelos), bty = "n", pch=20 ,
          col=col , text.col = "grey", cex=1.2, pt.cex=1.5)
}

#Calcula el area de la curva ROC
areaROC <- function(prediccion,real) {
  pred <- ROCR::prediction(prediccion,real)
  auc <- ROCR::performance(pred,"auc")
  return(attributes(auc)$y.values[[1]])
}

# Pagina de Cargar y Transformar Datos --------------------------------------------------------------------------------------

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
  res <- ifelse(deleteNA, "datos.originales <<- na.omit(datos.originales)\n",
                paste0("Mode <- function(x) {\n  x[which.max(summary(x))]\n}\n",
                       "for (variable in colnames(datos.originales)) {\n",
                       "  if(any(is.na(datos.originales[, variable]))){\n",
                       "    ifelse(class(datos.originales[, variable]) %in% c('numeric', 'integer'),\n",
                       "           datos.originales[, variable][is.na(datos.originales[, variable])] <<- \n",
                       "                                              mean(datos.originales[, variable], na.rm = T),\n",
                       "           datos.originales[, variable][is.na(datos.originales[, variable])] <<- \n",
                       "                                     Mode(datos.originales[, variable]))",
                       "\n   }\n}"))
  return(res)
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

# Pagina de Segmentar Datos -------------------------------------------------------------------------------------------------

#Crea el código de la particion en testing y learning data
particion.code <- function(data = "datos", p = "0.5", variable = NULL, semilla = 5, perm.semilla = FALSE){
  variable.predecir <<- variable
  semilla <- ifelse(is.numeric(semilla), semilla, 5)
  semilla <- ifelse(perm.semilla, paste0("set.seed(",semilla,")"), "rm(.Random.seed, envir = globalenv())")
  return(paste0(semilla,"\nparticion <- createDataPartition(datos$",variable,", p = ",p/100,", list = FALSE)\n
datos.prueba <<- datos[-particion,]\ndatos.aprendizaje <<- datos[particion,]"))
}


# Pagina de Resumen ---------------------------------------------------------------------------------------------------------

#Resumen Completo
cod.resum <- function(data = "datos"){
  return(paste0("summary(", data, ")"))
}

#Genera el resumen numerico de una variable
resumen.numerico <- function(data, variable){
  salida <- ""
  datos.numericos <- list(Q1 = list(id = "q1", Label = "Primer Cuartil",
                                    Value = format(round(quantile(data[, variable], .25), 3), scientific = F),
                                    color = "green"),
                          Mediana = list(id = "mediana", Label = "Mediana",
                                         Value = format(round(median(data[, variable]), 3), scientific = F),
                                         color = "orange"),
                          Q3 = list(id = "q3", Label = "Tercer Cuartil",
                                    Value = format(round(quantile(data[, variable], .75), 3), scientific = F),
                                    color = "maroon"),
                          Minimo = list(id = "minimo", Label = "Mínimo",
                                        Value = format(round(min(data[, variable]), 3), scientific = F),
                                        color = "red"),
                          Promedio = list(id = "promedio", Label = "Promedio",
                                          Value = format(round(mean(data[, variable]), 3), scientific = F),
                                          color = "blue"),
                          Maximo = list(id = "maximo", Label = "Máximo",
                                        Value = format(round(max(data[, variable]), 3), scientific = F),
                                        color = "purple"),
                          DS <- list(id = "ds", Label = "Desviación Estandar",
                                     Value = format(round(sd(data[, variable]), 3), scientific = FALSE, nsmall = 3),
                                     color = "yellow"))

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


# Pagina del Test de Normalidad ---------------------------------------------------------------------------------------------

#Codigo de la genracion de la curva normal (test de normalidad)
default.normal <- function(data = "datos", vars = NULL, color = "#00FF22AA"){
  if(is.null(vars)){
    return(NULL)
  } else {
    return(paste0("promedio <- mean(", data, "[, '", vars, "']) \n",
                  "desviacion <- sd(", data, "[, '", vars, "']) \n",
                  "values <- dnorm(", data, "[, '", vars, "'], mean = promedio, sd = desviacion) \n",
                  "values <- c(values, hist(", data, "[, '", vars, "'],  plot = F)$density) \n",
                  "hist(", data, "[, '", vars, "'], col = '", color, "', border=F, axes=F,\n",
                  "  freq = F, ylim = range(0, max(values)), \n",
                  "  main = paste0('Test de normalidad de la variable ','", vars, "')) \n",
                  "axis(1, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025) \n",
                  "axis(2, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025) \n",
                  "curve(dnorm(x, mean = promedio, sd = desviacion), add=T, col='blue', lwd=2)\n",
                  "legend('bottom', legend = 'Curva Normal', col = 'blue', lty=1, cex=1.5)"))
  }
}

#Genera  la tabla de normalidad
default.calc.normal <- function(data = "datos"){
  return(paste0("calc <- lapply(var.numericas(datos), function(i) modeest::skewness(i)[1]) \n",
                "calc <- as.data.frame(calc) \n",
                "calc <- rbind(calc, lapply(calc, function(i) ifelse(i > 0, 'Positiva', \n",
                "                                                           ifelse(i < 0, 'Negativa', 'Sin Asimetría')))) \n",
                "calc <- t(calc)\ncolnames(calc) <- c('Cálculo de Fisher', 'Asimetría')\ncalc"))
}

# Pagina de Dispersion ------------------------------------------------------------------------------------------------------

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

# Pagina de Distribucion ----------------------------------------------------------------------------------------------------

#Llama a la funcion que crea la distribuccion numerica
def.code.num <- function(data = "datos", variable = "input$sel.distribucion", color = 'input$col.dist'){
  return(paste0("distribucion.numerico(", data, "[, ", variable, "], ", variable, ", color = ", color,")"))
}

#Llama a la funcion que crea la distribuccion categorica
def.code.cat <- function(data = "datos", variable = "input$sel.distribucion", color = 'input$col.dist'){
  return(paste0("distribucion.categorico(", data, "[, ", variable,"], color = ", color, ")"))
}

#Hace el grafico de la distribucion numerica
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

#Hace el grafico de la distribucion categorica
default.func.cat <- function(){
  return(paste0("distribucion.categorico <<- function(var, color = 'input$col.dist'){
                colores <- sapply(c(1:length(levels(var))), function(i) rgb(sample(0:255, 1), sample(0:255, 1), sample(0:255, 1), 180, maxColorValue = 255))
                data <- data.frame(label = levels(var), value = summary(var))
                plot(ggplot(data, aes(label, value)) +
                geom_bar(stat = 'identity', fill = colores) +
                geom_text(aes(label = value, y = value), vjust = -0.5, size = 4) +
                theme_minimal() +
                labs(title = 'Distribución', y = 'Cantidad de casos', x = 'Categorias'))
}"))
}

# Pagina de Correlacion -----------------------------------------------------------------------------------------------------

#Calcula la matriz de correlacion
modelo.cor <- function(data = "datos"){
  return(paste0("correlacion <<- cor(var.numericas(", data, "))"))
}

#Codigo de la generacion de correlaciones
correlaciones <- function(metodo = 'circle', tipo = "lower"){
  return(paste0("corrplot(correlacion, method='", metodo,"', shade.col=NA, tl.col='black',
                tl.srt=20, addCoef.col='black', order='AOE', type = '", tipo, "')"))
}

# Pagina de Poder Predictivo ------------------------------------------------------------------------------------------------

#Calcula proporciones
dist.x.predecir <- function(data, variable, variable.predecir) {
  data. <- data %>%
    group_by_(variable, variable.predecir) %>%
    summarise(count = n()) %>%
    mutate(prop = round(count/sum(count),4))
  return(data.)
}

#Hace la grafica de proporciones segun la variable predictiva
plot.code.dist.porc <- function(variable, nom.variable, var.predecir, nom.predecir, colores = NA, label.size = 9.5){
  return(paste0("colores <- gg_color_hue(length(unique(datos[,'",var.predecir,"'])))
                label.size <- ",label.size," - length(unique(datos[,'",variable,"']))
                label.size <- ifelse(label.size < 3, 3, label.size)
                data. <- dist.x.predecir(datos, '",variable,"', '",var.predecir,"')
                ggplot(data., aes(fct_reorder(data.[['",variable,"']], count, .desc = T), prop, fill = data.[['",var.predecir,"']])) +
                geom_bar(stat = 'identity') +
                geom_text(aes(label = paste0(count, ' (', scales::percent(prop), ')'), y = prop), color = 'gray90',
                position = position_stack(vjust = .5), size = label.size) +
                theme_minimal() +
                theme(text = element_text(size=15)) +
                scale_fill_manual(values = colores) +
                scale_y_continuous(labels = scales::percent)+
                coord_flip() +
                labs(title = 'Distribución relativa de la variable ",nom.variable," según la ",nom.predecir,"', x = '', y = '') +
                guides(fill = guide_legend(reverse=T)) +
                theme(legend.position = 'top', legend.title = element_blank())
                "))
}

#Grafica el pairs
pairs.poder <- function(){
  return(paste0("vars.p <- datos[,'",variable.predecir,"']
                col <- rainbow( length(unique(vars.p)) + 1 )
                col <- col[2:length(col)]
                pairs.panels(var.numericas(datos),bg = col[datos[,'",variable.predecir,"']],
                pch= 22, main='', hist.col = gg_color_hue(1), ellipses = FALSE)"))
}

# Pagina de KNN -------------------------------------------------------------------------------------------------------------

#Crea el modelo KNN
kkn.modelo <- function(variable.pr = NULL, scale = TRUE,kmax = 7, kernel = "optimal"){
  return(paste0("modelo.knn <<- train.kknn(",variable.pr,"~., data = datos.aprendizaje, scale =",scale,", kmax=",kmax,", kernel = '",kernel,"')"))
}

#Codigo de la prediccion de knn
kkn.prediccion <- function() {
  return("prediccion.knn <<- predict(modelo.knn, datos.prueba)")
}

#Codigo de la matriz de confucion de knn
knn.MC <- function(variable.p){
  return(paste0("MC.knn <<- table(datos.prueba$",variable.p,", prediccion.knn)"))
}

# -------------------  SVM ------------------------ #

#Crea el modelo SVM
svm.modelo <- function(variable.pr = NULL, scale = TRUE, kernel = "linear"){
  return(paste0("modelo.svm <<- svm(",variable.pr,"~., data = datos.aprendizaje, scale =",scale,", kernel = '",kernel,"')"))
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
  if(is.null(variables)){
    return("NULL")
  }
  l <- c()
  for(i in 1:length(resto)){
    l <- c(l , paste0(resto[i],"=",2+i))
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

#Codigo del grafico de dt
dt.plot <- function(){
  num <- length(levels(datos[,variable.predecir]))
  return(paste0("prp(modelo.dt, type = 2, extra = 104, nn = T, varlen = 0, faclen = 0,
fallen.leaves = TRUE, branch.lty = 6, shadow.col = 'gray82',
box.col = gg_color_hue(",num,")[modelo.dt$frame$yval])"))
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

#Codigo del grafico de rf
rf.plot <- function(){
  return(paste0("varImpPlot(modelo.rf)"))
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

#Codigo del grafico de boosting
boosting.plot <- function(){
  return(paste0("plot(modelo.boosting,FALSE,TRUE)"))
}

#Codigo del grafico de boosting
boosting.plot.import <- function(){
  return(paste0("varplot(modelo.boosting)"))
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
       plot(c(1, 600), c(-100, 500), type = 'n', xlab='', ylab='', xaxt='n', yaxt='n')
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
       text(mean(c(x1.aux,x2.aux)) , mean(c(y1.aux,y2.aux)), cm[(i+1),(j+1)], cex=1.3, font=2, col='white')
       }
       if (j == (n.class-2)){
       rect(x1.aux, y1.aux, x2.aux, y2.aux, col= '#e4f9f5' )
       text(mean(c(x1.aux,x2.aux)) , mean(c(y1.aux,y2.aux)), paste0(round(prec.cat[i+1] * 100,2), '%'), cex=1.3, font=2, col='black')
       }
       if (j == (n.class-1)){
       rect(x1.aux, y1.aux, x2.aux, y2.aux, col= '#e4f9f5' )
       text(mean(c(x1.aux,x2.aux)) , mean(c(y1.aux,y2.aux)), paste0(round(error.cat[i+1] * 100,2), '%'), cex=1.3, font=2, col='black')
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
codigo.reporte <<- list()


datos <<- NULL  # Los datos cargados pueden estar transformados
datos.originales <<- NULL # Los datos cargados originales
datos.prueba <<- NULL
datos.aprendizaje <<- NULL
variable.predecir <<- NULL

contador <<- 0

# -------------------  Estadisticas Basicas ------------------------ #


correlacion <<- NULL
cod.disp <- default.disp()
cod.cor <- correlaciones()
cod.dya.cat <- def.code.cat()
cod.dya.num <- def.code.num()

cod.poder.cat <- NULL
cod.poder.num <- NULL

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
area.svm <<- NA

# -------------------  DT ------------------------ #

modelo.dt <<- NULL
MC.dt <<- NULL
prediccion.dt <<- NULL
indices.dt <<- rep(0,8)
score.dt <<- NULL
area.dt <<- NA

# -------------------  RF ------------------------ #

modelo.rf <<- NULL
MC.rf <<- NULL
prediccion.rf <<- NULL
indices.rf <<- rep(0,8)
score.rf <<- NULL
area.rf <<- NA

# -------------------  BOOSTING ------------------------ #

modelo.boosting <<- NULL
MC.boosting <<- NULL
prediccion.boosting <<- NULL
indices.boosting <<- rep(0,8)
score.booting <<- NULL
area.boosting <<- NA

