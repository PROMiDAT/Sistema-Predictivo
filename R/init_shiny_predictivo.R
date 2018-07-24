init_shiny_predictivo <- function(){
  .GlobalEnv$foto <- ls(envir = .GlobalEnv)
  shiny::runApp(appDir = system.file("application",package = "PROMIDAT.PREDICTIVO.SHINY"),launch.browser = TRUE)
}
