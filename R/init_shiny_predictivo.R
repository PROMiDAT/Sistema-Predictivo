init_shiny_predictivo <- function(){
  shiny::runApp(appDir = system.file("application",package = "PROMIDAT.PREDICTIVO.SHINY"),launch.browser = TRUE)
}
