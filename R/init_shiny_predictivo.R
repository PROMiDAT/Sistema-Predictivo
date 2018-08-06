init_shiny_predictivo <- function(){
  .GlobalEnv$foto <- ls(envir = .GlobalEnv)
  Sys.setenv("LANGUAGE" = "ES")
  options(encoding = "utf8")
  shiny::runApp(appDir = system.file("application",package = "PROMIDAT.PREDICTIVO.SHINY"),launch.browser = TRUE)
}
