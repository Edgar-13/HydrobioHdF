# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
# Pas de pkgload en prod
options("golem.app.prod" = TRUE)
HydrobioHdF::run_app()
