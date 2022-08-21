# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))a

# Document and reload your packagea
golem::document_and_reload()

# Run the application
run_app()
 
#rm(list = c("app_ui", "golem_add_external_resources", "mod_xname_of_module2_server", "mod_name_of_module2_ui", "run_app"))
