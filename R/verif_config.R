# Creates or opens a verification config file in the default editor.
# The template should be in ./inst somewhere

make_verif_config <- function(config_file_name) {

}

edit_verif_config <- function(config_file_name) {
  utils::edit(file = config_file_name)
}
