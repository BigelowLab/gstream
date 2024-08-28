#' Get the root data path
#' 
#' @export
#' @param filename chr, the file to read
#' @return character root data path
get_root_path = function(filename = "~/.gstream"){
  yaml::read_yaml(filename) |>
    getElement("path")
}

#' Build a gstream data path
#' 
#' @export
#' @param ... character path segements passed to \code{file.path}
#' @param root chr, the root path
#' @return a file path
gstream_path = function(..., root = get_root_path()){
  file.path(root, ...)
}
