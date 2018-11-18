#' Add suffix to a character vector if it's longer than 1
#'
#'
#' @param x Character vector to make unique
#' @param pad Characters to pad between string and suffix
#' @param suffix Vector of things to add, defalults to lower case letters
#' @return A unique character vector
#'
#'@export
make_unique <- function(x, pad = "", suffix = letters){
  if (length(unique(x))>1) stop("x is not unique")
  if (length(x)==1 | all(is.na(x))){
    x
  }else{
    paste0(x, pad, suffix[1:length(x)])
  }
}
