#' 
#' is x a trtsel object?
#' 
#' Function to check if an object is of class "trtsel"
#' 
#' 
#' @param x Any R object.
#' @return Returns TRUE or FALSE depending on whether x is a "trtsel" object or
#' not.
#' @export is.trtsel
is.trtsel <-
function(x){
   inherits(x, "trtsel")
}
