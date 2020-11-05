#' Build a table of parameters. 
#'
#' @param d data.frame or matrix
#' @param fg_params named list of params
#' @param bg_params named list of params
#'
#' @importFrom grid unit.c 
#'
#' @author Yoann Pradat
#' @keywords internal
table_params <- function(d, fg_params, bg_params){
  nc <- ncol(d)
  nr <- nrow(d)
  n <- nc*nr
  
  fg_params <- lapply(fg_params, rep_ifshort, n = n, nc = nc, nr = nr)
  bg_params <- lapply(bg_params, rep_ifshort, n = n, nc = nc, nr = nr)
  
  return(list(fg_params=fg_params, bg_params=bg_params))
}

#' Add circle parameters
#'
#' @param d data.frame or matrix
#' @param n_cat integer indicating the number of differently-sized circles
#' @param r_max unit object indicating the maximum radius
#'
#' @importFrom grid unit.c 
#'
#' @author Yoann Pradat
#' @keywords internal
table_params_circle <- function(d, n_cat=10, r_max=unit(10, "mm")){
  d <- scalecat(d, n_cat=n_cat, vmax=1)
  d <- as.vector(d)
  params <- list(r=do.call(unit.c, lapply(d, function(x) x*r_max)))
  return(params)
}

#' Add text parameters
#'
#' @param d data.frame or matrix
#'
#' @author Yoann Pradat
#' @keywords internal
table_params_text <- function(d){
  d <- as.vector(d)
  params <- list(label=d)
  return(params)
}


#' Add parameter specific to the grob class.
#'
#' @param d data.frame or matrix
#' @param params named list of table params
#' @param fun a function that returns a grob object.
#'
#' @author Yoann Pradat
#' @keywords internal
add_table_params <- function(d, params, fun, ...){
  if (all.equal(fun, circle_grob)==T){
    extra_params <- table_params_circle(d, ...)
  } else if (all.equal(fun, text_grob)==T) {
    extra_params <- table_params_text(d)
  } else {
    stop("unsupported value of fun")
  }

  return(c(params, extra_params))
}
