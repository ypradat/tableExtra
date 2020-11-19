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

#' Add circle scale parameters
#'
#' @param d data.frame or matrix
#' @param n_cat integer indicating the number of differently-sized circles
#' @param r_max unit object indicating the maximum radius
#'
#' @importFrom grid unit.c 
#'
#' @author Yoann Pradat
#' @keywords internal
table_params_circle_scale <- function(d, n_cat=10, r_max=unit(10, "mm"), d_min=NULL, d_max=NULL){
  d <- scalecat(d, d_min, d_max, n_cat=n_cat, vmax=1)
  d <- as.vector(d)
  params <- list(r=do.call(unit.c, lapply(d, function(x) x*r_max)))
  return(params)
}

#' Add circle color parameters
#'
#' @param d data.frame or matrix
#' @param pal a character vector of color names
#' @param pal_breaks a numeric vector of break points
#'
#' @importFrom methods is
#'
#' @author Yoann Pradat
#' @keywords internal
table_params_circle_color <- function(d, pal, pal_breaks=NULL){
  if (is.null(d)){
    return(list())
  } else {
    if (is.null(pal_breaks)){
      pal_breaks <- length(pal)
    }
    if (is(pal_breaks,"integer")){
      if (pal_breaks==1){
        d <- as.matrix(pal, nrow=nrow(d), ncol=ncol(d))
      }
      else {
        d <- cut(d, breaks=pal_breaks, right=F)
      }
    } else {
      d <- cut(d, breaks=pal_breaks, right=F)
      levels(d) <- pal
    }
    params <- list(fill=as.vector(d))
    return(params)
  }
}

#' Add circle parameters
#'
#' @param d data.frame or matrix
#' @param n_cat integer indicating the number of differently-sized circles
#' @param r_max unit object indicating the maximum radius
#' @param pal a character vector of color names
#' @param pal_breaks a numeric vector of break points
#'
#' @importFrom grid unit.c 
#'
#' @author Yoann Pradat
#' @keywords internal
table_params_circle <- function(dscale, dcolor, n_cat, r_max, pal, pal_breaks, dscale_min=NULL, dscale_max=NULL){
  if (missing(dscale_min)){
    print("dscale is missing")
  }
  params_scale <- table_params_circle_scale(d=dscale, n_cat=n_cat, r_max=r_max, d_min=dscale_min, d_max=dscale_max)

  if(!missing(dcolor)){
    params_color <- table_params_circle_color(d=dcolor, pal=pal, pal_breaks=pal_breaks)
    params <- c(params_scale, params_color)
  } else {
    params <- params_scale
  }
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
