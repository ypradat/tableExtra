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
table_params <- function(d, fg_params, bg_params=NULL, rep_mode="row"){
  nc <- ncol(d)
  nr <- nrow(d)
  n <- nc*nr
  
  fg_params <- lapply(fg_params, rep_ifshort, n = n, nc = nc, nr = nr, rep_mode=rep_mode)

  if (!is.null(bg_params)){
    bg_params <- lapply(bg_params, rep_ifshort, n = n, nc = nc, nr = nr, rep_mode=rep_mode)
    return(list(fg_params=fg_params, bg_params=bg_params))
  } else {
    return(list(fg_params=fg_params))
  }
}

#' Add circle scale parameters
#'
#' @param d data.frame or matrix
#' @param scale_breaks integer indicating the number of differently-sized circles
#' @param r_max unit object indicating the maximum radius
#'
#' @importFrom grid unit.c 
#'
#' @author Yoann Pradat
#' @keywords internal
table_params_circle_scale <- function(d, scale_breaks=10, r_min=unit(4, "mm"), r_max=unit(10, "mm"), d_min=NULL, 
                                      d_max=NULL){
  d <- breaks_scale(d=d, d_min=d_min, d_max=d_max, breaks=scale_breaks)
  d <- as.vector(d)
  r_func <- function(x){
    if (x==0){
      return(unit(0,"mm"))
    } else {
      return(r_min + x*(r_max-r_min))
    }
  }
  params <- list(r=do.call(unit.c, lapply(d, r_func)))
  return(params)
}

#' Add circle color parameters
#'
#' @param d data.frame or matrix
#' @param color_palette a character vector of color names
#' @param color_breaks a numeric vector of break points
#'
#' @author Yoann Pradat
#' @keywords internal
table_params_color <- function(d, color_palette, color_breaks=NULL){
  if (is.null(d)){
    return(list())
  } else {
    if (is.null(color_breaks)){
      color_breaks <- length(color_palette)
    }
    if (length(color_breaks)==1){
      if (color_breaks==1){
        d <- as.matrix(color_palette, nrow=nrow(d), ncol=ncol(d))
      }
      else {
        d <- cut(d, breaks=color_breaks, right=F)
      }
    } else {
      d <- cut(d, breaks=color_breaks, right=F)
      levels(d) <- color_palette
    }
    params <- list(fill=as.vector(d))
    return(params)
  }
}

#' Add rect parameters
#'
#' @param d data.frame or matrix
#' @param scale_breaks integer indicating the number of differently-sized circles
#' @param r_max unit object indicating the maximum radius
#' @param color_palette a character vector of color names
#' @param color_breaks a numeric vector of break points
#'
#' @importFrom grid unit.c 
#'
#' @author Yoann Pradat
#' @keywords internal
table_params_rect <- function(dcolor, color_palette=NULL, color_breaks=NULL){
  params <- NULL
  if (!is.null(color_palette)){
    params_color <- table_params_color(d=dcolor, color_palette=color_palette, color_breaks=color_breaks)
    params <- c(params, params_color)
  }
  return(params)
}

#' Add circle parameters
#'
#' @param d data.frame or matrix
#' @param scale_breaks integer indicating the number of differently-sized circles
#' @param r_max unit object indicating the maximum radius
#' @param color_palette a character vector of color names
#' @param color_breaks a numeric vector of break points
#'
#' @importFrom grid unit.c 
#'
#' @author Yoann Pradat
#' @keywords internal
table_params_circle <- function(dscale, dcolor, scale_breaks, r_min, r_max,  color_palette, color_breaks, 
                                dscale_min=NULL, dscale_max=NULL){
  params_scale <- table_params_circle_scale(d=dscale, scale_breaks=scale_breaks, r_min=r_min, r_max=r_max, 
                                            d_min=dscale_min, d_max=dscale_max)

  if(!missing(dcolor)){
    params_color <- table_params_color(d=dcolor, color_palette=color_palette, color_breaks=color_breaks)
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
  if (all.equal(fun, circle_grob)==TRUE){
    extra_params <- table_params_circle(d, ...)
  } else if (all.equal(fun, text_grob)==TRUE) {
    extra_params <- table_params_text(d)
  } else if (all.equal(fun, rect_grob)==TRUE) { 
    extra_params <- table_params_rect(d, ...)
  } else {
    stop("Unsupported value of fun")
  }

  return(c(params, extra_params))
}
