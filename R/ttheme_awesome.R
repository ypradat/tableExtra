#' Define theme for awesome table plot.
#'
#' @param base_size default font size
#' @param base_colour default font colour
#' @param base_family default font family
#' @param core_size cell size for core background grobs
#' @param scale_breaks number of size categories for core foreground grobs or numeric vector of bin breaks
#' @param scale_ratio ratio of minimum to maximum core foreground grobs sizes
#' @param color_palette color palette for core foreground grobs
#' @param color_breaks bin breaks for color palette for core foreground grobs
#' @param parse logical, default behaviour for parsing text as plotmath
#' @param padding length-2 unit vector specifying the horizontal and vertical padding of text within each cell
#' @param ... extra parameters added to the theme list
#'
#' @importFrom utils modifyList
#'
#' @author Yoann Pradat
#'
#' @export
ttheme_awesome <- function(base_size=8, 
                           base_colour="black", 
                           base_family="",
                           core_size=unit(10, "mm"),
                           scale_breaks=10,
                           scale_ratio=0.25,
                           color_palette="black",
                           color_breaks=NULL,
                           parse=FALSE, 
                           padding=unit(c(0.3,0.3), "mm"), ...){

  # The code `do.call(mapply, c(fg_params, list(FUN = fg_fun, SIMPLIFY=FALSE)))`
  # cannot accomodate variables of the class `unit`.  
  # Current fix: split the value and the unit.
  core_size_value <- as.numeric(core_size)
  core_size_unit <- attr(core_size, "unit")
  core <- list(fg_fun=circle_grob, 
               fg_params=list(col="white", lwd=0),
               bg_fun=rect_grob, 
               bg_params=list(fill=c("#f2f2f2","#e5e5e5"),
                              width=core_size_value, 
                              height=core_size_value,
                              default.units=core_size_unit,
                              lwd=0, col="white"),
               size=core_size,
               scale_breaks=scale_breaks,
               scale_ratio=scale_ratio,
               color_palette=color_palette,
               color_breaks=color_breaks,
               padding=padding)

  colhead <- list(fg_fun=text_grob, 
                  fg_params=list(parse=parse, col=base_colour,
                                 fontface=1L,
                                 fontsize=base_size,
                                 fontfamily=base_family,
                                 hjust=0,
                                 y=0.05,
                                 rot=90),
                  bg_fun=rect_grob, 
                  bg_params=list(fill=c("white"),
                                 width=core_size_value,
                                 default.units=core_size_unit,
                                 lwd=0, col="white"),
                  padding=padding)

  colmore <- list(fg_fun=text_grob, 
                  fg_params=list(parse=parse, col=base_colour,
                                 fontface=1L,
                                 fontsize=base_size,
                                 fontfamily=base_family,
                                 hjust=0,
                                 y=0.05,
                                 rot=90),
                  bg_fun=rect_grob, 
                  bg_params=list(fill=c("white"),
                                 width=core_size_value,
                                 default.units=core_size_unit,
                                 lwd=0, col="white"),
                  padding=padding)

  rowhead <- list(fg_fun=text_grob, 
                  fg_params=list(parse=parse, col=base_colour,
                                 fontface=1L,
                                 fontsize=base_size,
                                 fontfamily=base_family,
                                 hjust=1, 
                                 x=0.95),
                  bg_fun=rect_grob, 
                  bg_params=list(fill=c("white"),
                                 height=core_size_value,
                                 default.units=core_size_unit,
                                 lwd=0, col="white"),
                  padding=padding)

  rowmore <- list(fg_fun=text_grob, 
                  fg_params=list(parse=parse, col=base_colour,
                                 fontface=1L,
                                 fontsize=base_size,
                                 fontfamily=base_family,
                                 hjust=0, 
                                 x=0),
                  bg_fun=rect_grob, 
                  bg_params=list(fill=c("white"),
                                 height=core_size_value,
                                 default.units=core_size_unit,
                                 lwd=0, col="white"),
                  padding=padding)

  default <- list(core=core,
                  colhead=colhead,
                  colmore=colmore,
                  rowhead=rowhead,
                  rowmore=rowmore)

  modifyList(default, list(...))
}
