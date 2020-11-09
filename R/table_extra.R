#' @aliases ttheme_awesome
#' @title Graphical display of a textual table
#'
#' Builds on top of \code{tableGrob} from gridExtra R package.
#'
#' @describeIn circleTableGrog  return a grob
#' @description Create a gtable containing circle grobs representing a numeric matrix.
#' @param dscale a matrix
#' @param dcolor (optional) a matrix
#' @param rows (optional) a character vector
#' @param cols (optional) a character vector
#' @param theme list of theme parameters
#' @param vp optional viewport
#'
#' @import gtable
#' @importFrom gtable gtable_add_rows 
#'
#' @return A gtable.
#'
#' @author Yoann Pradat
#'
#' @export
#' @examples
#' library(tableExtra)
extra_table_grob <- function(dscale, dcolor=NULL,
                             rows=rownames(dscale), cols=colnames(dscale), 
                             rows_more=NULL, cols_more=NULL,
                             rows_more_title="", cols_more_title="",
                             theme=ttheme_awesome(), vp=NULL){

  widths <- rep(theme$core$size, ncol(dscale))
  heights <- rep(theme$core$size, nrow(dscale))

  g <- gtable_table(dscale, name="circle",
                    widths=widths,
                    heights=heights,
                    fg_fun=theme$core$fg_fun, 
                    bg_fun=theme$core$bg_fun, 
                    fg_params=theme$core$fg_params, 
                    bg_params=theme$core$bg_params, 
                    padding=theme$core$padding,
                    n_cat=theme$core$n_cat,
                    r_max=0.5*theme$core$size,
                    pal=theme$core$pal,
                    pal_breaks=theme$core$pal_breaks,
                    dcolor=dcolor)


  if(!is.null(cols)){
    if (!is.null(cols_more)){
      gc <- gtable_table(t(cols_more), name="colmore",
                         fg_fun=theme$colmore$fg_fun, 
                         bg_fun=theme$colmore$bg_fun, 
                         fg_params=theme$colmore$fg_params, 
                         bg_params=theme$colmore$bg_params, 
                         padding=theme$colmore$padding)
      g <- rbind_2(gc, g, "max", height=theme$colmore$padding[1])
    }

    gc <- gtable_table(t(cols), name="colhead",
                       fg_fun=theme$colhead$fg_fun, 
                       bg_fun=theme$colhead$bg_fun, 
                       fg_params=theme$colhead$fg_params, 
                       bg_params=theme$colhead$bg_params, 
                       padding=theme$colhead$padding)
    g <- rbind_2(gc, g, "max", height=theme$colhead$padding[1])
  }

  if(!is.null(rows)){
    if(!is.null(cols)){
      if(!is.null(cols_more))
        rows <- c("", cols_more_title, rows)
      else
        rows <- c("", rows)
      }
    gr <- gtable_table(rows, name="rowhead",
                       fg_fun=theme$rowhead$fg_fun, 
                       bg_fun=theme$rowhead$bg_fun, 
                       fg_params=theme$rowhead$fg_params, 
                       bg_params=theme$rowhead$bg_params,
                       padding=theme$rowhead$padding)
    g <- cbind_2(gr, g, "max", width=theme$rowhead$padding[2])

    if(!is.null(rows_more)){
      if(!is.null(cols)){
        if(!is.null(cols_more)){
          rows_more <- c("", rows_more_title, rows_more)
        } else {
          rows_more <- c("", rows_more)
        }
      }

      gr <- gtable_table(rows_more, name="rowmore",
                         fg_fun=theme$rowmore$fg_fun, 
                         bg_fun=theme$rowmore$bg_fun, 
                         fg_params=theme$rowmore$fg_params, 
                         bg_params=theme$rowmore$bg_params,
                         padding=theme$rowmore$padding)
      g <- cbind_2(g, gr, "max", width=theme$rowmore$padding[2])
    }
  }

  colnames(g) <- paste0("c", seq_len(ncol(g)))
  rownames(g) <- paste0("r", seq_len(nrow(g)))

  if(!is.null(vp)) g$vp <- vp
  g
}


#' Define theme for awesome table plot.
#'
#' @describeIn circleTableGrob default theme for circle tables
#' @param base_size default font size
#' @param base_colour default font colour
#' @param base_family default font family
#' @param core_size cell size for core background grobs
#' @param n_cat number of size categories for core foreground grobs
#' @param pal color palette for core foreground grobs
#' @param pal_breaks breaks for color palette for core foreground grobs
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
                           n_cat=10,
                           pal="black",
                           pal_breaks=NULL,
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
               n_cat=n_cat,
               pal=pal,
               pal_breaks=pal_breaks,
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
