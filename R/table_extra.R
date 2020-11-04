#' @aliases ttheme_awesome
#' @title Graphical display of a textual table
#'
#' Builds on top of \code{tableGrob} from gridExtra R package.
#'
#' @describeIn circleTableGrog  return a grob
#' @description Create a gtable containing circle grobs representing a numeric matrix.
#' @param se \code{SummarizedExperiment} object with rownames, colnames, rowData, colData.
#' @param theme list of theme parameters
#' @param vp optional viewport
#' @param ... further arguments to control the gtable
#'
#' @importFrom gtable gtable_add_rows 
#'
#' @return A gtable.
#'
#' @author Yoann Pradat
#'
#' @export
#' @examples
#' library(tableExtra)
extra_table_grob <- function(d, rows=rownames(d), cols=colnames(d), 
                            theme = ttheme_awesome(), vp = NULL,...){

  d <- norm_and_cat(d, ncat=theme$core$ncircle, vmax=0.5)

  g <- gtable_circle(d, name="core",
                    fg_fun = theme$core$fg_fun, 
                    bg_fun = theme$core$bg_fun, 
                    fg_params = theme$core$fg_params, 
                    bg_params = theme$core$bg_params, 
                    padding=theme$core$padding, ...)
  
  if(!is.null(cols)){
    gc <- gtable_text(t(cols), name="colhead",
                       fg_fun = theme$colhead$fg_fun, 
                       bg_fun = theme$colhead$bg_fun, 
                       fg_params = theme$colhead$fg_params, 
                       bg_params = theme$colhead$bg_params, 
                       padding=theme$colhead$padding)
    g <- rbind_2(gc, g, "max", height=theme$rowhead$padding[1])
  }
  if(!is.null(rows)){
    if(!is.null(cols)) # need to add dummy cell
      rows <- c("", rows)
    gr <- gtable_text(rows, name="rowhead",
                       fg_fun = theme$rowhead$fg_fun, 
                       bg_fun = theme$rowhead$bg_fun, 
                       fg_params = theme$rowhead$fg_params, 
                       bg_params = theme$rowhead$bg_params,
                       padding=theme$rowhead$padding)
    g <- cbind_2(gr, g, "max")
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
#' @param parse logical, default behaviour for parsing text as plotmath
#' @param padding length-2 unit vector specifying the horizontal and vertical padding of text within each cell
#'
#' @importFrom utils modifyList
#'
#' @author Yoann Pradat
#'
#' @export
ttheme_awesome <- function(base_size=12, 
                           base_colour="black", 
                           base_family="",
                           ncircle=10,
                           parse=FALSE, 
                           padding = unit(c(1, 1), "mm"), ...){
  
  core <- list(fg_fun = circle_grob, 
               fg_params = list(fill = c("#6767f8"), col="white", lwd=0),
               bg_fun = rect_grob, 
               bg_params = list(fill = c("#f2f2f2","#e5e5e5"), 
                                lwd=0, col="white"),
               ncircle = ncircle,
               padding = padding)
  
  colhead <- list(fg_fun = text_grob, 
                  fg_params = list(parse=parse, col=base_colour,
                                   fontface=1L,
                                   fontsize = base_size,
                                   fontfamily = base_family,
                                   hjust = 0,
                                   y = 0.05,
                                   rot = 90),
                  bg_fun = rect_grob, 
                  bg_params = list(fill = c("grey95"), lwd=0, col="white"),
                  padding = padding)
  
  rowhead <- list(fg_fun = text_grob, 
                  fg_params = list(parse=parse, col=base_colour,
                                   fontface=1L,
                                   fontsize = base_size,
                                   fontfamily = base_family,
                                   hjust = 1, 
                                   x = 0.95),
                  bg_fun = rect_grob, 
                  bg_params = list(fill=c("grey95"), lwd=0, col="white"),
                  padding = padding)
  
  default <- list(
    core = core,
    colhead = colhead,
    rowhead= rowhead
  )
  
  modifyList(default, list(...))
  
}
