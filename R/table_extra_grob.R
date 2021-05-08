#' @title Graphical display of a table with circles of varying scales and colours.
#'
#' The code is inspired by the \code{tableGrob} function \code{gridExtra}
#'
#' @description Create a gtable containing circle grobs representing a numeric matrix.
#' @param dscale a matrix
#' @param dscale_min value for setting the minimum scale size of foreground grobs. Entries in the \code{dscale} matrix
#' below \code{dscale_min} will have a scale of 0 (no grob).
#' @param dscale_max value for setting the maximum scale size of foreground grobs. Entries in the \code{dscale} matrix
#' above \code{dscale_max} will have a scale of 0 (no grob).
#' @param dcolor (optional) a matrix
#' @param rows (optional) a character vector
#' @param cols (optional) a character vector
#' @param rows_more (optional) a named list of additional columns (right-part) of the plot for describing the rows. The
#' list names will be used as column headers.
#' @param cols_more (optional) a named list of additional rows (top-part) of the plot for describing the columns The
#' list names will be used as row headers.
#' @param theme list of theme parameters
#' @param vp optional viewport
#'
#' @import gtable
#' @importFrom gtable gtable_add_rows 
#'
#' @seealso [ttheme_awesome()]
#'
#' @return An R object of class \code{grob}
#'
#' @author Yoann Pradat
#'
#' @export
#' @examples
#' library(tableExtra)
table_extra_grob <- function(dscale, dcolor=NULL,
                             dscale_min=NULL, dscale_max=NULL,
                             rows=rownames(dscale), cols=colnames(dscale), 
                             rows_more=NULL, cols_more=NULL,
                             theme=ttheme_awesome(), vp=NULL){

  widths <- rep(theme$core$size, ncol(dscale))
  heights <- rep(theme$core$size, nrow(dscale))

  g <- gtable_table(dscale, name="core",
                    widths=widths,
                    heights=heights,
                    fg_fun=theme$core$fg_fun, 
                    bg_fun=theme$core$bg_fun, 
                    fg_params=theme$core$fg_params, 
                    bg_params=theme$core$bg_params, 
                    padding=theme$core$padding,
                    scale_breaks=theme$core$scale_breaks,
                    dscale_min=dscale_min,
                    dscale_max=dscale_max,
                    rep_mode=theme$core$rep_mode,
                    r_min=theme$core$scale_ratio*0.5*theme$core$size,
                    r_max=0.5*theme$core$size,
                    color_palette=theme$core$color_palette,
                    color_breaks=theme$core$color_breaks,
                    dcolor=dcolor)

  if(!is.null(cols)){
    if (!is.null(cols_more)){
      for (cols_m_name in names(cols_more)){
        cols_m <- cols_more[[cols_m_name]]
        gc <- gtable_table(t(cols_m), name="colmore",
                           fg_fun=theme$colmore$fg_fun, 
                           bg_fun=theme$colmore$bg_fun, 
                           fg_params=theme$colmore$fg_params, 
                           bg_params=theme$colmore$bg_params, 
                           padding=theme$colmore$padding)
        g <- gtable_rbind(gc, g, size="max", height=theme$colmore$padding[1])
      }
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
        rows <- c("", rev(names(cols_more)), rows)
      else
        rows <- c("", rows)
      }
    gr <- gtable_table(rows, name="rowhead",
                       fg_fun=theme$rowhead$fg_fun, 
                       bg_fun=theme$rowhead$bg_fun, 
                       fg_params=theme$rowhead$fg_params, 
                       bg_params=theme$rowhead$bg_params,
                       padding=theme$rowhead$padding)
    g <- gtable_cbind(gr, g, size="max", width=theme$rowhead$padding[2])

    if(!is.null(rows_more)){
      for (rows_m_name in names(rows_more)){
        rows_m_pre <- c()
        if(!is.null(cols)){
          rows_m_pre <- c(rows_m_pre, "")
        }
        if(!is.null(cols_more)){
          rows_m_pre <- c(rows_m_pre, rep("", length(cols_more)-1), rows_m_name)
        } 

        rows_m <- c(rows_m_pre, rows_more[[rows_m_name]])

        gr <- gtable_table(rows_m, name="rowmore",
                           fg_fun=theme$rowmore$fg_fun, 
                           bg_fun=theme$rowmore$bg_fun, 
                           fg_params=theme$rowmore$fg_params, 
                           bg_params=theme$rowmore$bg_params,
                           padding=theme$rowmore$padding)
        g <- gtable_cbind(g, gr, size="max", width=theme$rowmore$padding[2])
      }
    }
  }

  colnames(g) <- paste0("c", seq_len(ncol(g)))
  rownames(g) <- paste0("r", seq_len(nrow(g)))

  if(!is.null(vp)) g$vp <- vp
  g
}
