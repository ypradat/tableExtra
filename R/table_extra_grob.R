#' @title Graphical display of a table with circles of varying scales and colours.
#'
#' The code is inspired by the [gridExtra::tableGrob()] function.
#'
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
#' @seealso [theme_awesome()]
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

  g <- gtable_table(dscale, name="circle",
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
        g <- rbind_2(gc, g, "max", height=theme$colmore$padding[1])
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
        rows <- c("", names(cols_more), rows)
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
      for (rows_m_name in names(rows_more)){
        if(!is.null(cols)){
          if(!is.null(cols_more)){
            rows_m <- c("", names(rows_more), rows_more[[rows_m_name]])
          } else {
            rows_m <- c("", rows_more[[rows_m_name]])
          }
        }

        gr <- gtable_table(rows_m, name="rowmore",
                           fg_fun=theme$rowmore$fg_fun, 
                           bg_fun=theme$rowmore$bg_fun, 
                           fg_params=theme$rowmore$fg_params, 
                           bg_params=theme$rowmore$bg_params,
                           padding=theme$rowmore$padding)
        g <- cbind_2(g, gr, "max", width=theme$rowmore$padding[2])
      }
    }
  }

  colnames(g) <- paste0("c", seq_len(ncol(g)))
  rownames(g) <- paste0("r", seq_len(nrow(g)))

  if(!is.null(vp)) g$vp <- vp
  g
}
