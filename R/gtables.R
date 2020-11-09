## Note: this functions is modified copy of gtable_table from the gridExtra package.

#' Build a table with foreground and background grobs.
#'
#' This function is a copy of the internal function \code{gtable_table} of gridExtra package. 
#'
#' @param d data.frame or matrix
#' @param widths optional \code{unit.list} specifying the grob widths
#' @param heights optional \code{unit.list} specifying the grob heights
#' @param fg_fun grob-drawing function
#' @param fg_params  named list of params passed to fg_fun
#' @param bg_fun grob-drawing function 
#' @param bg_params  named list of params passed to bg_fun
#' @param padding \code{unit.list} object specifying the padding between adjacent cells.
#' @param name optional name of the grob
#' @param vp optional viewport
#' @param ... additional parameters passed to add_table_params.
#' @return A gtable.
#'
#' @import gtable
#'
#' @author Yoann Pradat
#' @keywords internal
gtable_table <- function(d, widths, heights,
                         fg_fun = text_grob, fg_params = list(),
                         bg_fun = rect_grob, bg_params = list(),
                         padding = unit(c(4, 4), "mm"),
                         name = "table", vp = NULL, ...){

  d <- as.matrix(d)

  nc <- ncol(d)
  nr <- nrow(d)
  n <- nc*nr

  tb_params <- table_params(d, fg_params, bg_params)
  bg_params <- tb_params[["bg_params"]]
  fg_params <- tb_params[["fg_params"]]
  fg_params <- add_table_params(d, fg_params, fg_fun, ...)

  frgds <- do.call(mapply, c(fg_params, list(FUN = fg_fun, SIMPLIFY=FALSE)))
  bkgds <- do.call(mapply, c(bg_params, list(FUN = bg_fun, SIMPLIFY=FALSE)))

  frgds_grobs <- matrix(frgds, ncol = nc, byrow = FALSE)
  bkgds_grobs <- matrix(bkgds, ncol = nc, byrow = FALSE)

  if(missing(widths))
    widths <- col_widths(frgds_grobs)
  if(missing(heights))
    heights <- row_heights(frgds_grobs)

  ## make the gtable matrix of foreground
  g <- gtable_matrix(paste0(name, "-fg"), 
                     grobs = frgds_grobs, 
                     widths = widths, 
                     heights = heights, vp=vp)
  
  ## add the background
  g <- gtable_add_grob(g, bkgds_grobs, 
                       t=rep(seq_len(nr), length.out = n), 
                       l=rep(seq_len(nc), each = nr), z=0, 
                       name=paste0(name, "-bg"))
  
  # add padding
  g <- gtable_add_col_space(g, padding[1])
  g <- gtable_add_row_space(g, padding[2])

  g
}
