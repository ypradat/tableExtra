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
#' @param rep_mode optional parameter passed to \code{table_params}.
#' @param ... additional parameters passed to \code{add_table_params}.
#' @return A gtable.
#'
#' @import gtable
#'
#' @author Yoann Pradat
#' @keywords internal
gtable_table <- function(d, widths, heights,
                         fg_fun=text_grob, fg_params=list(),
                         bg_fun=NULL, bg_params=NULL,
                         padding=unit(c(4, 4), "mm"),
                         name="table", vp=NULL, rep_mode="row", ...){

  d <- as.matrix(d)

  nc <- ncol(d)
  nr <- nrow(d)
  n <- nc*nr

  tb_params <- table_params(d, fg_params, bg_params, rep_mode)
  fg_params <- tb_params[["fg_params"]]
  fg_params <- add_table_params(d, fg_params, fg_fun, ...)

  frgds <- do.call(mapply, c(fg_params, list(FUN = fg_fun, SIMPLIFY=FALSE)))
  frgds_grobs <- matrix(frgds, ncol = nc, byrow = FALSE)

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
  if (!is.null(bg_params)){
    bg_params <- tb_params[["bg_params"]]
    bkgds <- do.call(mapply, c(bg_params, list(FUN = bg_fun, SIMPLIFY=FALSE)))
    bkgds_grobs <- matrix(bkgds, ncol = nc, byrow = FALSE)

    g <- gtable_add_grob(g, bkgds_grobs, 
                         t=rep(seq_len(nr), length.out = n), 
                         l=rep(seq_len(nc), each = nr), z=0, 
                         name=paste0(name, "-bg"))
  }
  
  # add padding
  g <- gtable_add_col_space(g, padding[1])
  g <- gtable_add_row_space(g, padding[2])

  g
}

#' Build a grob containing a legend.
#'
#' Build a grob with a legend inside.
#'
#' @param d data.frame or matrix
#' @param labels tick labels displayed at legend tick marks
#' @param widths optional \code{unit.list} specifying the grob widths
#' @param heights optional \code{unit.list} specifying the grob heights
#' @param fg_fun grob-drawing function
#' @param fg_params  named list of params passed to fg_fun
#' @param bg_fun grob-drawing function 
#' @param bg_params  named list of params passed to bg_fun
#' @param title_x \code{unit} specifying the x position of the title
#' @param title_y \code{unit} specifying the x position of the title
#' @param title_label character vector
#' @param title_gp graphical parameters of the title
#' @param labels_pad padding between the text labels
#' @param labels_gp graphical parameters of the text labels
#' @param padding numeric vector specifying the padding between adjacent cells.
#' @param size_unit character vector defining the unit used for sizes. See \code{grid::unit} for all possible
#'    specifications
#' @param name name of the grob
#' @param orientation choose 'horizontal' or 'vertical'
#' @param vp optional viewport
#' @param ... additional parameters passed to \code{add_table_params}.
#' @return A gtable.
#'
#' @import gtable
#'
#' @author Yoann Pradat
#' @export
gtable_legend <- function(d, labels, widths, heights, fg_fun, fg_params, bg_fun=NULL, bg_params=NULL,
                          title_x=NULL, title_y=NULL, title_label="Title", title_gp=gpar(fontsize=10), 
                          labels_pad=-1, labels_gp=gpar(fontsize=6), padding=0.3,
                          size_unit="mm", name="legend", vp=NULL, orientation=c("horizontal", "vertical"), ...){

  orientation <- match.arg(orientation)
  labels_pad <- unit(labels_pad, size_unit)
  padding <- unit(padding, size_unit)

  # legend body
  g <- gtable_table(d, name=name,
                    widths=widths,
                    heights=heights,
                    fg_fun=fg_fun,
                    bg_fun=bg_fun, 
                    fg_params=fg_params, 
                    bg_params=bg_params, 
                    padding=padding, vp=vp, ...)

  # legend title
  g_title <- textGrob(label=title_label, 
                      x=title_x,
                      y=title_y,
                      just="centre", 
                      gp=title_gp)
  g <- gtable_add_grob(g, g_title, t=1, l=1, b=1, r=1, name=paste0(name, "_title"), clip="off")

  # legend labels
  if (orientation=="horizontal"){
    x <- unit(0, size_unit)
    for (i in 1:length(labels)){
      g_label <- textGrob(label=labels[i], 
                          x=x,
                          y=labels_pad, 
                          just="centre", 
                          gp=labels_gp)
      g <- gtable_add_grob(g, g_label, t=1, l=1, b=1, r=1, name=paste0(name, "_label_", i), clip="off")
      if (i < length(labels)){
        x <- x + widths[i] + padding[2]
      }
    }
  } else if (orientation=="vertical"){
    y <- (-1)*sum(heights) + heights[1] + (-1)*(length(heights)-1)*padding[1]
    for (i in 1:length(labels)){
      g_label <- textGrob(label=labels[i], 
                          x=labels_pad,
                          y=y, 
                          just="centre", 
                          gp=labels_gp)
      g <- gtable_add_grob(g, g_label, t=1, l=1, b=1, r=1, name=paste0(name, "_label_", i), clip="off")
      if (i < length(labels)){
        y <- y + heights[i] + padding[1]
      }
    }
  } else {
    stop("Unsupported value '", orientation, "' of orientation. Choose 'vertical' or 'horizontal'")
  }

  g
}
