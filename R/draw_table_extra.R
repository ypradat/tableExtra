#' @title Graphical display of a table with grobs of varying scales and colours.
#'
#' @description Draw a table containing grobs of varying sizes and colors encoding two different kinds of information.
#' The column names and row names of the table are displayed on the top and left sides of the table respectively.
#'
#' @param dscale a matrix containing the values defining the grobs scales.
#' @param theme a list of theme parameters. Use an instance of `ttheme_awesome`.
#' @param output path to output file. Only pdf supported for now. 
#' @param dcolor (optional) a matrix of size (n,m) containing the values defining the grobs colors.
#' @param dscale_min (optional) value for setting the minimum scale size of foreground grobs. Entries in the
#'    \code{dscale} matrix below \code{dscale_min} will have a scale of 0 (no grob).
#' @param dscale_max (optional) value for setting the maximum scale size of foreground grobs. Entries in the
#'    \code{dscale} matrix above \code{dscale_max} will have a scale of 1.
#' @param rows_more (optional) a named list of additional columns (right-part) of the plot for describing the rows. The
#'    list names will be used as column headers.
#' @param cols_more (optional) a named list of additional rows (top-part) of the plot for describing the columns The
#'    list names will be used as row headers.
#' @param dscale_title_legend (optional) title for the colorbar providing a legend for scales.
#' @param dcolor_title_legend (optional) title for the colorbar providing a legend for colors 
#' @param margin_x (optional) use it to fine-tune the width of the plot if some elements are not displayed correctly.
#' @param margin_y (optional) use it to fine-tune the height of the plot if some elements are not displayed correctly.
#' @return No return value, the last instruction calls graphics.off() in order to write the plot to the .pdf file 
#'  specified via \code{output} argument.
#'
#' @importFrom grDevices dev.off
#' @importFrom grid unit
#'
#' @seealso [ttheme_awesome()], [gtable_table()], [gtable_legend()]
#'
#' @author Yoann Pradat
#'
#' @export
#' @examples
#' \donttest{
#' library(dplyr)
#' library(tableExtra)
#' library(tibble)
#'
#' # load data
#' load(system.file("testdata", "pcawg_counts.rda", package="tableExtra"))
#' load(system.file("testdata", "sbs_aetiologies.rda", package="tableExtra"))
#'
#' pcawg_plot_data <- function(){
#'   scale_breaks <- seq(from=0, to=1, by=0.1)
#'   color_palette <- c("#ffc651", "#ffa759", "#ff8962", "#ff6b6b", "#cc6999", "#9968c8", 
#'                      "#6767f8", "#4459ce", "#224ba5","#013d7c")
#'   color_breaks <- c(0, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10, 25, 1e6)
#'   color_bg <- c("#f8f9fa", "#e9ecef")
#' 
#'   theme <- ttheme_awesome(base_size=12,
#'                           rep_mode="col",
#'                           core_size=5, 
#'                           scale_breaks=scale_breaks,
#'                           color_palette=color_palette, 
#'                           color_breaks=color_breaks, 
#'                           core=list(bg_params=list(fill=color_bg)))
#' 
#'   # define dscale and cols_more from PCAWG data
#'   dscale <- pcawg_counts %>%
#'     group_by(Cancer.Types) %>%
#'     mutate(n=n()) %>%
#'     summarize_at(vars(-Sample.Names, -Accuracy), ~sum(.x>0)) %>%
#'     mutate_at(vars(-Cancer.Types,-n), ~./n)
#' 
#'   cols_more <- list("n="=dscale$n)
#'   dscale$n <- NULL
#'   dscale <- column_to_rownames(.data=dscale, var="Cancer.Types")
#'   dscale <- t(as.matrix(dscale))
#'   
#'   # define dcolor and rows_more from PCAWG data
#'   mask <- sbs_aetiologies$Signature %in% rownames(dscale)
#'   rows_more <- list("Aetiology"=sbs_aetiologies[mask, "Aetiology"])
#' 
#'   dcolor <- pcawg_counts %>%
#'     group_by(Cancer.Types) %>%
#'     summarize_at(vars(-Sample.Names, -Accuracy), ~median(.[.!=0]*1e6/3.2e9)) %>%
#'     replace(is.na(.),0)
#' 
#'   dcolor <- column_to_rownames(.data=dcolor, var="Cancer.Types")
#'   dcolor <- t(as.matrix(dcolor))
#' 
#'   list(dscale=dscale, dcolor=dcolor, cols_more=cols_more, rows_more=rows_more, theme=theme)
#' }
#'
#' # tables needed for the plot and graphical parameters in `theme`
#' plot_data <- pcawg_plot_data()
#'
#' # draw
#' output <- file.path(tempdir(),"table_extra_pcawg.pdf")
#' draw_table_extra(dscale=plot_data$dscale, theme=plot_data$theme, output=output,
#'                  dcolor=plot_data$dcolor, cols_more=plot_data$cols_more,
#'                  rows_more=plot_data$rows_more,
#'                  dscale_title_legend="Prop of tumors with the signature",
#'                  dcolor_title_legend="Median mut/Mb due to signature")
#'}
draw_table_extra <- function(dscale, theme, output, dcolor=NULL, dscale_min=NULL, dscale_max=NULL, cols_more=NULL,
                             rows_more=NULL, dscale_title_legend="Scale title", dcolor_title_legend="Color title",
                             margin_x=unit(1, "inches"), margin_y=unit(1, "inches")){

  # set legend position and layout according to rows_more
  if (is.null(theme$legend$position)){
    if (!is.null(rows_more)){
      theme$legend$position <- "top_right"
    } else {
      theme$legend$position <- "top_left"
    }
  }

  if (is.null(theme$legend$layout)){
    theme$legend$layout <- "columnwise"
  }

  # get output plot dimensions
  dims <- get_table_extra_dimensions(dscale=dscale, dcolor=dcolor, theme=theme, rows_more=rows_more, 
                                     cols_more=cols_more, unit="inches", margin_x=margin_x, margin_y=margin_y, 
                                     dscale_title_legend=dscale_title_legend, dcolor_title_legend=dcolor_title_legend)

  # for some legend positions, the space available for legend is not enough. To increase space available,
  # whitespaces may be added to the rownames or colnames of dscale or to rows_more/cols_more
  rownames(dscale) <- dims$rows
  colnames(dscale) <- dims$cols
  rows_more <- dims$rows_more
  cols_more <- dims$cols_more
  margin_x <- convert_unit(margin_x, "inches")
  margin_y <- convert_unit(margin_y, "inches")

  # init device
  grDevices::pdf(file=output,
                 width=as.numeric(dims$width),
                 height=as.numeric(dims$height),
                 onefile=TRUE)

  # main plot
  g <- gtable_extra(dscale=dscale, dcolor=dcolor, 
                        rows=rownames(dscale), cols=colnames(dscale),
                        cols_more=cols_more, rows_more=rows_more,
                        theme=theme)

  # render core plot
  grid.draw(g)

  if (theme$legend$show){
    # legend theme parameters
    theme$legend$core <- theme$core
    theme$legend$core$size <- theme$legend$scale*theme$legend$core$size
    theme$legend$core$bg_params$width <- theme$legend$scale*theme$legend$core$bg_params$width
    theme$legend$core$bg_params$height <- theme$legend$scale*theme$legend$core$bg_params$height

    if (is.null(theme$legend$title_fontsize)) theme$legend$title_fontsize <- theme$colhead$fontsize
    if (is.null(theme$legend$labels_fontsize)) theme$legend$labels_fontsize <- theme$colhead$fontsize

    # extract scale breaks
    if (length(theme$core$scale_breaks)==1){
      scale_breaks <- seq(from=0, to=1, length.out=theme$core$scale_breaks+1)
    } else {
      scale_breaks <- theme$core$scale_breaks
    }

    # legend 1
    g1 <- gtable_legend(d=t(scale_breaks[2:length(scale_breaks)]),
                        labels=scale_breaks,
                        widths=rep(theme$legend$core$size, length(scale_breaks)-1),
                        heights=theme$legend$core$size,
                        fg_fun=theme$legend$core$fg_fun, 
                        bg_fun=theme$legend$core$bg_fun, 
                        fg_params=theme$legend$core$fg_params, 
                        bg_params=theme$legend$core$bg_params, 
                        title_label=dscale_title_legend,
                        title_gp=gpar(fontsize=theme$legend$title_fontsize),
                        title_x=0.5*(length(scale_breaks)-1)*theme$legend$core$size,
                        title_y=1.5*theme$legend$core$size,
                        labels_gp=gpar(fontsize=theme$legend$labels_fontsize),
                        labels_pad=theme$legend$labels_pad,
                        padding=theme$legend$core$padding,
                        scale_breaks=scale_breaks,
                        rep_mode=theme$legend$core$rep_mode,
                        r_min=theme$legend$core$scale_ratio*0.5*theme$legend$core$size,
                        r_max=0.5*theme$legend$core$size)

    if (!is.null(theme$core$color_breaks)){
      # legend 2
      fg_fun <- theme$legend$core$bg_fun
      fg_params <- theme$legend$core$bg_params
      fg_params$fill <- NULL

      # extract color breaks
      if (length(theme$core$color_breaks)==1){
        color_breaks <- seq(from=0, to=1, length.out=theme$core$color_breaks+1)
      } else {
        color_breaks <- theme$core$color_breaks
      }

      g2 <- gtable_legend(d=t(color_breaks[1:length(color_breaks)-1]),
                          labels=color_breaks,
                          widths=rep(theme$legend$core$size, length(color_breaks)-1),
                          heights=theme$legend$core$size,
                          fg_fun=fg_fun,
                          fg_params=fg_params,
                          bg_fun=NULL,
                          bg_params=NULL,
                          title_label=dcolor_title_legend,
                          title_gp=gpar(fontsize=theme$legend$title_fontsize),
                          title_x=0.5*(length(color_breaks)-1)*theme$legend$core$size,
                          title_y=1.5*theme$legend$core$size,
                          orientation="horizontal",
                          labels_gp=gpar(fontsize=theme$legend$labels_fontsize),
                          labels_pad=theme$legend$labels_pad,
                          padding=theme$legend$core$padding,
                          color_breaks=color_breaks,
                          color_palette=theme$core$color_palette)
    }

    if (is.null(theme$legend$width)) theme$legend$width <- dims$width_legend
    if (is.null(theme$legend$height)) theme$legend$height <- dims$height_legend
    
    # legend x
    if (is.null(theme$legend$x)){
      if (theme$legend$position=="top_right"){
        theme$legend$x <- (dims$width - 0.5*margin_x - 0.5 * dims$width_rows_more - 0.5*theme$legend$width)/dims$width
        just1 <- c("left", "bottom")
        just2 <- c("left", "top")
      } else if (theme$legend$position=="top_left"){
        theme$legend$x <- (0.5*margin_x + 0.5*dims$width_rows - 0.5*theme$legend$width)/dims$width
        just1 <- c("left", "bottom")
        just2 <- c("left", "top")
      } else if (theme$legend$position=="top_center"){
        theme$legend$x <- 0.5
        just1 <- c("center", "bottom")
        just2 <- c("center", "top")
      }
    }
    
    # legend y
    if (is.null(theme$legend$y)){
      if (theme$legend$position %in% c("top_left", "top_right")){
        theme$legend$y <- (dims$height - 0.5*(dims$height_cols+dims$height_cols_more) - 0.5*theme$legend$height)/dims$height
      } else {
        theme$legend$y <- (dims$height - 0.5*margin_y - 0.5*dims$height_legend)/dims$height
      }
    }


    vp1 <- viewport(x=theme$legend$x, y=theme$legend$y, 
                    width=dims$width_legend/dims$width, height=2*dims$height_legend_one/dims$height,
                    just=just1)
    pushViewport(vp1)
    grid.draw(g1)
    popViewport()

    vp2 <- viewport(x=theme$legend$x, y=theme$legend$y, 
                    width=dims$width_legend/dims$width, height=2*dims$height_legend_one/dims$height,
                    just=just2)
    pushViewport(vp2)
    grid.draw(g2)
    popViewport()
  }

  dev.off()
}


#' @title Get width and height of the plot.
#'
#' @description Compute the width and height in user-specified unit required for drawing the plot.
#' @inheritParams draw_table_extra 
#' @param unit (optional) choose any unit that is valid for `grid::unit`.
#' @return a list with dimensions of the global plot and of parts of the plot.
#'
#' @importFrom grid unit
#' @importFrom graphics par strheight strwidth
#' @author Yoann Pradat
#' @keywords internal
get_table_extra_dimensions <- function(dscale, dcolor, theme, rows_more=NULL, cols_more=NULL,  unit="inches",
                                       dscale_title_legend=NULL, dcolor_title_legend=NULL, margin_x=unit(1, "inches"),
                                       margin_y=unit(1, "inches")){

  # rows and cols will be used to update rownames and colnames of dscale respectively
  rows <- rownames(dscale)
  cols <- colnames(dscale)

  #### width

  # width of core plot
  width <- convert_unit(sum(rep(theme$core$size, ncol(dscale))), unit) + 
    convert_unit(sum(rep(theme$core$padding[[1]], ncol(dscale)-1)), unit)

  # width of row labels
  width_rows <- convert_unit(extract_strwidth_max_label(rows, theme$rowhead$fg_params$fontsize, "inches"),
                             from="inches",  to=unit)

  # width needed for legend
  if (theme$legend$show){
    width_legend_one <- convert_unit(theme$core$size, unit) * (length(theme$core$scale_breaks)-1) * theme$legend$scale
    width_legend <- width_legend_one
  } else {
    width_legend_one <- 0
    width_legend <- 0
  }

  # get width taken by rows_more
  if (!is.null(rows_more)){
    rows_more_agg <- sapply(1:nrow(dscale), function(i) paste(extract_breadth_first(rows_more, i), collapse=""))
    width_rows_more <- convert_unit(extract_strwidth_max_label(rows_more_agg, theme$rowmore$fg_params$fontsize, "inches"),
                                    from="inches", to=unit)
  } else {
    width_rows_more <- 0
  }

  # if required, add whitespaces to rows_more
  if (theme$legend$show & theme$legend$position=="top_right"){
    if (is.null(rows_more)){
      width_space <- strwidth(" ", font=1, cex=theme$rowmore$fg_params$fontsize/12, units=unit, ps=par(ps=12))
      nspaces_required <- ceiling(width_legend/width_space)+1
      text_spaces <- paste(rep(" ", nspaces_required), collapse="")
      rows_more <- list(" "=rep(text_spaces, nrow(dscale)))
      width_rows_more <- width_space*nspaces_required
    } else {
      if (width_rows_more < width_legend){
        width_space <- strwidth(" ", font=1, cex=theme$rowmore$fg_params$fontsize/12, units=unit, ps=par(ps=12))
        nspaces_required <- ceiling((width_legend-width_rows_more)/width_space)+1
        text_spaces <- paste(rep(" ", nspaces_required), collapse="")
        rows_more <- lapply(rows_more, function(x) paste(x, text_spaces))
        width_rows_more <- width_rows_more + width_space*nspaces_required 
      }
    }
  }

  # if required, add whitespaces to rows to row names of dscale to fit the legend
  if (theme$legend$show & theme$legend$position=="top_left"){
    if (width_rows < width_legend){
      width_space <- strwidth(" ", font=1, cex=theme$rowhead$fg_params$fontsize/12, units=unit, ps=par(ps=12))
      nspaces_required <- ceiling((width_legend-width_rows)/width_space)+1
      text_spaces <- paste(rep(" ", nspaces_required), collapse="")
      rows <- paste(text_spaces, rows)
      width_rows <- width_space*nspaces_required
    }
  }

  width <- width_rows + width +  width_rows_more + convert_unit(margin_x, unit)

  #### height

  # height of core plot
  height <- convert_unit(sum(rep(theme$core$size, nrow(dscale))), unit) + 
    convert_unit(sum(rep(theme$core$padding[[2]], nrow(dscale)-1)), unit)

  # height of col labels
  fontsize <- theme$colhead$fg_params$fontsize
  height_cols <- convert_unit(extract_strwidth_max_label(cols, fontsize, "inches"), from="inches", to=unit)

  # height needed for legend
  if (theme$legend$show){
    height_legend_one <- convert_unit(theme$core$size, unit) * theme$legend$scale + 
      strheight(dscale_title_legend, font=1, cex=theme$legend$title_fontsize/12, units=unit, ps=par(ps=12))
    # height_legend_one <- convert_unit(theme$core$size, unit) * theme$legend$scale + 
    #   strheight(dscale_title_legend, font=1, cex=theme$legend$title_fontsize/12, units=unit, ps=par(ps=12)) +
    #   strheight("0", font=1, cex=theme$legend$labels_fontsize/12, units=unit, ps=par(ps=12)) +
    #   2*convert_unit(unit(abs(theme$legend$labels_pad), "mm"), unit)
    if (!is.null(dcolor) & !is.null(dcolor_title_legend) & theme$legend$layout=="columnwise"){
      height_legend <- 2*height_legend_one
    } else {
      height_legend <- height_legend_one
    }
  } else {
    height_legend_one <- 0
    height_legend <- 0
  }

  # if required, add whitespaces to rows to row names of dscale to fit the legend
  if (theme$legend$show & theme$legend$position=="top_center"){
    height_space <- strheight(" ", font=1, cex=theme$colhead$fg_params$fontsize/12, units=unit, ps=par(ps=12))
    nspaces_required <- ceiling(2*height_legend/height_space)
    text_spaces <- paste(rep(" ", nspaces_required), collapse="")
    cols <- paste(cols, text_spaces)
    height_cols <- height_cols + height_space*nspaces_required
  }

  # increase height_cols or height_cols_more if not enough space for legend
  if (!is.null(cols_more)){
    cols_more_agg <- sapply(1:ncol(dscale), function(i) paste(extract_breadth_first(cols_more, i), collapse=""))
    fontsize <- theme$colmore$fg_params$fontsize
    height_cols_more <- extract_strwidth_max_label(cols_more_agg, fontsize, "inches")
    height_cols_more <- convert_unit(height_cols_more, from="inches", to=unit)

    if (height_cols_more+height_cols < height_legend){
      height_cols <- height_legend-height_cols_more
    }
  } else {
    height_cols_more <- 0

    if (height_cols < height_legend){
      height_cols <- height_legend
    }
  }

  height <- height_cols + height +  height_cols_more + convert_unit(margin_y, unit)

  list(width=width, height=height, unit=unit, width_rows=width_rows, width_rows_more=width_rows_more, 
       height_cols=height_cols, height_cols_more=height_cols_more, width_legend=width_legend,
       width_legend_one=width_legend_one, height_legend=height_legend, height_legend_one=height_legend_one,
       rows=rows, cols=cols, rows_more=rows_more, cols_more=cols_more)
}
