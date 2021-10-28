test_that("gtable text row no padding", {
  
  theme <- ttheme_awesome(padding=c(0,0))
  d <- t(DBS$colData$description)
  widths <- rep(theme$core$size, ncol(d))
  heights <- unit(4, "mm")

  gc <- gtable_table(d, name="colhead",
                     widths=widths,
                     heights=heights,
                     fg_fun=theme$colhead$fg_fun, 
                     bg_fun=theme$colhead$bg_fun, 
                     fg_params=theme$colhead$fg_params, 
                     bg_params=theme$colhead$bg_params, 
                     padding=theme$colhead$padding)

  width <- convertUnit(sum(widths), "inches")
  height <- convertUnit(sum(heights), "inches")
  out <- plot_grob(gc, name="gtable_text_row_no_padding.pdf", width=width, height=height)
  expect_true(out$plot_success)
})


test_that("gtable text row with padding", {
  
  theme <- ttheme_awesome(padding=c(2,0))
  d <- t(DBS$colData$description)
  widths <- rep(theme$core$size, ncol(d))
  heights <- unit(4, "mm")

  gc <- gtable_table(d, name="colhead",
                     widths=widths,
                     heights=heights,
                     fg_fun=theme$colhead$fg_fun, 
                     bg_fun=theme$colhead$bg_fun, 
                     fg_params=theme$colhead$fg_params, 
                     bg_params=theme$colhead$bg_params, 
                     padding=theme$colhead$padding)

  width <- convertUnit(sum(widths), "inches")
  height <- convertUnit(sum(heights), "inches")
  out <- plot_grob(gc, name="gtable_text_row_with_padding.pdf", width=width, height=height)
  expect_true(out$plot_success)
})


test_that("gtable text col", {
  
  theme <- ttheme_awesome(padding=c(0,0))
  d <- DBS$rowData$name
  widths <- theme$core$size
  heights <- rep(theme$core$size, length(d))

  gr <- gtable_table(d, name="rowhead",
                     widths=widths,
                     heights=heights,
                     fg_fun=theme$rowhead$fg_fun, 
                     bg_fun=theme$rowhead$bg_fun, 
                     fg_params=theme$rowhead$fg_params, 
                     bg_params=theme$rowhead$bg_params, 
                     padding=theme$rowhead$padding)

  width <- convertUnit(sum(widths), "inches")
  height <- convertUnit(sum(heights), "inches")
  out <- plot_grob(gr, name="gtable_text_col.pdf", width=width, height=height)
  expect_true(out$plot_success)
})


test_that("gtable text mat no padding", {
  
  theme <- ttheme_awesome(padding=c(0,0))
  d <- DBS$rowData$name
  d <- matrix(rep(d, 5), nrow=5, byrow=T)
  widths <- rep(theme$core$size, ncol(d))
  heights <- rep(theme$core$size, nrow(d))

  gr <- gtable_table(d, name="rowhead",
                     widths=widths,
                     heights=heights,
                     fg_fun=theme$rowhead$fg_fun, 
                     bg_fun=theme$rowhead$bg_fun, 
                     fg_params=theme$rowhead$fg_params, 
                     bg_params=theme$rowhead$bg_params, 
                     padding=theme$rowhead$padding)

  width <- convertUnit(sum(widths), "inches")
  height <- convertUnit(sum(heights), "inches")
  out <- plot_grob(gr, name="gtable_text_mat_no_padding.pdf", width=width, height=height)
  expect_true(out$plot_success)
})


test_that("gtable text mat with padding", {
  
  theme <- ttheme_awesome(padding=c(1,1))
  d <- DBS$rowData$name
  d <- matrix(rep(d, 5), nrow=5, byrow=T)
  widths <- rep(theme$core$size, ncol(d))
  heights <- rep(theme$core$size, nrow(d))

  gr <- gtable_table(d, name="rowhead",
                     widths=widths,
                     heights=heights,
                     fg_fun=theme$rowhead$fg_fun, 
                     bg_fun=theme$rowhead$bg_fun, 
                     fg_params=theme$rowhead$fg_params, 
                     bg_params=theme$rowhead$bg_params, 
                     padding=theme$rowhead$padding)

  width <- convertUnit(sum(widths) + theme$core$padding[1]*(length(widths)-1), "inches")
  height <- convertUnit(sum(heights) + theme$core$padding[2]*(length(heights)-1), "inches")
  out <- plot_grob(gr, name="gtable_text_mat_with_padding.pdf", width=width, height=height)
  expect_true(out$plot_success)
})


test_that("gtable circle", {
  
  theme <- ttheme_awesome(core_size=10, core=list(fg_params=list(fill="blue")))

  d <- DBS$assays$proportion
  widths <- rep(theme$core$size, ncol(d))
  heights <- rep(theme$core$size, nrow(d))

  g <- gtable_table(d, name="circle",
                    widths=widths,
                    heights=heights,
                    fg_fun=theme$core$fg_fun, 
                    bg_fun=theme$core$bg_fun, 
                    fg_params=theme$core$fg_params, 
                    bg_params=theme$core$bg_params, 
                    padding=theme$core$padding,
                    scale_breaks=theme$core$scale_breaks,
                    dscale_min=NULL,
                    dscale_max=NULL,
                    r_min=0.1*theme$core$size,
                    r_max=0.5*theme$core$size)

  width <- convertUnit(sum(widths) + theme$core$padding[1]*(length(widths)-1), "inches")
  height <- convertUnit(sum(heights) + theme$core$padding[2]*(length(heights)-1), "inches")
  out <- plot_grob(g, name="gtable_circle.pdf", width=width, height=height)
  expect_true(out$plot_success)
})


test_that("gtable circle annotated", {
  
  theme <- ttheme_awesome()
  d <- DBS$assays$proportion
  widths <- rep(theme$core$size, ncol(d))
  heights <- rep(theme$core$size, nrow(d))

  g <- gtable_table(d, name="circle",
                    widths=widths,
                    heights=heights,
                    fg_fun=theme$core$fg_fun, 
                    bg_fun=theme$core$bg_fun, 
                    fg_params=theme$core$fg_params, 
                    bg_params=theme$core$bg_params, 
                    padding=theme$core$padding,
                    scale_breaks=theme$core$scale_breaks,
                    dscale_min=NULL,
                    dscale_max=NULL,
                    r_min=0.1*theme$core$size,
                    r_max=0.5*theme$core$size)

  find_cell <- function(table, row, col, name="circle-fg"){
    l <- table$layout
    which(l$t==2*row-1 & l$l==2*col-1 & l$name==name)
  }

  ind <- find_cell(g, 2, 2, "circle-bg")
  g$qrobs[ind][[1]][["gp"]] <- modifyList(g$grobs[ind][[1]][["gp"]], list(col="red", lwd=3))

  width <- convertUnit(sum(widths) + theme$core$padding[1]*(length(widths)-1), "inches")
  height <- convertUnit(sum(heights) + theme$core$padding[2]*(length(heights)-1), "inches")
  out <- plot_grob(g, name="gtable_circle_annotated.pdf", width=width, height=height)
  expect_true(out$plot_success)
})


test_that("gtable extra", {
  
  theme <- ttheme_awesome(core_size=5)
  d <- DBS$assays$proportion

  g <- gtable_extra(d, rows=rownames(d), cols=colnames(d), theme=theme)

  out <- plot_grob(g, name="gtable_extra.pdf", width=4, height=4)
  expect_true(out$plot_success)
})


test_that("gtable extra cols more", {
  
  theme <- ttheme_awesome(core_size=5)
  d <- DBS$assays$proportion

  g <- gtable_extra(d, rows=rownames(d), cols=colnames(d),
                        cols_more=list("n="=DBS$colData$description),
                        theme=theme)

  out <- plot_grob(g, name="gtable_extra_cols_more.pdf", width=4, height=4)
  expect_true(out$plot_success)
})


test_that("gtable extra cols more rows more no color annotated", {
  
  theme <- ttheme_awesome(core_size=5)
  d <- DBS$assays$proportion
  cols_more <- list("n="=DBS$colData$description)
  rows_more <- list("Proposed aetiology"=DBS$rowData$description)

  g <- gtable_extra(d, rows=rownames(d), cols=colnames(d),
                        cols_more=cols_more, rows_more=rows_more,
                        theme=theme)


  find_cell <- function(g, row, col, name){
    l <- g$layout
    which(l$t==2*row-1 & l$l==2*col-1 & l$name==name)
  }

  ind <- find_cell(g, 3, 3, "core-bg")
  g$grobs[ind][[1]][["gp"]] <- gpar(col = "red", lwd=3)

  out <- plot_grob(g, name="gtable_extra_cols_more_rows_more_no_color_annotated.pdf", width=5, height=4)
  expect_true(out$plot_success)
})


test_that("gtable extra cols more rows more with color", {
  
  color_palette <- c("#ffc651", "#ffa759", "#ff8962", "#ff6b6b", "#cc6999", "#9968c8", "#6767f8", "#4459ce", "#224ba5", 
                     "#013d7c")
  color_breaks <- c(0, 0.001,0.005,0.008,0.01,0.02,0.03,0.05,0.1,0.5,1)

  theme <- ttheme_awesome(core_size=5, color_palette=color_palette, color_breaks=color_breaks)
  dscale <- DBS$assays$proportion
  dcolor <- DBS$assays$median
  cols_more <- list("n="=DBS$colData$description)
  rows_more <- list("Proposed aetiology"=DBS$rowData$description)

  g <- gtable_extra(dscale=dscale, dcolor=dcolor, 
                        rows=rownames(dscale), cols=colnames(dscale),
                        cols_more=cols_more, rows_more=rows_more,
                        theme=theme)

  out <- plot_grob(g, name="gtable_extra_cols_more_rows_more_with_color.pdf", width=5, height=4)
  expect_true(out$plot_success)
})


test_that("gtable extra genes work", {
  
  color_palette <- c("#6e0d25", "#c6ca53")
  color_breaks <- c(-2, 0, 2)

  theme <- ttheme_awesome(core_size=5, color_palette=color_palette, color_breaks=color_breaks)
  dscale <- -log10(DEXP$assays$pvalue) + 1
  dcolor <- DEXP$assays$sign

  g <- gtable_extra(dscale=dscale, dcolor=dcolor, 
                        rows=rownames(dscale), cols=colnames(dscale),
                        cols_more=list("n1="=DBS$colData$description, 
                                       "n2="=DBS$colData$description), 
                        rows_more=NULL,
                        theme=theme)

  out <- plot_grob(g, name="gtable_extra_genes.pdf", width=5, height=12)
  expect_true(out$plot_success)
})


test_that("gtable extra genes with dscale_min and dscale_max works", {
  
  color_palette <- c("#6e0d25", "#c6ca53")
  color_breaks <- c(-2, 0, 2)

  theme <- ttheme_awesome(core_size=5,
                          color_palette=color_palette, 
                          color_breaks=color_breaks)
  dscale <- -log10(DEXP$assays$pvalue) + 1
  dcolor <- DEXP$assays$sign

  g <- gtable_extra(dscale=dscale, dcolor=dcolor, 
                        rows=rownames(dscale), cols=colnames(dscale),
                        cols_more=list("n1="=DBS$colData$description), 
                        rows_more=NULL,
                        dscale_min=-log10(0.05),
                        dscale_max=NULL,
                        theme=theme)

  out <- plot_grob(g, name="gtable_extra_genes_dmin_dmax.pdf", width=5, height=12)
  expect_true(out$plot_success)
})


test_that("gtable extra genes with scale breaks works", {
  
  color_palette <- c("#6e0d25", "#c6ca53")
  color_breaks <- c(-2, 0, 2)
  scale_breaks <- c(1, 2, 5)

  theme <- ttheme_awesome(core_size=5, 
                          scale_breaks=scale_breaks,
                          color_palette=color_palette, 
                          color_breaks=color_breaks)
  dscale <- -log10(DEXP$assays$pvalue) + 1
  dcolor <- DEXP$assays$sign

  g <- gtable_extra(dscale=dscale, dcolor=dcolor, 
                        rows=rownames(dscale), cols=colnames(dscale),
                        cols_more=list("n1="=DBS$colData$description), 
                        rows_more=NULL,
                        dscale_min=NULL,
                        dscale_max=NULL,
                        theme=theme)

  out <- plot_grob(g, name="gtable_extra_genes_dmin_dmax_with_scale_breaks.pdf", width=5, height=12)
  expect_true(out$plot_success)
})


