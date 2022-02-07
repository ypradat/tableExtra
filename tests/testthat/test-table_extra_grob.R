test_that("extra table grob", {
  skip_on_cran()
  theme <- ttheme_awesome(core_size=5)
  d <- DBS$assays$proportion

  g <- table_extra_grob(d, rows=rownames(d), cols=colnames(d),
                        theme=theme)

  out <- plot_grob(g, name="table_extra_grob.pdf", width=4, height=4)
  expect_true(out$plot_success)
})


test_that("extra table grob cols more", {
  skip_on_cran()
  theme <- ttheme_awesome(core_size=5)
  d <- DBS$assays$proportion

  g <- table_extra_grob(d, rows=rownames(d), cols=colnames(d),
                        cols_more=list("n="=DBS$colData$description),
                        theme=theme)

  out <- plot_grob(g, name="table_extra_grob_cols_more.pdf", width=4, height=4)
  expect_true(out$plot_success)
})


test_that("extra table grob cols more rows more no color annotated", {
  skip_on_cran()
  theme <- ttheme_awesome(core_size=5)
  d <- DBS$assays$proportion
  cols_more <- list("n="=DBS$colData$description)
  rows_more <- list("Proposed aetiology"=DBS$rowData$description)

  g <- table_extra_grob(d, rows=rownames(d), cols=colnames(d),
                        cols_more=cols_more, rows_more=rows_more,
                        theme=theme)


  find_cell <- function(g, row, col, name){
    l <- g$layout
    which(l$t==2*row-1 & l$l==2*col-1 & l$name==name)
  }

  ind <- find_cell(g, 3, 3, "core-bg")
  g$grobs[ind][[1]][["gp"]] <- gpar(col = "red", lwd=3)

  out <- plot_grob(g, name="table_extra_grob_cols_more_rows_more_no_color_annotated.pdf", width=5, height=4)
  expect_true(out$plot_success)
})


test_that("extra table grob cols more rows more with color", {
  skip_on_cran()
  color_palette <- c("#ffc651", "#ffa759", "#ff8962", "#ff6b6b", "#cc6999", "#9968c8", "#6767f8", "#4459ce", "#224ba5", 
                     "#013d7c")
  color_breaks <- c(0, 0.001,0.005,0.008,0.01,0.02,0.03,0.05,0.1,0.5,1)

  theme <- ttheme_awesome(core_size=5, color_palette=color_palette, color_breaks=color_breaks)
  dscale <- DBS$assays$proportion
  dcolor <- DBS$assays$median
  cols_more <- list("n="=DBS$colData$description)
  rows_more <- list("Proposed aetiology"=DBS$rowData$description)

  g <- table_extra_grob(dscale=dscale, dcolor=dcolor, 
                        rows=rownames(dscale), cols=colnames(dscale),
                        cols_more=cols_more, rows_more=rows_more,
                        theme=theme)

  out <- plot_grob(g, name="table_extra_grob_cols_more_rows_more_with_color.pdf", width=5, height=4)
  expect_true(out$plot_success)
})


test_that("extra table grob genes work", {
  skip_on_cran()
  color_palette <- c("#6e0d25", "#c6ca53")
  color_breaks <- c(-2, 0, 2)

  theme <- ttheme_awesome(core_size=5, color_palette=color_palette, color_breaks=color_breaks)
  dscale <- -log10(DEXP$assays$pvalue) + 1
  dcolor <- DEXP$assays$sign

  g <- table_extra_grob(dscale=dscale, dcolor=dcolor, 
                        rows=rownames(dscale), cols=colnames(dscale),
                        cols_more=list("n1="=DBS$colData$description, 
                                       "n2="=DBS$colData$description), 
                        rows_more=NULL,
                        theme=theme)

  out <- plot_grob(g, name="table_extra_grob_genes.pdf", width=5, height=12)
  expect_true(out$plot_success)
})


test_that("extra table grob genes with dscale_min and dscale_max works", {
  skip_on_cran()
  color_palette <- c("#6e0d25", "#c6ca53")
  color_breaks <- c(-2, 0, 2)

  theme <- ttheme_awesome(core_size=5,
                          color_palette=color_palette, 
                          color_breaks=color_breaks)
  dscale <- -log10(DEXP$assays$pvalue) + 1
  dcolor <- DEXP$assays$sign

  g <- table_extra_grob(dscale=dscale, dcolor=dcolor, 
                        rows=rownames(dscale), cols=colnames(dscale),
                        cols_more=list("n1="=DBS$colData$description), 
                        rows_more=NULL,
                        dscale_min=-log10(0.05),
                        dscale_max=NULL,
                        theme=theme)

  out <- plot_grob(g, name="table_extra_grob_genes_dmin_dmax.pdf", width=5, height=12)
  expect_true(out$plot_success)
})


test_that("extra table grob genes with scale breaks works", {
  skip_on_cran()
  color_palette <- c("#6e0d25", "#c6ca53")
  color_breaks <- c(-2, 0, 2)
  scale_breaks <- c(1, 2, 5)

  theme <- ttheme_awesome(core_size=5, 
                          scale_breaks=scale_breaks,
                          color_palette=color_palette, 
                          color_breaks=color_breaks)
  dscale <- -log10(DEXP$assays$pvalue) + 1
  dcolor <- DEXP$assays$sign

  g <- table_extra_grob(dscale=dscale, dcolor=dcolor, 
                        rows=rownames(dscale), cols=colnames(dscale),
                        cols_more=list("n1="=DBS$colData$description), 
                        rows_more=NULL,
                        dscale_min=NULL,
                        dscale_max=NULL,
                        theme=theme)

  out <- plot_grob(g, name="table_extra_grob_genes_dmin_dmax_with_scale_breaks.pdf", width=5, height=12)
  expect_true(out$plot_success)
})


test_that("extra table grob with rows more", {
  skip_on_cran()
  # get tables for plot
  plot_data <- pcawg_plot_data()

  # draw
  output <- file.path(system.file("tests", "outputs", package="tableExtra"), "table_extra_grob_pcawg.pdf")
  draw_table_extra(dscale=plot_data$dscale, theme=plot_data$theme, output=output,
                   dcolor=plot_data$dcolor, cols_more=plot_data$cols_more, rows_more=plot_data$rows_more,
                   dscale_title_legend="Prop of tumors with the signature",
                   dcolor_title_legend="Median mut/Mb due to signature")
  graphics.off()
  expect_true(file.exists(output))
})


test_that("extra table grob without rows more no legend", {
  skip_on_cran()
  # get tables for plot
  plot_data <- pcawg_plot_data()
  plot_data$theme$legend$show <- F

  # draw
  output <- file.path(system.file("tests", "outputs", package="tableExtra"),
                      "table_extra_grob_pcawg_no_rows_more_no_legend.pdf")
  draw_table_extra(dscale=plot_data$dscale, theme=plot_data$theme, output=output,
                   dcolor=plot_data$dcolor, cols_more=plot_data$cols_more, rows_more=NULL,
                   dscale_title_legend="Prop of tumors with the signature",
                   dcolor_title_legend="Median mut/Mb due to signature")
  graphics.off()
  expect_true(file.exists(output))
})


test_that("extra table grob without rows more legend center", {
  skip_on_cran()
  # get tables for plot
  plot_data <- pcawg_plot_data()
  plot_data$theme$legend$position <- "top_center"

  # draw
  output <- file.path(system.file("tests", "outputs", package="tableExtra"),
                      "table_extra_grob_pcawg_no_rows_more_legend_center.pdf")
  draw_table_extra(dscale=plot_data$dscale, theme=plot_data$theme, output=output,
                   dcolor=plot_data$dcolor, cols_more=plot_data$cols_more, rows_more=NULL,
                   dscale_title_legend="Prop of tumors with the signature",
                   dcolor_title_legend="Median mut/Mb due to signature")
  graphics.off()
  expect_true(file.exists(output))
})


test_that("extra table grob legend top left", {
  skip_on_cran()
  # get tables for plot
  plot_data <- pcawg_plot_data()
  plot_data$theme$legend$position <- "top_left"

  # draw
  output <- file.path(system.file("tests", "outputs", package="tableExtra"),
                      "table_extra_grob_pcawg_no_rows_more_legend_top_left.pdf")
  draw_table_extra(dscale=plot_data$dscale, theme=plot_data$theme, output=output,
                   dcolor=plot_data$dcolor, cols_more=plot_data$cols_more, rows_more=NULL,
                   dscale_title_legend="Prop of tumors with the signature",
                   dcolor_title_legend="Median mut/Mb due to signature")
  graphics.off()
  expect_true(file.exists(output))
})


test_that("extra table grob legend top right", {
  skip_on_cran()
  # get tables for plot
  plot_data <- pcawg_plot_data()
  plot_data$theme$legend$position <- "top_right"

  # draw
  output <- file.path(system.file("tests", "outputs", package="tableExtra"),
                      "table_extra_grob_pcawg_no_rows_more_legend_top_right.pdf")
  draw_table_extra(dscale=plot_data$dscale, theme=plot_data$theme, output=output,
                   dcolor=plot_data$dcolor, cols_more=plot_data$cols_more, rows_more=NULL,
                   dscale_title_legend="Prop of tumors with the signature", margin_x=unit(0.5, "inches"),
                   dcolor_title_legend="Median mut/Mb due to signature")
  graphics.off()
  expect_true(file.exists(output))
})


test_that("extra table grob cols more rows more with color and frames", {
  skip_on_cran()
  # get tables for plot
  plot_data <- pcawg_plot_data()
  plot_data$theme$legend$position <- "top_left"

  # add random frames
  dframes <- list(test=matrix(0, nrow=nrow(plot_data$dscale), ncol=ncol(plot_data$dscale),
                              dimnames=list(rownames(plot_data$dscale), colnames(plot_data$dscale))))
  dframes[["test"]][1,1] <- 1
  dframes[["test"]][10,5] <- 1
  colors_frames <- list(test="#E5383B")

  # draw
  output <- file.path(system.file("tests", "outputs", package="tableExtra"),
                      "table_extra_grob_pcawg_no_rows_more_legend_top_left_with_frames.pdf")
  draw_table_extra(dscale=plot_data$dscale, theme=plot_data$theme, output=output,
                   dcolor=plot_data$dcolor, cols_more=plot_data$cols_more, rows_more=NULL,
                   dscale_title_legend="Prop of tumors with the signature",
                   dcolor_title_legend="Median mut/Mb due to signature",
                   dframes=dframes, colors_frames=colors_frames)
  graphics.off()
  expect_true(file.exists(output))
})
