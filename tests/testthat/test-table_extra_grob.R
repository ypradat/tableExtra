test_that("extra table grob", {
  theme <- ttheme_awesome(core_size=unit(5, "mm"))
  d <- SummarizedExperiment::assays(DBS)$proportion

  g <- table_extra_grob(d, rows=rownames(d), cols=colnames(d),
                        theme=theme)

  out <- plot_grob(g, name="table_extra_grob.pdf", width=4, height=4)
  expect_true(out$plot_success)
})

test_that("extra table grob cols more", {
  theme <- ttheme_awesome(core_size=unit(5, "mm"))
  d <- SummarizedExperiment::assays(DBS)$proportion

  g <- table_extra_grob(d, rows=rownames(d), cols=colnames(d),
                        cols_more=list("n="=SummarizedExperiment::colData(DBS)$description),
                        theme=theme)

  out <- plot_grob(g, name="table_extra_grob_cols_more.pdf", width=4, height=4)
  expect_true(out$plot_success)
})

test_that("extra table grob cols more rows more no color", {
  theme <- ttheme_awesome(core_size=unit(5, "mm"))
  d <- SummarizedExperiment::assays(DBS)$proportion
  cols_more <- list("n="=SummarizedExperiment::colData(DBS)$description)
  rows_more <- list("Proposed aetiology"=SummarizedExperiment::rowData(DBS)$description)

  g <- table_extra_grob(d, rows=rownames(d), cols=colnames(d),
                        cols_more=cols_more, rows_more=rows_more,
                        theme=theme)

  out <- plot_grob(g, name="table_extra_grob_cols_more_rows_more_no_color.pdf", width=5, height=4)
  expect_true(out$plot_success)
})

test_that("extra table grob cols more rows more with color", {
  color_palette <- c("#ffc651", "#ffa759", "#ff8962", "#ff6b6b", "#cc6999", "#9968c8", "#6767f8", "#4459ce", "#224ba5", 
                     "#013d7c")
  color_breaks <- c(0, 0.001,0.005,0.008,0.01,0.02,0.03,0.05,0.1,0.5,1)

  theme <- ttheme_awesome(core_size=unit(5, "mm"), color_palette=color_palette, color_breaks=color_breaks)
  dscale <- SummarizedExperiment::assays(DBS)$proportion
  dcolor <- SummarizedExperiment::assays(DBS)$median
  cols_more <- list("n="=SummarizedExperiment::colData(DBS)$description)
  rows_more <- list("Proposed aetiology"=SummarizedExperiment::rowData(DBS)$description)

  g <- table_extra_grob(dscale=dscale, dcolor=dcolor, 
                        rows=rownames(dscale), cols=colnames(dscale),
                        cols_more=cols_more, rows_more=rows_more,
                        theme=theme)

  out <- plot_grob(g, name="table_extra_grob_cols_more_rows_more_with_color.pdf", width=5, height=4)
  expect_true(out$plot_success)
})

test_that("extra table grob genes work", {
  color_palette <- c("#6e0d25", "#c6ca53")
  color_breaks <- c(-2, 0, 2)

  theme <- ttheme_awesome(core_size=unit(5, "mm"), color_palette=color_palette, color_breaks=color_breaks)
  dscale <- -log10(SummarizedExperiment::assays(DEXP)$pvalue) + 1
  dcolor <- SummarizedExperiment::assays(DEXP)$sign

  g <- table_extra_grob(dscale=dscale, dcolor=dcolor, 
                        rows=rownames(dscale), cols=colnames(dscale),
                        cols_more=list("n1="=SummarizedExperiment::colData(DBS)$description, 
                                       "n2="=SummarizedExperiment::colData(DBS)$description), 
                        rows_more=NULL,
                        theme=theme)

  out <- plot_grob(g, name="table_extra_grob_genes.pdf", width=5, height=12)
  expect_true(out$plot_success)
})

test_that("extra table grob genes with dscale_min and dscale_max works", {
  color_palette <- c("#6e0d25", "#c6ca53")
  color_breaks <- c(-2, 0, 2)

  theme <- ttheme_awesome(core_size=unit(5, "mm"), 
                          color_palette=color_palette, 
                          color_breaks=color_breaks)
  dscale <- -log10(SummarizedExperiment::assays(DEXP)$pvalue) + 1
  dcolor <- SummarizedExperiment::assays(DEXP)$sign

  g <- table_extra_grob(dscale=dscale, dcolor=dcolor, 
                        rows=rownames(dscale), cols=colnames(dscale),
                        cols_more=list("n1="=SummarizedExperiment::colData(DBS)$description), 
                        rows_more=NULL,
                        dscale_min=-log10(0.05),
                        dscale_max=NULL,
                        theme=theme)

  out <- plot_grob(g, name="table_extra_grob_genes_dmin_dmax.pdf", width=5, height=12)
  expect_true(out$plot_success)
})

test_that("extra table grob genes with scale breaks works", {
  color_palette <- c("#6e0d25", "#c6ca53")
  color_breaks <- c(-2, 0, 2)
  scale_breaks <- c(1, 2, 5)

  theme <- ttheme_awesome(core_size=unit(5, "mm"), 
                          scale_breaks=scale_breaks,
                          color_palette=color_palette, 
                          color_breaks=color_breaks)
  dscale <- -log10(SummarizedExperiment::assays(DEXP)$pvalue) + 1
  dcolor <- SummarizedExperiment::assays(DEXP)$sign

  g <- table_extra_grob(dscale=dscale, dcolor=dcolor, 
                        rows=rownames(dscale), cols=colnames(dscale),
                        cols_more=list("n1="=SummarizedExperiment::colData(DBS)$description), 
                        rows_more=NULL,
                        dscale_min=NULL,
                        dscale_max=NULL,
                        theme=theme)

  out <- plot_grob(g, name="table_extra_grob_genes_dmin_dmax_with_scale_breaks.pdf", width=5, height=12)
  expect_true(out$plot_success)
})

test_that("extra table grob on pcawg works", {

  dscale <- pcawg_counts %>%
    group_by(Cancer.Types) %>%
    group_by(n=n(), .add=TRUE) %>%
    summarize_at(vars(-Sample.Names, -Accuracy), ~sum(.x>0)) %>%
    mutate_at(vars(-Cancer.Types,-n), ~./n)

  cols_more <- list("n="=dscale$n)
  dscale$n <- NULL
  dscale <- column_to_rownames(.data=dscale, var="Cancer.Types")
  dscale <- t(as.matrix(dscale))
  
  scale_breaks <- seq(from=0, to=10, by=0.1)

  dcolor <- pcawg_counts %>%
    group_by(Cancer.Types) %>%
    summarize_at(vars(-Sample.Names, -Accuracy), ~median(.x*1e6/3.2e9))
  dcolor <- column_to_rownames(.data=dcolor, var="Cancer.Types")
  dcolor <- t(as.matrix(dcolor))

  color_palette <- c("#ffc651", "#ffa759", "#ff8962", "#ff6b6b", "#cc6999", "#9968c8", "#6767f8", "#4459ce", "#224ba5", 
                     "#013d7c")
  color_breaks <- c(0, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10, 25, 1e6)

  theme <- ttheme_awesome(base_size=12,
                          core_size=unit(5, "mm"), 
                          scale_breaks=scale_breaks,
                          color_palette=color_palette, 
                          color_breaks=color_breaks)

  g <- table_extra_grob(dscale=dscale, dcolor=dcolor, 
                        rows=rownames(dscale), cols=colnames(dscale),
                        cols_more=cols_more, rows_more=NULL,
                        theme=theme)

  out <- plot_grob(g, name="table_extra_grob_pcawg.pdf", width=12, height=16)
  expect_true(out$plot_success)
})
