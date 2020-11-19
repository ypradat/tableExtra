test_that("extra table grob", {
  theme <- ttheme_awesome(core_size=unit(5, "mm"))
  d <- SummarizedExperiment::assays(DBS)$proportion

  g <- extra_table_grob(d, rows=rownames(d), cols=colnames(d),
                        theme=theme)

  out <- plot_grob(g, name="extra_table_grob.pdf", width=4, height=4)
  expect_true(out$plot_success)
})

test_that("extra table grob cols more", {
  theme <- ttheme_awesome(core_size=unit(5, "mm"))
  d <- SummarizedExperiment::assays(DBS)$proportion

  g <- extra_table_grob(d, rows=rownames(d), cols=colnames(d),
                        cols_more=list("n="=SummarizedExperiment::colData(DBS)$description),
                        theme=theme)

  out <- plot_grob(g, name="extra_table_grob_cols_more.pdf", width=4, height=4)
  expect_true(out$plot_success)
})

test_that("extra table grob cols more rows more no color", {
  theme <- ttheme_awesome(core_size=unit(5, "mm"))
  d <- SummarizedExperiment::assays(DBS)$proportion

  g <- extra_table_grob(d, rows=rownames(d), cols=colnames(d),
                        cols_more=list("n="=SummarizedExperiment::colData(DBS)$description),
                        rows_more=SummarizedExperiment::rowData(DBS)$description, rows_more_title="Proposied aetiology",
                        theme=theme)

  out <- plot_grob(g, name="extra_table_grob_cols_more_rows_more_no_color.pdf", width=5, height=4)
  expect_true(out$plot_success)
})

test_that("extra table grob cols more rows more with color", {
  pal <- c("#ffc651", "#ffa759", "#ff8962", "#ff6b6b", "#cc6999", "#9968c8", "#6767f8", "#4459ce", "#224ba5", "#013d7c")
  pal_breaks <- c(0, 0.001,0.005,0.008,0.01,0.02,0.03,0.05,0.1,0.5,1)

  theme <- ttheme_awesome(core_size=unit(5, "mm"), pal=pal, pal_breaks=pal_breaks)
  dscale <- SummarizedExperiment::assays(DBS)$proportion
  dcolor <- SummarizedExperiment::assays(DBS)$median

  g <- extra_table_grob(dscale=dscale, dcolor=dcolor, 
                        rows=rownames(dscale), cols=colnames(dscale),
                        cols_more=list("n="=SummarizedExperiment::colData(DBS)$description),
                        rows_more=SummarizedExperiment::rowData(DBS)$description, rows_more_title="Proposied aetiology",
                        theme=theme)

  out <- plot_grob(g, name="extra_table_grob_cols_more_rows_more_with_color.pdf", width=5, height=4)
  expect_true(out$plot_success)
})

test_that("extra table grob genes work", {
  pal <- c("#6e0d25", "#c6ca53")
  pal_breaks <- c(-2, 0, 2)

  theme <- ttheme_awesome(core_size=unit(5, "mm"), pal=pal, pal_breaks=pal_breaks)
  dscale <- SummarizedExperiment::assays(DEXP)$pvalue
  dcolor <- SummarizedExperiment::assays(DEXP)$sign

  g <- extra_table_grob(dscale=dscale, dcolor=dcolor, 
                        rows=rownames(dscale), cols=colnames(dscale),
                        cols_more=list("n1="=SummarizedExperiment::colData(DBS)$description, 
                                       "n2="=SummarizedExperiment::colData(DBS)$description), 
                        rows_more=NULL,
                        theme=theme)

  out <- plot_grob(g, name="extra_table_grob_cols_more_with_color_genes.pdf", width=5, height=12)
  expect_true(out$plot_success)
})
