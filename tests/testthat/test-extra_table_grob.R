test_that("extra table grob", {
  load("testdata/DBS.rda")

  theme <- ttheme_awesome()
  d <- SummarizedExperiment::assays(DBS)$proportion

  g <- extra_table_grob(d, rows=rownames(d), cols=colnames(d),
                        theme=theme)

  out <- plot_grob(g, name="extra_table_grob_big.pdf", width=6, height=6)
  expect_true(out$plot_success)
})

test_that("extra table grob", {
  load("testdata/DBS.rda")

  theme <- ttheme_awesome(core_size=unit(5, "mm"))
  d <- SummarizedExperiment::assays(DBS)$proportion

  g <- extra_table_grob(d, rows=rownames(d), cols=colnames(d),
                        theme=theme)

  out <- plot_grob(g, name="extra_table_grob.pdf", width=4, height=4)
  expect_true(out$plot_success)
})

test_that("extra table grob cols more", {
  load("testdata/DBS.rda")

  theme <- ttheme_awesome(core_size=unit(5, "mm"))
  d <- SummarizedExperiment::assays(DBS)$proportion

  g <- extra_table_grob(d, rows=rownames(d), cols=colnames(d),
                        cols_more=SummarizedExperiment::colData(DBS)$description, cols_more_title="n=",
                        theme=theme)

  out <- plot_grob(g, name="extra_table_grob_cols_more.pdf", width=4, height=4)
  expect_true(out$plot_success)
})

test_that("extra table grob cols more rows more", {
  load("testdata/DBS.rda")

  theme <- ttheme_awesome(core_size=unit(5, "mm"))
  d <- SummarizedExperiment::assays(DBS)$proportion

  g <- extra_table_grob(d, rows=rownames(d), cols=colnames(d),
                        cols_more=SummarizedExperiment::colData(DBS)$description, cols_more_title="n=",
                        rows_more=SummarizedExperiment::rowData(DBS)$description, rows_more_title="Proposied aetiology",
                        theme=theme)

  out <- plot_grob(g, name="extra_table_grob_cols_more_rows_more.pdf", width=5, height=4)
  expect_true(out$plot_success)
})
