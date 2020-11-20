test_that("gtable text row no padding", {
  theme <- ttheme_awesome(padding=unit(c(0,0), "mm"))
  d <- t(SummarizedExperiment::colData(DBS)$description)
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

  out <- plot_grob(gc, name="gtable_text_row_no_padding.pdf", width=8, height=1)
  expect_true(out$plot_success)

})

test_that("gtable text row with padding", {
  theme <- ttheme_awesome(padding=unit(c(2,0), "mm"))
  d <- t(SummarizedExperiment::colData(DBS)$description)
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

  out <- plot_grob(gc, name="gtable_text_row_with_padding.pdf", width=8, height=1)
  expect_true(out$plot_success)
})

test_that("gtable text col", {
  theme <- ttheme_awesome(padding=unit(c(0,0), "mm"))
  d <- SummarizedExperiment::rowData(DBS)["name"]
  heights <- rep(theme$core$size, nrow(d))

  gr <- gtable_table(d, name="rowhead",
                     heights=heights,
                     fg_fun=theme$rowhead$fg_fun, 
                     bg_fun=theme$rowhead$bg_fun, 
                     fg_params=theme$rowhead$fg_params, 
                     bg_params=theme$rowhead$bg_params, 
                     padding=theme$rowhead$padding)

  out <- plot_grob(gr, name="gtable_text_col.pdf", width=1, height=4.5)
  expect_true(out$plot_success)
})

test_that("gtable text mat no padding", {
  theme <- ttheme_awesome(padding=unit(c(0,0), "mm"))
  d <- SummarizedExperiment::rowData(DBS)$name
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

  out <- plot_grob(gr, name="gtable_text_mat_no_padding.pdf")
  expect_true(out$plot_success)
})

test_that("gtable text mat with padding", {
  theme <- ttheme_awesome(padding=unit(c(1,1), "mm"))
  d <- SummarizedExperiment::rowData(DBS)$name
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

  out <- plot_grob(gr, name="gtable_text_mat_with_padding.pdf")
  expect_true(out$plot_success)
})


test_that("gtable circle", {
  theme <- ttheme_awesome()
  d <- SummarizedExperiment::assays(DBS)$proportion
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
                     r_max=0.5*theme$core$size)

  out <- plot_grob(g, name="gtable_circle.pdf")
  expect_true(out$plot_success)
})
