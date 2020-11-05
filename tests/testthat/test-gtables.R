test_that("gtable text row", {
  load("testdata/DBS.rda")

  theme <- ttheme_awesome(base_colour="blue")
  d <- t(SummarizedExperiment::colData(DBS)$description)

  gc <- gtable_table(d, name="colhead",
                     fg_fun = theme$colhead$fg_fun, 
                     bg_fun = theme$colhead$bg_fun, 
                     fg_params = theme$colhead$fg_params, 
                     bg_params = theme$colhead$bg_params, 
                     padding=theme$colhead$padding)

  out <- plot_grob(gc, name="gtable_text_row.pdf", width=4, height=1)
  expect_true(out$plot.success)
})

test_that("gtable text col", {
  load("testdata/DBS.rda")

  theme <- ttheme_awesome()
  d <- SummarizedExperiment::rowData(DBS)$name

  gr <- gtable_table(d, name="rowhead",
                     fg_fun = theme$rowhead$fg_fun, 
                     bg_fun = theme$rowhead$bg_fun, 
                     fg_params = theme$rowhead$fg_params, 
                     bg_params = theme$rowhead$bg_params, 
                     padding=theme$rowhead$padding)

  out <- plot_grob(gr, name="gtable_text_col.pdf", width=1, height=4)
  expect_true(out$plot.success)
})

test_that("gtable text mat", {
  load("testdata/DBS.rda")

  theme <- ttheme_awesome()
  d <- SummarizedExperiment::rowData(DBS)$name
  d <- matrix(rep(d, 5), nrow=5, byrow=T)

  gr <- gtable_table(d, name="rowhead",
                     fg_fun = theme$rowhead$fg_fun, 
                     bg_fun = theme$rowhead$bg_fun, 
                     fg_params = theme$rowhead$fg_params, 
                     bg_params = theme$rowhead$bg_params, 
                     padding=theme$rowhead$padding)

  out <- plot_grob(gr, name="gtable_text_mat.pdf")
  expect_true(out$plot.success)
})

# test_that("gtable circle", {
#   load("testdata/DBS.rda")
# 
#   theme <- ttheme_awesome()
# 
#   d <- SummarizedExperiment::assays(DBS)$proportion
#   d <- norm_and_cat(d,ncat=theme$core$ncircle, vmax=0.5)
# 
#   g <- gtable_circle(d, name="circle",
#                      fg_fun = theme$core$fg_fun, 
#                      bg_fun = theme$core$bg_fun, 
#                      fg_params = theme$core$fg_params, 
#                      bg_params = theme$core$bg_params, 
#                      padding=theme$core$padding)
# 
#   out <- plot_grob(g, name="gtable_circle.pdf")
#   expect_true(out$plot.success)
# })
