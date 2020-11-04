test_that("rbind_2 on text row", {
  load("testdata/DBS.rda")

  theme <- ttheme_awesome()
  d <- t(SummarizedExperiment::colData(DBS)$description)

  g1 <- gtable_text(d, name="colhead-1",
                    fg_fun = theme$colhead$fg_fun, 
                    bg_fun = theme$colhead$bg_fun, 
                    fg_params = theme$colhead$fg_params, 
                    bg_params = theme$colhead$bg_params, 
                    padding=theme$colhead$padding)

  g2 <- gtable_text(d, name="colhead-2",
                    fg_fun = theme$colhead$fg_fun, 
                    bg_fun = theme$colhead$bg_fun, 
                    fg_params = theme$colhead$fg_params, 
                    bg_params = theme$colhead$bg_params, 
                    padding=theme$colhead$padding)

  g <- rbind_2(g1, g2, "max", height=theme$core$padding[1])

  out <- plot_grob(g, name="gtable_rbind_2_text_row.pdf")
  expect_true(out$plot.success)
})

test_that("rbind_2 on text mat", {
  load("testdata/DBS.rda")

  theme <- ttheme_awesome()
  d <- SummarizedExperiment::rowData(DBS)$name
  d <- matrix(rep(d, 5), nrow=5, byrow=T)

  g1 <- gtable_text(d, name="rowhead-1",
                    fg_fun = theme$rowhead$fg_fun, 
                    bg_fun = theme$rowhead$bg_fun, 
                    fg_params = theme$rowhead$fg_params, 
                    bg_params = theme$rowhead$bg_params, 
                    padding=theme$rowhead$padding)

  g2 <- gtable_text(d, name="rowhead-2",
                    fg_fun = theme$rowhead$fg_fun, 
                    bg_fun = theme$rowhead$bg_fun, 
                    fg_params = theme$rowhead$fg_params, 
                    bg_params = theme$rowhead$bg_params, 
                    padding=theme$rowhead$padding)

  g <- rbind(g1, g2, size="first")

  out <- plot_grob(g, name="gtable_rbind_2_text_mat.pdf")
  expect_true(out$plot.success)
})


test_that("cbind_2 on table circle", {
  load("testdata/DBS.rda")

  theme <- ttheme_awesome()

  d <- SummarizedExperiment::assays(DBS)$proportion
  d <- norm_and_cat(d,ncat=theme$core$ncircle, vmax=0.5)

  g1 <- gtable_circle(d, name="circle",
                      fg_fun = theme$core$fg_fun, 
                      bg_fun = theme$core$bg_fun, 
                      fg_params = theme$core$fg_params, 
                      bg_params = theme$core$bg_params, 
                      padding=theme$core$padding)

  g2 <- gtable_circle(d, name="circle",
                      fg_fun = theme$core$fg_fun, 
                      bg_fun = theme$core$bg_fun, 
                      fg_params = theme$core$fg_params, 
                      bg_params = theme$core$bg_params, 
                      padding=theme$core$padding)

  g <- cbind_2(g1, g2, "max", width=theme$core$padding[1])

  out <- plot_grob(g, name="gtable_circle_cbind.pdf")
  expect_true(out$plot.success)
})

test_that("rbind_2 on table text - circle", {
  load("testdata/DBS.rda")

  theme <- ttheme_awesome()

  d <- SummarizedExperiment::assays(DBS)$proportion
  d <- norm_and_cat(d,ncat=theme$core$ncircle, vmax=0.5)
  col <- t(colnames(d))

  g1 <- gtable_text(col, name="colhead-1",
                    fg_fun = theme$colhead$fg_fun, 
                    bg_fun = theme$colhead$bg_fun, 
                    fg_params = theme$colhead$fg_params, 
                    bg_params = theme$colhead$bg_params, 
                    padding=theme$colhead$padding)

  g2 <- gtable_circle(d, name="circle",
                      fg_fun = theme$core$fg_fun, 
                      bg_fun = theme$core$bg_fun, 
                      fg_params = theme$core$fg_params, 
                      bg_params = theme$core$bg_params, 
                      padding=theme$core$padding)

  #g <- rbind_2(g1, g2, size="last", height=theme$core$padding[2])
  g <- rbind(g1, g2, size="last")

  out <- plot_grob(g, name="gtable_rbind_2_text_circle.pdf")
  expect_true(out$plot.success)
})
