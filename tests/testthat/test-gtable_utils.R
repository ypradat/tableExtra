test_that("rbind_2 on text row", {
  theme <- ttheme_awesome()
  d <- t(SummarizedExperiment::colData(DBS)$description)

  g1 <- gtable_table(d, name="colhead-1",
                    fg_fun = theme$colhead$fg_fun, 
                    bg_fun = theme$colhead$bg_fun, 
                    fg_params = theme$colhead$fg_params, 
                    bg_params = theme$colhead$bg_params, 
                    padding=theme$colhead$padding)

  g2 <- gtable_table(d, name="colhead-2",
                    fg_fun = theme$colhead$fg_fun, 
                    bg_fun = theme$colhead$bg_fun, 
                    fg_params = theme$colhead$fg_params, 
                    bg_params = theme$colhead$bg_params, 
                    padding=theme$colhead$padding)

  g <- rbind_2(g1, g2, "max", height=theme$core$padding[1])

  out <- plot_grob(g, name="gtable_rbind_2_text_row.pdf", width=4, height=1)
  expect_true(out$plot_success)
})

test_that("rbind_2 on text mat", {
  theme <- ttheme_awesome()
  d <- SummarizedExperiment::rowData(DBS)$name
  d <- matrix(rep(d, 5), nrow=5, byrow=T)

  g1 <- gtable_table(d, name="rowhead-1",
                     fg_fun = theme$rowhead$fg_fun, 
                     bg_fun = theme$rowhead$bg_fun, 
                     fg_params = theme$rowhead$fg_params, 
                     bg_params = theme$rowhead$bg_params, 
                     padding=theme$rowhead$padding)

  g2 <- gtable_table(d, name="rowhead-2",
                     fg_fun = theme$rowhead$fg_fun, 
                     bg_fun = theme$rowhead$bg_fun, 
                     fg_params = theme$rowhead$fg_params, 
                     bg_params = theme$rowhead$bg_params, 
                     padding=theme$rowhead$padding)

  g <- rbind_2(g1, g2, size="first", height=theme$rowhead$padding[1])

  out <- plot_grob(g, name="gtable_rbind_2_text_mat.pdf", width=4, height=3)
  expect_true(out$plot_success)
})


test_that("cbind_2 on table circle", {
  theme <- ttheme_awesome()
  d <- SummarizedExperiment::assays(DBS)$proportion
  widths <- rep(theme$core$size, ncol(d))
  heights <- rep(theme$core$size, nrow(d))

  g1 <- gtable_table(d, name="circle",
                     widths=widths,
                     heights=heights,
                     fg_fun = theme$core$fg_fun, 
                     bg_fun = theme$core$bg_fun, 
                     fg_params = theme$core$fg_params, 
                     bg_params = theme$core$bg_params, 
                     padding=theme$core$padding,
                     n_cat=theme$core$n_cat,
                     r_max=0.5*theme$core$size)

  g2 <- gtable_table(d, name="circle",
                     widths=widths,
                     heights=heights,
                     fg_fun = theme$core$fg_fun, 
                     bg_fun = theme$core$bg_fun, 
                     fg_params = theme$core$fg_params, 
                     bg_params = theme$core$bg_params, 
                     padding=theme$core$padding,
                     n_cat=theme$core$n_cat,
                     r_max=0.5*theme$core$size)

  g <- cbind_2(g1, g2, "max", width=theme$core$padding[2])

  out <- plot_grob(g, name="gtable_circle_cbind.pdf", width=10, height=6)
  expect_true(out$plot_success)
})
 
test_that("rbind_2 on table text - circle", {
  theme <- ttheme_awesome()
  d <- SummarizedExperiment::assays(DBS)$proportion
  col <- t(colnames(d))
  #col <- t(colData(DBS)$description)

  #heights <- unit(30, "mm")
  g1 <- gtable_table(col, name="colhead-1",
                     fg_fun = theme$colhead$fg_fun, 
                     bg_fun = theme$colhead$bg_fun, 
                     fg_params = theme$colhead$fg_params, 
                     bg_params = theme$colhead$bg_params, 
                     padding=theme$colhead$padding)

  widths <- rep(theme$core$size, ncol(d))
  heights <- rep(theme$core$size, nrow(d))
  g2 <- gtable_table(d, name="circle",
                     widths=widths,
                     heights=heights,
                     fg_fun = theme$core$fg_fun, 
                     bg_fun = theme$core$bg_fun, 
                     fg_params = theme$core$fg_params, 
                     bg_params = theme$core$bg_params, 
                     padding=theme$core$padding,
                     n_cat=theme$core$n_cat,
                     r_max=0.5*theme$core$size)

  g <- rbind_2(g1, g2, size="last", height=theme$core$padding[1])

  out <- plot_grob(g, name="gtable_rbind_2_text_circle.pdf")
  expect_true(out$plot_success)
})
