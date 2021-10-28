test_that("rbind_2 and gtable_rbind on text row works", {
  
  theme <- ttheme_awesome()
  d <- t(DBS$colData$description)

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
  width <- convertUnit(ncol(d)*theme$core$size + theme$core$padding[1]*(ncol(d)-1), "inches")
  height <- convertUnit(nrow(d)*theme$core$size + theme$core$padding[2]*(nrow(d)-1), "inches")
  out <- plot_grob(g, name="rbind_2_text_row.pdf", width=width, height=2*height)
  expect_true(out$plot_success)

  g <- gtable_rbind(g1, g2, g1, g2, size="max", height=theme$core$padding[1])
  out <- plot_grob(g, name="gtable_rbind_4_text_row.pdf", width=4, height=1)
  expect_true(out$plot_success)
})


test_that("rbind_2 and gtable_rbind on text mat works", {
  
  theme <- ttheme_awesome()
  d <- DBS$rowData$name
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
  out <- plot_grob(g, name="rbind_2_text_mat.pdf", width=4, height=3)
  expect_true(out$plot_success)


  g <- gtable_rbind(g1, g2, g1, g2, size="max", height=theme$core$padding[1])
  out <- plot_grob(g, name="gtable_rbind_4_text_mat.pdf", width=6, height=3)
  expect_true(out$plot_success)
})


test_that("cbind_2 on table circle works", {
  
  theme <- ttheme_awesome()
  d <- DBS$assays$proportion
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
                     scale_breaks=theme$core$scale_breaks,
                     dscale_min=NULL,
                     dscale_max=NULL,
                     r_min=0.1*theme$core$size,
                     r_max=0.5*theme$core$size)

  g2 <- gtable_table(d, name="circle",
                     widths=widths,
                     heights=heights,
                     fg_fun = theme$core$fg_fun, 
                     bg_fun = theme$core$bg_fun, 
                     fg_params = theme$core$fg_params, 
                     bg_params = theme$core$bg_params, 
                     padding=theme$core$padding,
                     scale_breaks=theme$core$scale_breaks,
                     dscale_min=NULL,
                     dscale_max=NULL,
                     r_min=0.1*theme$core$size,
                     r_max=0.5*theme$core$size)

  g <- cbind_2(g1, g2, "max", width=theme$core$padding[2])
  out <- plot_grob(g, name="cbind_2_circles.pdf", width=10, height=6)
  expect_true(out$plot_success)


  g <- gtable_cbind(g1, g2, g1, g2, size="max", width=theme$core$padding[2])
  out <- plot_grob(g, name="gtable_cbind_4_circles.pdf", width=20, height=6)
  expect_true(out$plot_success)
})
 

test_that("rbind_2 on table text - circle works", {
  
  theme <- ttheme_awesome()
  d <- DBS$assays$proportion
  col <- t(colnames(d))

  g1 <- gtable_table(col, name="colhead-1",
                     fg_fun = theme$colhead$fg_fun, 
                     bg_fun = theme$colhead$bg_fun, 
                     fg_params = theme$colhead$fg_params, 
                     bg_params = theme$colhead$bg_params, 
                     padding=theme$colhead$padding)

  labels <- as.vector(col)
  label_max <- labels[which(nchar(labels)==max(nchar(labels)))]
  fontsize <- theme$colhead$fg_params$fontsize
  height1 <- unit(strwidth(label_max, font=1, cex=fontsize/par()$ps, units='in'), "inches")

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
                     scale_breaks=theme$core$scale_breaks,
                     dscale_min=NULL,
                     dscale_max=NULL,
                     r_min=0.1*theme$core$size,
                     r_max=0.5*theme$core$size)

  g <- rbind_2(g1, g2, size="last", height=theme$core$padding[1])

  width <- convertUnit(sum(widths) + theme$core$padding[1]*(length(widths)-1), "inches")
  height2 <- convertUnit(sum(heights) + theme$core$padding[2]*(length(heights)-1), "inches")
  height <- height1 + convertUnit(theme$core$padding[[1]],"inches") + height2
  margin <- unit(0.25,"inches")
  out <- plot_grob(g, name="rbind_2_text_circle.pdf", width=width+margin, height=height+margin)
  expect_true(out$plot_success)
})
