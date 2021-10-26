test_that("gtable text row no padding", {
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
