test_that("text grob", {
  
  g <- text_grob(label="test", fontsize=24, col="black", lineheight=1)

  expect_true(is(g, "grob"))
  expect_true(is(g, "text"))

  width <- strwidth(g$label, font=1, cex=g$gp$fontsize/par()$ps, units='in')
  height <- strheight(g$label, font=1, cex=g$gp$fontsize/par()$ps, units='in')
  out <- plot_grob(g, name="text_grob.pdf", width=width, height=height)
  expect_true(out$plot_success)
})


test_that("rect grob", {
  
  g <- rect_grob(width=unit(10, "mm"),
                 height=unit(10, "mm"),
                 fill="grey80",
                 col="black",
                 lty="solid",
                 cex=1)

  expect_true(is(g, "grob"))
  expect_true(is(g, "rect"))

  out <- plot_grob(g, name="rect_grob.pdf", width=1, height=1)
  expect_true(out$plot_success)
})


test_that("rect grob", {
  
  g <- rect_grob(x=unit(0.5, "mm"),
                 y=unit(0.5, "mm"),
                 width=unit(10,"mm"),
                 height=unit(10,"mm"),
                 fill="grey80",
                 col="black",
                 lty="solid",
                 cex=1)

  expect_true(is(g, "grob"))
  expect_true(is(g, "rect"))

  out <- plot_grob(g, name="rect_grob_xy.pdf", width=1, height=1)
  expect_true(out$plot_success)
})


test_that("circle grob", {
  
  g <- circle_grob(r=unit(5, "mm"),
                   fill="grey80",
                   col="black",
                   lty="solid",
                   cex=1)

  expect_true(is(g, "grob"))
  expect_true(is(g, "circle"))

  out <- plot_grob(g, name="circle_grob.pdf", width=1, height=1)
  expect_true(out$plot_success)
})


test_that("circle rect grob", {
  
  gc <- circle_grob(r=unit(5, "mm"),
                    lwd=0,
                    fill="grey80",
                    col="black",
                    lty="solid",
                    cex=1)

  gr <- rect_grob(width=unit(10,"mm"),
                  height=unit(10,"mm"),
                  fill="white",
                  col="black",
                  lty="solid",
                  cex=1)

  g <- grid::gTree(children=grid::gList(gr, gc))

  expect_true(is(g, "grob"))
  out <- plot_grob(g, name="rect_circle_grob.pdf", width=1, height=1)
  expect_true(out$plot_success)
})


test_that("circle rect grob viewport", {
  
  gc <- circle_grob(r=unit(5, "mm"),
                    lwd=0,
                    fill="grey80",
                    col="black",
                    lty="solid",
                    cex=1)

  gr <- rect_grob(width=unit(10,"mm"),
                  height=unit(10,"mm"),
                  fill="white",
                  col="black",
                  lty="solid",
                  cex=1)


  grDevices::pdf(file=file.path(tempdir(), "rect_circle_grob_vp.pdf"),
                 width=1,
                 height=1,
                 onefile=T)

  grid.draw(gc)

  vp <- viewport(x = 0.5, y = 0.5, 
                 width = 0.5, height = 0.5,
                 just = c("left", "bottom"))
  pushViewport(vp)
  grid.draw(roundrectGrob())
  grid.draw(gr)
  popViewport()
  grDevices::dev.off()

  expect_true(file.exists(file.path(tempdir(), "rect_circle_grob_vp.pdf")))
})

test_that("circle rect grob small", {
  
  gc <- circle_grob(r=unit(5, "mm"),
                    lwd=0,
                    fill="grey80",
                    col="black",
                    lty="solid",
                    cex=1)

  gr <- rect_grob(width=unit(10,"mm"),
                  height=unit(10,"mm"),
                  fill="white",
                  col="black",
                  lty="solid",
                  cex=1)

  g <- grid::gTree(children=grid::gList(gr, gc))
  expect_true(is(g, "grob"))
  out <- plot_grob(g, name="rect_circle_grob_small.pdf", width=4, height=4)
  expect_true(out$plot_success)
})
