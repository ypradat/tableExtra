
test_that("text grob", {
  g <- text_grob(label="test",
                 col="black")

  expect_true(is(g, "grob"))
  expect_true(is(g, "text"))

  out <- plot_grob(g, name="text_grob.pdf", width=1, height=1)
  expect_true(out$plot.success)
})

test_that("rect grob", {
  g <- rect_grob(fill="grey80",
                 col="black",
                 lty="solid",
                 cex=1)

  expect_true(is(g, "grob"))
  expect_true(is(g, "rect"))

  out <- plot_grob(g, name="rect_grob.pdf", width=1, height=1)
  expect_true(out$plot.success)
})

test_that("circle grob", {
  g <- circle_grob(fill="grey80",
                   col="black",
                   lty="solid",
                   cex=1)

  expect_true(is(g, "grob"))
  expect_true(is(g, "circle"))

  out <- plot_grob(g, name="circle_grob.pdf", width=1, height=1)
  expect_true(out$plot.success)
})

test_that("circle square grob", {
  gc <- circle_grob(r=0.5,
                    lwd=0,
                    fill="grey80",
                    col="black",
                    lty="solid",
                    cex=1)

  gr <- rect_grob(fill="white",
                  x = 1,
                  y = 1,
                  col="black",
                  lty="solid",
                  cex=1)

  g <- grid::gTree(children=grid::gList(gr, gc))

  expect_true(is(g, "grob"))

  out <- plot_grob(g, name="rect_circle_grob.pdf", width=1, height=1)
  expect_true(out$plot.success)
})
