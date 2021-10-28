test_that("draw table extra with rows more", {
  
  # get tables for plot
  plot_data <- pcawg_plot_data()

  # draw
  output <- file.path(tempdir(), "table_extra_pcawg.pdf")
  draw_table_extra(dscale=plot_data$dscale, theme=plot_data$theme, output=output,
                   dcolor=plot_data$dcolor, cols_more=plot_data$cols_more, rows_more=plot_data$rows_more,
                   dscale_title_legend="Prop of tumors with the signature",
                   dcolor_title_legend="Median mut/Mb due to signature")
  expect_true(file.exists(output))
})


test_that("draw table extra without rows more no legend", {
  
  # get tables for plot
  plot_data <- pcawg_plot_data()
  plot_data$theme$legend$show <- F

  # draw
  output <- file.path(tempdir(),
                      "table_extra_pcawg_no_rows_more_no_legend.pdf")
  draw_table_extra(dscale=plot_data$dscale, theme=plot_data$theme, output=output,
                   dcolor=plot_data$dcolor, cols_more=plot_data$cols_more, rows_more=NULL,
                   dscale_title_legend="Prop of tumors with the signature",
                   dcolor_title_legend="Median mut/Mb due to signature")
  expect_true(file.exists(output))
})


test_that("draw table extra without rows more legend center", {
  
  # get tables for plot
  plot_data <- pcawg_plot_data()
  plot_data$theme$legend$position <- "top_center"

  # draw
  output <- file.path(tempdir(),
                      "table_extra_pcawg_no_rows_more_legend_center.pdf")
  draw_table_extra(dscale=plot_data$dscale, theme=plot_data$theme, output=output,
                   dcolor=plot_data$dcolor, cols_more=plot_data$cols_more, rows_more=NULL,
                   dscale_title_legend="Prop of tumors with the signature",
                   dcolor_title_legend="Median mut/Mb due to signature")
  expect_true(file.exists(output))
})


test_that("draw table extra legend top left", {
  
  # get tables for plot
  plot_data <- pcawg_plot_data()
  plot_data$theme$legend$position <- "top_left"

  # draw
  output <- file.path(tempdir(),
                      "table_extra_pcawg_no_rows_more_legend_top_left.pdf")
  draw_table_extra(dscale=plot_data$dscale, theme=plot_data$theme, output=output,
                   dcolor=plot_data$dcolor, cols_more=plot_data$cols_more, rows_more=NULL,
                   dscale_title_legend="Prop of tumors with the signature",
                   dcolor_title_legend="Median mut/Mb due to signature")
  expect_true(file.exists(output))
})


test_that("draw table extra legend top right", {
  
  # get tables for plot
  plot_data <- pcawg_plot_data()
  plot_data$theme$legend$position <- "top_right"

  # draw
  output <- file.path(tempdir(),
                      "table_extra_pcawg_no_rows_more_legend_top_right.pdf")
  draw_table_extra(dscale=plot_data$dscale, theme=plot_data$theme, output=output,
                   dcolor=plot_data$dcolor, cols_more=plot_data$cols_more, rows_more=NULL,
                   dscale_title_legend="Prop of tumors with the signature", margin_x=unit(0.5, "inches"),
                   dcolor_title_legend="Median mut/Mb due to signature")
  expect_true(file.exists(output))
})
