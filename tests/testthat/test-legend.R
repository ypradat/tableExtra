test_that("legend circle grob", {
  
  scale_breaks <- seq(from=0, to=1, by=0.1)
  theme <- ttheme_awesome(base_size=12,
                          rep_mode="col",
                          core_size=5, 
                          scale_breaks=scale_breaks,
                          color_palette=NULL, 
                          color_breaks=NULL)

  g <- gtable_legend(d=t(scale_breaks[2:length(scale_breaks)]),
                     labels=scale_breaks,
                     widths=rep(theme$core$size, length(scale_breaks)-1),
                     heights=theme$core$size,
                     fg_fun=theme$core$fg_fun, 
                     bg_fun=theme$core$bg_fun, 
                     fg_params=theme$core$fg_params, 
                     bg_params=theme$core$bg_params, 
                     title_label="Prop of tumors with the signature",
                     title_gp=gpar(fontsize=8),
                     title_x=0.5*(length(scale_breaks)-1)*theme$core$size,
                     title_y=1.5*theme$core$size,
                     labels_gp=gpar(fontsize=6),
                     labels_pad=-1,
                     padding=theme$core$padding,
                     scale_breaks=theme$core$scale_breaks,
                     rep_mode=theme$core$rep_mode,
                     r_min=theme$core$scale_ratio*0.5*theme$core$size,
                     r_max=0.5*theme$core$size)

  ## add the background
  expect_true(is(g, "grob"))
  out <- plot_grob(g, name="legend_circle_grob.pdf", width=5, height=1)

  expect_true(out$plot_success)
})


test_that("legend rect grob", {
  
  color_palette <- c("#ffc651", "#ffa759", "#ff8962", "#ff6b6b", "#cc6999", "#9968c8", "#6767f8", "#4459ce", "#224ba5", 
                     "#013d7c")
  color_breaks <- c(0, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10, 25, 1e6)

  theme <- ttheme_awesome(base_size=12,
                          rep_mode="col",
                          core_size=5, 
                          color_palette=color_palette, 
                          color_breaks=color_breaks)

  fg_fun <- theme$core$bg_fun
  fg_params <- theme$core$bg_params
  fg_params$fill <- NULL

  g <- gtable_legend(d=t(color_breaks[1:length(color_breaks)-1]),
                     labels=color_breaks,
                     widths=rep(theme$core$size, length(color_breaks)-1),
                     heights=theme$core$size,
                     fg_fun=fg_fun,
                     fg_params=fg_params,
                     bg_fun=NULL,
                     bg_params=NULL,
                     title_label="Median mut/Mb due to signature",
                     title_gp=gpar(fontsize=8),
                     title_x=0.5*(length(color_breaks)-1)*theme$core$size,
                     title_y=1.5*theme$core$size,
                     orientation="horizontal",
                     labels_gp=gpar(fontsize=6),
                     labels_pad=-1,
                     padding=theme$core$padding,
                     color_breaks=color_breaks,
                     color_palette=color_palette)

  ## add the background
  expect_true(is(g, "grob"))
  out <- plot_grob(g, name="legend_rect_grob.pdf", width=5, height=1)

  expect_true(out$plot_success)
})


test_that("legend rect grob vertical", {
  
  color_palette <- c("#ffc651", "#ffa759", "#ff8962", "#ff6b6b", "#cc6999", "#9968c8", "#6767f8", "#4459ce", "#224ba5", 
                     "#013d7c")
  color_breaks <- c(0, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10, 25, 1e6)

  theme <- ttheme_awesome(base_size=12,
                          rep_mode="col",
                          core_size=5, 
                          color_palette=color_palette, 
                          color_breaks=color_breaks)

  fg_fun <- theme$core$bg_fun
  fg_params <- theme$core$bg_params
  fg_params$fill <- NULL

  g <- gtable_legend(d=color_breaks[1:length(color_breaks)-1],
                     labels=color_breaks,
                     widths=theme$core$size,
                     heights=rep(theme$core$size, length(color_breaks)-1),
                     fg_fun=fg_fun,
                     fg_params=fg_params,
                     bg_fun=NULL,
                     bg_params=NULL,
                     title_label="Title",
                     title_gp=gpar(fontsize=8),
                     title_y=-0.5*(length(color_breaks)-3)*(theme$core$size+theme$core$padding[1]),
                     title_x=2*theme$core$size,
                     orientation="vertical",
                     labels_gp=gpar(fontsize=6),
                     labels_pad=-3,
                     padding=theme$core$padding,
                     color_breaks=color_breaks,
                     color_palette=color_palette)

  ## add the background
  expect_true(is(g, "grob"))
  out <- plot_grob(g, name="legend_rect_grob_vertical.pdf", width=1, height=3)

  expect_true(out$plot_success)
})
