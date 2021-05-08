test_that("extra table grob", {
  theme <- ttheme_awesome(core_size=5)
  d <- DBS$assays$proportion

  g <- table_extra_grob(d, rows=rownames(d), cols=colnames(d),
                        theme=theme)

  out <- plot_grob(g, name="table_extra_grob.pdf", width=4, height=4)
  expect_true(out$plot_success)
})

test_that("extra table grob cols more", {
  theme <- ttheme_awesome(core_size=5)
  d <- DBS$assays$proportion

  g <- table_extra_grob(d, rows=rownames(d), cols=colnames(d),
                        cols_more=list("n="=DBS$colData$description),
                        theme=theme)

  out <- plot_grob(g, name="table_extra_grob_cols_more.pdf", width=4, height=4)
  expect_true(out$plot_success)
})

test_that("extra table grob cols more rows more no color annotated", {
  theme <- ttheme_awesome(core_size=5)
  d <- DBS$assays$proportion
  cols_more <- list("n="=DBS$colData$description)
  rows_more <- list("Proposed aetiology"=DBS$rowData$description)

  g <- table_extra_grob(d, rows=rownames(d), cols=colnames(d),
                        cols_more=cols_more, rows_more=rows_more,
                        theme=theme)


  find_cell <- function(g, row, col, name){
    l <- g$layout
    which(l$t==2*row-1 & l$l==2*col-1 & l$name==name)
  }

  ind <- find_cell(g, 3, 3, "core-bg")
  g$grobs[ind][[1]][["gp"]] <- gpar(col = "red", lwd=3)

  out <- plot_grob(g, name="table_extra_grob_cols_more_rows_more_no_color_annotated.pdf", width=5, height=4)
  expect_true(out$plot_success)
})

test_that("extra table grob cols more rows more with color", {
  color_palette <- c("#ffc651", "#ffa759", "#ff8962", "#ff6b6b", "#cc6999", "#9968c8", "#6767f8", "#4459ce", "#224ba5", 
                     "#013d7c")
  color_breaks <- c(0, 0.001,0.005,0.008,0.01,0.02,0.03,0.05,0.1,0.5,1)

  theme <- ttheme_awesome(core_size=5, color_palette=color_palette, color_breaks=color_breaks)
  dscale <- DBS$assays$proportion
  dcolor <- DBS$assays$median
  cols_more <- list("n="=DBS$colData$description)
  rows_more <- list("Proposed aetiology"=DBS$rowData$description)

  g <- table_extra_grob(dscale=dscale, dcolor=dcolor, 
                        rows=rownames(dscale), cols=colnames(dscale),
                        cols_more=cols_more, rows_more=rows_more,
                        theme=theme)

  out <- plot_grob(g, name="table_extra_grob_cols_more_rows_more_with_color.pdf", width=5, height=4)
  expect_true(out$plot_success)
})

test_that("extra table grob genes work", {
  color_palette <- c("#6e0d25", "#c6ca53")
  color_breaks <- c(-2, 0, 2)

  theme <- ttheme_awesome(core_size=5, color_palette=color_palette, color_breaks=color_breaks)
  dscale <- -log10(DEXP$assays$pvalue) + 1
  dcolor <- DEXP$assays$sign

  g <- table_extra_grob(dscale=dscale, dcolor=dcolor, 
                        rows=rownames(dscale), cols=colnames(dscale),
                        cols_more=list("n1="=DBS$colData$description, 
                                       "n2="=DBS$colData$description), 
                        rows_more=NULL,
                        theme=theme)

  out <- plot_grob(g, name="table_extra_grob_genes.pdf", width=5, height=12)
  expect_true(out$plot_success)
})

test_that("extra table grob genes with dscale_min and dscale_max works", {
  color_palette <- c("#6e0d25", "#c6ca53")
  color_breaks <- c(-2, 0, 2)

  theme <- ttheme_awesome(core_size=5,
                          color_palette=color_palette, 
                          color_breaks=color_breaks)
  dscale <- -log10(DEXP$assays$pvalue) + 1
  dcolor <- DEXP$assays$sign

  g <- table_extra_grob(dscale=dscale, dcolor=dcolor, 
                        rows=rownames(dscale), cols=colnames(dscale),
                        cols_more=list("n1="=DBS$colData$description), 
                        rows_more=NULL,
                        dscale_min=-log10(0.05),
                        dscale_max=NULL,
                        theme=theme)

  out <- plot_grob(g, name="table_extra_grob_genes_dmin_dmax.pdf", width=5, height=12)
  expect_true(out$plot_success)
})

test_that("extra table grob genes with scale breaks works", {
  color_palette <- c("#6e0d25", "#c6ca53")
  color_breaks <- c(-2, 0, 2)
  scale_breaks <- c(1, 2, 5)

  theme <- ttheme_awesome(core_size=5, 
                          scale_breaks=scale_breaks,
                          color_palette=color_palette, 
                          color_breaks=color_breaks)
  dscale <- -log10(DEXP$assays$pvalue) + 1
  dcolor <- DEXP$assays$sign

  g <- table_extra_grob(dscale=dscale, dcolor=dcolor, 
                        rows=rownames(dscale), cols=colnames(dscale),
                        cols_more=list("n1="=DBS$colData$description), 
                        rows_more=NULL,
                        dscale_min=NULL,
                        dscale_max=NULL,
                        theme=theme)

  out <- plot_grob(g, name="table_extra_grob_genes_dmin_dmax_with_scale_breaks.pdf", width=5, height=12)
  expect_true(out$plot_success)
})

test_that("extra table grob on pcawg works", {

  dscale <- pcawg_counts %>%
    group_by(Cancer.Types) %>%
    mutate(n=n()) %>%
    summarize_at(vars(-Sample.Names, -Accuracy), ~sum(.x>0)) %>%
    mutate_at(vars(-Cancer.Types,-n), ~./n)

  cols_more <- list("n="=dscale$n)
  dscale$n <- NULL
  dscale <- column_to_rownames(.data=dscale, var="Cancer.Types")
  dscale <- t(as.matrix(dscale))
  rows_more <- list("Aetiology"=sbs_aetiologies[sbs_aetiologies$Signature %in% rownames(dscale), "Aetiology"])
  
  scale_breaks <- seq(from=0, to=1, by=0.1)

  dcolor <- pcawg_counts %>%
    group_by(Cancer.Types) %>%
    summarize_at(vars(-Sample.Names, -Accuracy), ~median(.[.!=0]*1e6/3.2e9)) %>%
    replace(is.na(.),0)

  dcolor <- column_to_rownames(.data=dcolor, var="Cancer.Types")
  dcolor <- t(as.matrix(dcolor))

  color_palette <- c("#ffc651", "#ffa759", "#ff8962", "#ff6b6b", "#cc6999", "#9968c8", "#6767f8", "#4459ce", "#224ba5", 
                     "#013d7c")
  color_breaks <- c(0, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10, 25, 1e6)

  theme <- ttheme_awesome(base_size=12,
                          rep_mode="col",
                          core_size=5, 
                          scale_breaks=scale_breaks,
                          color_palette=color_palette, 
                          color_breaks=color_breaks)

  # main plot
  g <- table_extra_grob(dscale=dscale, dcolor=dcolor, 
                        rows=rownames(dscale), cols=colnames(dscale),
                        cols_more=cols_more, rows_more=rows_more,
                        theme=theme)

  # legend 1
  g1 <- gtable_legend(d=t(scale_breaks[2:length(scale_breaks)]),
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

  # legend 2
  fg_fun <- theme$core$bg_fun
  fg_params <- theme$core$bg_params
  fg_params$fill <- NULL

  g2 <- gtable_legend(d=t(color_breaks[1:length(color_breaks)-1]),
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

  # second legend
  grDevices::pdf(file=file.path("pdfs_plots", "table_extra_grob_pcawg.pdf"),
                 width=15,
                 height=16,
                 onefile=T)

  grid.draw(g)

  vp1 <- viewport(x = 0.65, y = 0.92, 
                  width = 0.2, height = 0.03,
                  just = c("left", "bottom"))
  pushViewport(vp1)
  grid.draw(g1)
  popViewport()

  vp2 <- viewport(x = 0.65, y = 0.88, 
                  width = 0.2, height = 0.03,
                  just = c("left", "bottom"))
  pushViewport(vp2)
  grid.draw(g2)
  popViewport()

  grDevices::dev.off()

  expect_true(file.exists(file.path("pdfs_plots", "table_extra_grob_pcawg.pdf")))
})

# test_that("extra table grob on prims works", {
#   prism_counts <- read.csv("../../data-raw/prism_counts.csv")
#   prism_counts <- prism_counts %>%
#     group_by(Project_TCGA_More) %>%
#     mutate(n=n())
# 
#   # select primaries
#   threshold_min_n <- 3
#   prism_counts <- prism_counts[prism_counts$n >= threshold_min_n,]
#   prism_counts$n <- NULL
# 
#   dscale <- prism_counts %>%
#     group_by(Project_TCGA_More) %>%
#     mutate(n=n()) %>%
#     summarize_at(vars(-Patient), ~sum(.x>0)) %>%
#     mutate_at(vars(-Project_TCGA_More,-n), ~./n)
# 
#   cols_more <- list("n="=dscale$n)
#   dscale$n <- NULL
#   dscale <- column_to_rownames(.data=dscale, var="Project_TCGA_More")
#   dscale <- t(as.matrix(dscale))
#   rows_more <- list("Aetiology"=sbs_aetiologies[sbs_aetiologies$Signature %in% rownames(dscale), "Aetiology"])
# 
#   scale_breaks <- seq(from=0, to=1, by=0.1)
# 
#   dcolor <- prism_counts %>%
#     group_by(Project_TCGA_More) %>%
#     summarize_at(vars(-Patient), ~median(.[.!=0]*1e6/67.2e6)) %>%
#     replace(is.na(.),0)
# 
#   dcolor <- column_to_rownames(.data=dcolor, var="Project_TCGA_More")
#   dcolor <- t(as.matrix(dcolor))
# 
#   color_palette <- c("#ffc651", "#ffa759", "#ff8962", "#ff6b6b", "#cc6999", "#9968c8", "#6767f8", "#4459ce", "#224ba5", 
#                      "#013d7c")
#   color_breaks <- c(0, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10, 25, 1e6)
# 
#   theme <- ttheme_awesome(base_size=12,
#                           rep_mode="col",
#                           core_size=5, 
#                           scale_breaks=scale_breaks,
#                           color_palette=color_palette, 
#                           color_breaks=color_breaks)
# 
#   # main plot
#   g <- table_extra_grob(dscale=dscale, dcolor=dcolor, 
#                         rows=rownames(dscale), cols=colnames(dscale),
#                         cols_more=cols_more, rows_more=rows_more,
#                         theme=theme)
# 
#   g1 <- gtable_legend(d=t(scale_breaks[2:length(scale_breaks)]),
#                       labels=scale_breaks,
#                       widths=rep(theme$core$size, length(scale_breaks)-1),
#                       heights=theme$core$size,
#                       fg_fun=theme$core$fg_fun, 
#                       bg_fun=theme$core$bg_fun, 
#                       fg_params=theme$core$fg_params, 
#                       bg_params=theme$core$bg_params, 
#                       title_label="Prop of tumors with the signature",
#                       title_gp=gpar(fontsize=8),
#                       title_x=0.5*(length(scale_breaks)-1)*theme$core$size,
#                       title_y=1.5*theme$core$size,
#                       labels_gp=gpar(fontsize=6),
#                       labels_pad=-1,
#                       padding=theme$core$padding,
#                       scale_breaks=theme$core$scale_breaks,
#                       rep_mode=theme$core$rep_mode,
#                       r_min=theme$core$scale_ratio*0.5*theme$core$size,
#                       r_max=0.5*theme$core$size)
# 
#   fg_fun <- theme$core$bg_fun
#   fg_params <- theme$core$bg_params
#   fg_params$fill <- NULL
# 
#   # first legend
#   g2 <- gtable_legend(d=t(color_breaks[1:length(color_breaks)-1]),
#                       labels=color_breaks,
#                       widths=rep(theme$core$size, length(color_breaks)-1),
#                       heights=theme$core$size,
#                       fg_fun=fg_fun,
#                       fg_params=fg_params,
#                       bg_fun=NULL,
#                       bg_params=NULL,
#                       title_label="Median mut/Mb due to signature",
#                       title_gp=gpar(fontsize=8),
#                       title_x=0.5*(length(color_breaks)-1)*theme$core$size,
#                       title_y=1.5*theme$core$size,
#                       orientation="horizontal",
#                       labels_gp=gpar(fontsize=6),
#                       labels_pad=-1,
#                       padding=theme$core$padding,
#                       color_breaks=color_breaks,
#                       color_palette=color_palette)
# 
# 
#   # second legend
#   grDevices::pdf(file=file.path("pdfs_plots", "table_extra_grob_prism.pdf"),
#                  width=12,
#                  height=16,
#                  onefile=T)
# 
#   grid.draw(g)
# 
#   vp1 <- viewport(x = 0.5, y = 0.92, 
#                   width = 0.2, height = 0.03,
#                   just = c("left", "bottom"))
#   pushViewport(vp1)
#   grid.draw(g1)
#   popViewport()
# 
#   vp2 <- viewport(x = 0.5, y = 0.88, 
#                   width = 0.2, height = 0.03,
#                   just = c("left", "bottom"))
#   pushViewport(vp2)
#   grid.draw(g2)
#   popViewport()
# 
#   grDevices::dev.off()
# 
#   expect_true(file.exists(file.path("pdfs_plots", "table_extra_grob_prism.pdf")))
# })
# 
# test_that("extra table grob on prism vs tcga works", {
# 
#   # PRISM ==============================================================================================================
# 
#   prism_counts <- read.csv("../../data-raw/prism_counts.csv")
#   prism_counts <- prism_counts %>%
#     filter(Project_TCGA_More != "NaN") %>%
#     mutate(Project_TCGA_More=forcats::fct_recode(Project_TCGA_More, "COAD-READ"="COAD")) %>%
#     group_by(Project_TCGA_More) %>%
#     mutate(n=n())
# 
#   # select primaries
#   threshold_min_n <- 3
#   prism_counts <- prism_counts[prism_counts$n >= threshold_min_n,]
#   prism_counts$n <- NULL
# 
#   dscale_prism <- prism_counts %>%
#     group_by(Project_TCGA_More) %>%
#     mutate(n=n()) %>%
#     summarize_at(vars(-Patient), ~sum(.x>0)) %>%
#     mutate_at(vars(-Project_TCGA_More,-n), ~./n) %>%
#     rename(Cancer.Types=Project_TCGA_More) %>%
#     mutate(Cancer.Types=paste0(Cancer.Types, " - PRISM"))
# 
#   dcolor_prism <- prism_counts %>%
#     group_by(Project_TCGA_More) %>%
#     summarize_at(vars(-Patient), ~median(.[.!=0]*1e6/50e6)) %>%
#     replace(is.na(.),0) %>%
#     rename(Cancer.Types=Project_TCGA_More) %>%
#     mutate(Cancer.Types=paste0(Cancer.Types, " - PRISM"))
# 
#   # TCGA ===============================================================================================================
#   old2new <- list("Adrenal-neoplasm"="ACC",
#                   "Breast-cancer"="BRCA",
#                   "ColoRect-AdenoCa"="COAD-READ",
#                   "Eye-Melanoma"="Eye",
#                   "Kidney-Papillary"="KIRP",
#                   "Lung-AdenoCa"="LUAD",
#                   "Mesothelium-Mesothelioma"="MESO",
#                   "Pheochromocytoma"="PCPG",
#                   "Skin-Melanoma"="SKCM",
#                   "Thy-AdenoCa"="THCA",
#                   "UCS"="UCS",
#                   "AML"="AML",
#                   "Cervix-CA"="CESC",
#                   "DLBC"="DLBC",
#                   "Head-SCC"="HNSC",
#                   "Kidney-RCC"="KIRC",
#                   "Lung-SCC"="LUSC",
#                   "Ovary-AdenoCa"="OV",
#                   "Prost-AdenoCa"="PRAD",
#                   "Stomach-AdenoCa"="STAD",
#                   "Thymoma"="THYM",
#                   "Uterus-AdenoCa"="UCEC",
#                   "Biliary-AdenoCa"="CHOL",
#                   "CNS-GBM"="GBM",
#                   "Eso-AdenoCa"="ESCA",
#                   "Kidney-ChRCC"="KICH",
#                   "Liver-HCC"="LIHC",
#                   "Lymph-BNHL"="Lymph-BNHL",
#                   "Panc-AdenoCa"="PAAD",
#                   "Sarcoma"="SARC",
#                   "Testis-Ca"="TGCT",
#                   "Transitional-cell-carcinoma"="BLCA")
# 
#   tcga_counts <- read.csv("../../data-raw/TCGA_WES_sigProfiler_SBS_signatures_in_samples.csv")
#   levels(tcga_counts$Cancer.Types) <- unlist(old2new[levels(tcga_counts$Cancer.Types)])
# 
#   dscale_tcga <- tcga_counts %>%
#     group_by(Cancer.Types) %>%
#     mutate(n=n()) %>%
#     summarize_at(vars(-Sample.Names, -Accuracy), ~sum(.x>0)) %>%
#     mutate_at(vars(-Cancer.Types,-n), ~./n) %>%
#     mutate(Cancer.Types=paste0(Cancer.Types, " - TCGA"))
# 
#   dcolor_tcga <- tcga_counts %>%
#     group_by(Cancer.Types) %>%
#     summarize_at(vars(-Sample.Names, -Accuracy), ~median(.[.!=0]*1e6/50e6)) %>%
#     replace(is.na(.),0) %>%
#     mutate(Cancer.Types=paste0(Cancer.Types, " - TCGA"))
# 
#   # Merge ==============================================================================================================
# 
#   dscale <- rbind(dscale_tcga, dscale_prism)
#   dscale <- dscale %>% arrange(Cancer.Types)
# 
#   cols_more <- list("n="=dscale$n)
#   dscale$n <- NULL
#   dscale <- column_to_rownames(.data=dscale, var="Cancer.Types")
#   dscale <- t(as.matrix(dscale))
#   rows_more <- list("Aetiology"=sbs_aetiologies[sbs_aetiologies$Signature %in% rownames(dscale), "Aetiology"])
#  
#   dcolor <- rbind(dcolor_tcga, dcolor_prism)
#   dcolor <- dcolor %>% arrange(Cancer.Types)
# 
#   dcolor <- column_to_rownames(.data=dcolor, var="Cancer.Types")
#   dcolor <- t(as.matrix(dcolor))
# 
#   # Theme ==============================================================================================================
# 
#   color_palette <- c("#ffc651", "#ffa759", "#ff8962", "#ff6b6b", "#cc6999", "#9968c8", "#6767f8", "#4459ce", "#224ba5", 
#                      "#013d7c")
#   color_breaks <- c(0, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10, 25, 1e6)
#   scale_breaks <- seq(from=0, to=1, by=0.1)
# 
#   bg_fill <- ifelse(grepl("TCGA", colnames(dscale)), "#f2f2f2", "#e5e5e5")
# 
#   theme <- ttheme_awesome(base_size=12,
#                           rep_mode="col",
#                           core_size=5, 
#                           scale_breaks=scale_breaks,
#                           color_palette=color_palette, 
#                           color_breaks=color_breaks, 
#                           core=list(bg_params=list(fill=bg_fill)))
# 
#   # main plot
#   g <- table_extra_grob(dscale=dscale, dcolor=dcolor, 
#                         rows=rownames(dscale), cols=colnames(dscale),
#                         cols_more=cols_more, rows_more=rows_more,
#                         theme=theme)
# 
#   theme$core$size <- 1.5*theme$core$size
#   theme$core$bg_params$width <- 1.5*theme$core$bg_params$width
#   theme$core$bg_params$height <- 1.5*theme$core$bg_params$height
# 
#   # legend 1
#   g1 <- gtable_legend(d=t(scale_breaks[2:length(scale_breaks)]),
#                       labels=scale_breaks,
#                       widths=rep(theme$core$size, length(scale_breaks)-1),
#                       heights=theme$core$size,
#                       fg_fun=theme$core$fg_fun, 
#                       bg_fun=theme$core$bg_fun, 
#                       fg_params=theme$core$fg_params, 
#                       bg_params=theme$core$bg_params, 
#                       title_label="Prop of tumors with the signature",
#                       title_gp=gpar(fontsize=12),
#                       title_x=0.5*(length(scale_breaks)-1)*theme$core$size,
#                       title_y=1.5*theme$core$size,
#                       labels_gp=gpar(fontsize=9),
#                       labels_pad=-1,
#                       padding=theme$core$padding,
#                       scale_breaks=theme$core$scale_breaks,
#                       rep_mode=theme$core$rep_mode,
#                       r_min=theme$core$scale_ratio*0.5*theme$core$size,
#                       r_max=0.5*theme$core$size)
# 
#   # legend 2
#   fg_fun <- theme$core$bg_fun
#   fg_params <- theme$core$bg_params
#   fg_params$fill <- NULL
# 
#   g2 <- gtable_legend(d=t(color_breaks[1:length(color_breaks)-1]),
#                      labels=color_breaks,
#                      widths=rep(theme$core$size, length(color_breaks)-1),
#                      heights=theme$core$size,
#                      fg_fun=fg_fun,
#                      fg_params=fg_params,
#                      bg_fun=NULL,
#                      bg_params=NULL,
#                      title_label="Median mut/Mb due to signature",
#                      title_gp=gpar(fontsize=12),
#                      title_x=0.5*(length(color_breaks)-1)*theme$core$size,
#                      title_y=1.5*theme$core$size,
#                      orientation="horizontal",
#                      labels_gp=gpar(fontsize=9),
#                      labels_pad=-1,
#                      padding=theme$core$padding,
#                      color_breaks=color_breaks,
#                      color_palette=color_palette)
# 
#   grDevices::pdf(file=file.path("pdfs_plots", "table_extra_grob_prism_vs_tcga.pdf"),
#                  width=18,
#                  height=16.5,
#                  onefile=T)
# 
#   grid.draw(g)
# 
#   vp1 <- viewport(x = 0.6, y = 0.92, 
#                   width = 0.3, height = 0.03,
#                   just = c("left", "bottom"))
#   pushViewport(vp1)
#   grid.draw(g1)
#   popViewport()
# 
#   vp2 <- viewport(x = 0.6, y = 0.86, 
#                   width = 0.3, height = 0.03,
#                   just = c("left", "bottom"))
#   pushViewport(vp2)
#   grid.draw(g2)
#   popViewport()
# 
#   grDevices::dev.off()
# 
#   expect_true(file.exists(file.path("pdfs_plots", "table_extra_grob_prism_vs_tcga.pdf")))
# })
