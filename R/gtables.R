## Note: this functions is modified copy of gtable_table from the gridExtra package.

#' Build a table with foreground and background grobs.
#'
#' This function is a copy of the internal function \code{gtable_table} of gridExtra package. 
#'
#' @param d data.frame or matrix
#' @param widths optional \code{unit.list} specifying the grob widths
#' @param heights optional \code{unit.list} specifying the grob heights
#' @param fg_fun grob-drawing function
#' @param fg_params  named list of params passed to fg_fun
#' @param bg_fun grob-drawing function 
#' @param bg_params  named list of params passed to bg_fun
#' @param padding \code{unit.list} object specifying the padding between adjacent cells.
#' @param name optional name of the grob
#' @param vp optional viewport
#' @param ... additional parameters passed to add_table_params.
#' @return A gtable.
#'
#' @importFrom gtable gtable_matrix gtable_add_grob
#'
#' @author Yoann Pradat
#' @keywords internal
gtable_table <- function(d, widths, heights,
                         fg_fun = text_grob, fg_params = list(),
                         bg_fun = rect_grob, bg_params = list(),
                         padding = unit(c(4, 4), "mm"),
                         name = "table", vp = NULL, ...){

  d <- as.matrix(d)

  nc <- ncol(d)
  nr <- nrow(d)
  n <- nc*nr

  table_params <- table_params(d, fg_params, bg_params)
  bg_params <- table_params[["bg_params"]]
  fg_params <- table_params[["fg_params"]]
  fg_params <- add_table_params(d, fg_params, fg_fun, ...)

  frgds <- do.call(mapply, c(fg_params, list(FUN = fg_fun, SIMPLIFY=FALSE)))
  bkgds <- do.call(mapply, c(bg_params, list(FUN = bg_fun, SIMPLIFY=FALSE)))

  frgds_grobs <- matrix(frgds, ncol = nc, byrow = FALSE)
  bkgds_grobs <- matrix(bkgds, ncol = nc, byrow = FALSE)

  if(missing(widths))
    widths <- col_widths(frgds_grobs) + padding[1]
  if(missing(heights))
    heights <- row_heights(frgds_grobs) + padding[2]

  ## make the gtable matrix of foreground
  g <- gtable_matrix(paste0(name, "-fg"), 
                     grobs = frgds_grobs, 
                     widths = widths, 
                     heights = heights, vp=vp)
  
  ## add the background
  g <- gtable_add_grob(g, bkgds_grobs, 
                       t=rep(seq_len(nr), length.out = n), 
                       l=rep(seq_len(nc), each = nr), z=0, 
                       name=paste0(name, "-bg"))
  
  # add padding
  g <- gtable::gtable_add_col_space(g, padding[1])
  g <- gtable::gtable_add_row_space(g, padding[2])

  g
}

# library(grid)
# library(gtable)
# suppressMessages(library(SummarizedExperiment))
# load("../tests/testthat/testdata/DBS.rda")
# 
# theme <- ttheme_awesome(base_size=8)
# cols  <- t(colnames(DBS))
# 
# name="colhead"
# fg_fun = theme$colhead$fg_fun 
# bg_fun = theme$colhead$bg_fun 
# fg_params = theme$colhead$fg_params 
# bg_params = theme$colhead$bg_params 
# padding=theme$colhead$padding
# 
# d <- cols
# 
# nc <- ncol(d)
# nr <- nrow(d)
# n <- nc*nr
# 
# table_params <- table_params(d, fg_params, bg_params)
# bg_params <- table_params[["bg_params"]]
# fg_params <- table_params[["fg_params"]]
# fg_params <- add_table_params(d, fg_params, fg_fun)
# 
# frgds <- do.call(mapply, c(fg_params, list(FUN = fg_fun, SIMPLIFY=FALSE)))
# bkgds <- do.call(mapply, c(bg_params, list(FUN = bg_fun, SIMPLIFY=FALSE)))
# 
# frgds_grobs <- matrix(frgds, ncol = nc, byrow = FALSE)
# bkgds_grobs <- matrix(bkgds, ncol = nc, byrow = FALSE)
# 
# pdf("testplot.pdf", width=6, height=6)
# 
# widths <- col_widths(frgds_grobs) + padding[1]
# heights <- row_heights(frgds_grobs) + padding[2]
# 
# ## make the gtable matrix of foreground
# g <- gtable_matrix(paste0(name, "-fg"), 
#                    grobs = frgds_grobs, 
#                    widths = widths, 
#                    heights = heights, vp=vp)
# 
# ## add the background
# g <- gtable_add_grob(g, bkgds_grobs, 
#                      t=rep(seq_len(nr), length.out = n), 
#                      l=rep(seq_len(nc), each = nr), z=0, 
#                      name=paste0(name, "-bg"))
# 
# # add padding
# g <- gtable::gtable_add_col_space(g, padding[1])
# g <- gtable::gtable_add_row_space(g, padding[2])
# 
# grid.draw(g)
# 
# dev.off()
# 
# gc <- gtable_table(t(cols), name="colhead",
#                    fg_fun = theme$colhead$fg_fun, 
#                    bg_fun = theme$colhead$bg_fun, 
#                    fg_params = theme$colhead$fg_params, 
#                    bg_params = theme$colhead$bg_params, 
#                    padding=theme$colhead$padding)
# 
# gr <- gtable_table(rowData(DBS)$name, name="colhead",
#                    fg_fun = theme$rowhead$fg_fun, 
#                    bg_fun = theme$rowhead$bg_fun, 
#                    fg_params = theme$rowhead$fg_params, 
#                    bg_params = theme$rowhead$bg_params, 
#                    padding=theme$rowhead$padding)
# 
# 
# d = t(cols)
# d <- rowData(DBS)$name
# 
# nc <- ncol(d)
# nr <- nrow(d)
# n <- nc*nr
# 
# 
# 
# table_params <- table_params(d, fg_params, bg_params)
# bg_params <- table_params[["bg_params"]]
# fg_params <- table_params[["fg_params"]]
# fg_params <- add_table_params(d, fg_params, fg_fun)
# 
# frgds <- do.call(mapply, c(fg_params, list(FUN = fg_fun, SIMPLIFY=FALSE)))
# bkgds <- do.call(mapply, c(bg_params, list(FUN = bg_fun, SIMPLIFY=FALSE)))
# 
# frgds_grobs <- matrix(frgds, ncol = nc, byrow = FALSE)
# bkgds_grobs <- matrix(bkgds, ncol = nc, byrow = FALSE)
# 
# 
# all.equal(fg_fun, text_grob)



# #' Build a table with text grobs.
# #'
# #' This function is a copy of the internal function \code{gtable_table} of gridExtra package. 
# #'
# #' @param d data.frame or matrix
# #' @param width optional \code{unit.list} specifying the grob widths
# #' @param heights optional \code{unit.list} specifying the grob heights
# #' @param fg_fun grob-drawing function
# #' @param fg_params  named list of params passed to fg_fun
# #' @param bg_fun grob-drawing function 
# #' @param bg_params  named list of params passed to bg_fun
# #' @param padding \code{unit.list} object specifying the padding between adjacent cells.
# #' @param name optional name of the grob
# #' @param vp optional viewport
# #' @return A gtable.
# #'
# #' @importFrom gtable gtable_matrix gtable_add_grob
# #'
# #' @author Yoann Pradat
# #' @keywords internal
# gtable_text <- function(d, widths, heights,
#                         fg_fun = text_grob, fg_params = list(),
#                         bg_fun = rect_grob, bg_params = list(),
#                         padding = unit(c(4, 4), "mm"),
#                         name = "table", vp = NULL){
#   
#   label_matrix <- as.matrix(d)
#   
#   nc <- ncol(label_matrix)
#   nr <- nrow(label_matrix)
#   n <- nc*nr
#   
#   ## formatting parameters will be recycled iff 
#   ## there are fewer elements than needed
#   rep_ifshort <- function(x, n, nc, nr){
#       if(length(x) >= n){
#         return(x[1:n]) 
#       } else # recycle 
#         return(rep(rep(x, length.out = nr), length.out= n)) 
#   }
#   
#   fg_params <- lapply(fg_params, rep_ifshort, n = n, nc = nc, nr = nr)
#   bg_params <- lapply(bg_params, rep_ifshort, n = n, nc = nc, nr = nr)
#   
#   fg_params <- data.frame(fg_params, 
#                           label = as.vector(label_matrix), # colwise
#                           stringsAsFactors=FALSE)
#   
#   bg_params <- data.frame(bg_params, stringsAsFactors=FALSE)
#   
#   labels <- do.call(mapply, c(fg_params, list(FUN = fg_fun, 
#                                               SIMPLIFY=FALSE)))
#   bkgds <- do.call(mapply, c(bg_params, list(FUN = bg_fun, 
#                                              SIMPLIFY=FALSE)))
# 
#   label_grobs <- matrix(labels, ncol = nc, byrow = FALSE)
#   bkgds_grobs <- matrix(bkgds, ncol = nc, byrow = FALSE)
# 
#   if(missing(widths))
#     widths <- col_widths(label_grobs) +  padding[1]
#   if(missing(heights))
#     heights <- row_heights(label_grobs) +  padding[2]
# 
#   ## place labels in a gtable
#   g <- gtable_matrix(paste0(name, "-fg"), 
#                      grobs = label_grobs, 
#                      widths = widths, 
#                      heights = heights, vp=vp)
#   
#   ## add the background
#   g <- gtable_add_grob(g, bkgds_grobs, 
#                        t=rep(seq_len(nr), length.out = n), 
#                        l=rep(seq_len(nc), each = nr), z=0, 
#                        name=paste0(name, "-bg"))
#   
#   # add padding
#   g <- gtable::gtable_add_col_space(g, padding[1])
#   g <- gtable::gtable_add_row_space(g, padding[2])
# 
#   g
# }
# 
# #' Build a table with circle grobs.
# #'
# #' This function is a copy of the internal function \code{gtable_table} of gridExtra package. 
# #'
# #' @param d data.frame or matrix
# #' @param width optional \code{unit.list} specifying the grob widths
# #' @param heights optional \code{unit.list} specifying the grob heights
# #' @param fg_fun grob-drawing function
# #' @param fg_params  named list of params passed to fg_fun
# #' @param bg_fun grob-drawing function 
# #' @param bg_params  named list of params passed to bg_fun
# #' @param padding \code{unit.list} object specifying the padding between adjacent cells.
# #' @param name optional name of the grob
# #' @param vp optional viewport
# #' @return A gtable.
# #'
# #' @importFrom gtable gtable_matrix gtable_add_grob
# #'
# #' @author Yoann Pradat
# #' @keywords internal
# gtable_circle <- function(d, widths, heights,
#                           fg_fun = text_grob, fg_params = list(),
#                           bg_fun = rect_grob, bg_params = list(),
#                           core_size = unit(10, "mm"),
#                           padding = unit(c(1, 1), "mm"),
#                           name = "table", vp = NULL){
# 
#   d <- as.matrix(d)
#   nc <- ncol(d)
#   nr <- nrow(d)
#   n <- nc*nr
# 
#   fg_fun <- rect_grob()
# 
# 
#   theme <- ttheme_awesome()
#   fg_params <- theme$core$fg_params
# 
#   fg_params <- list(r=do.call(unit.c, lapply(sizes, function(x) x*core_size)))
#   
#   
#   fg_params <- lapply(fg_params, rep_ifshort, n = n, nc = nc, nr = nr)
#   bg_params <- lapply(bg_params, rep_ifshort, n = n, nc = nc, nr = nr)
#                           
#   fg_params <- data.frame(fg_params, 
#                           r = do.call(unit.c, do.call(as.vector(d_matrix), # colwise
#                           stringsAsFactors=FALSE)
#   
#   bg_params <- data.frame(bg_params, stringsAsFactors=FALSE)
#   
#   labels <- do.call(mapply, c(fg_params, list(FUN = fg_fun, 
#                                               SIMPLIFY=FALSE)))
#   bkgds <- do.call(mapply, c(bg_params, list(FUN = bg_fun, 
#                                              SIMPLIFY=FALSE)))
# 
#   label_grobs <- matrix(labels, ncol = nc, byrow = FALSE)
#   bkgds_grobs <- matrix(bkgds, ncol = nc, byrow = FALSE)
# 
#   # if(missing(widths))
#   #   #widths <- rep(max(col_widths(label_grobs)), nc) + padding[1]
#   #   widths <- rep(max(col_widths(label_grobs)), nc) + padding[1]
#   # if(missing(heights))
#   #   #heights <- rep(max(row_heights(label_grobs)), nr) + padding[2]
#   #   heights <- rep(max(row_heights(label_grobs)), nr) + padding[2]
# 
#   widths  <- rep(core_size, nc) - padding[1]
#   heights <- rep(core_size, nr) - padding[2]
# 
#   ## place labels in a gtable
#   g <- gtable_matrix(paste0(name, "-fg"), 
#                      grobs = label_grobs, 
#                      widths = widths, 
#                      heights = heights, vp=vp)
# 
#   
#   ## add the background
#   g <- gtable_add_grob(g, bkgds_grobs, 
#                        t=rep(seq_len(nr), length.out = n), 
#                        l=rep(seq_len(nc), each = nr), z=0, 
#                        name=paste0(name, "-bg"))
#   ## add padding
#   g <- gtable::gtable_add_col_space(g, padding[1])
#   g <- gtable::gtable_add_row_space(g, padding[2])
#   
#   g
# }

# library(grid)
# library(gtable)
# load("../tests/testthat/testdata/DBS.rda")
# 
# 
# apply(d, 1:2, function(x) x * unit(1, "mm"))
# 
# d <- SummarizedExperiment::assays(DBS)$proportion
# d <- norm_and_cat(d,ncat=theme$core$ncircle, vmax=0.5)
# col <- t(colnames(d))
# 
# theme <- ttheme_awesome(base_size=8)
# 
# pdf('testplot.pdf')
# 
# g1 <- gtable_text(col, name="colhead-1",
#                   fg_fun = theme$colhead$fg_fun, 
#                   bg_fun = theme$colhead$bg_fun, 
#                   fg_params = theme$colhead$fg_params, 
#                   bg_params = theme$colhead$bg_params, 
#                   padding=theme$colhead$padding)
# 
# g2 <- gtable_circle(d, name="circle",
#                     fg_fun = theme$core$fg_fun, 
#                     bg_fun = theme$core$bg_fun, 
#                     fg_params = theme$core$fg_params, 
#                     bg_params = theme$core$bg_params, 
#                     padding=theme$core$padding)
# 
# g <- rbind(g1, g2, size="last")
# 
# g2 <- gtable_add_cols(g2, pos=0, widths=unit(6, "cm"))
# g2 <- gtable_add_rows(g2, pos=0, heights=unit(8, "cm"))
# g2 <- gtable_add_rows(g2, pos=-1, heights=unit(8, "cm"))
# g2 <- gtable_add_cols(g2, pos=-1, widths=unit(6, "cm"))
# 
# grid.draw(g2)
# 
# dev.off()
# 
# col_widths <- function(m){
#   do.call(grid::unit.c, apply(m, 2, function(l)
#     max(do.call(grid::unit.c, lapply(l, grid::grobWidth)))))
# }



# library(gtable)
# 
# load("../tests/testthat/testdata/DBS.rda")
# 
# pdf("testplot.pdf")
# 
# theme <- ttheme_awesome()
# d <- SummarizedExperiment::rowData(DBS)$name
# d <- matrix(rep(d, 5), nrow=5, byrow=T)
# 
# g1 <- gtable_text(d, name="colhead-1",
#                   fg_fun = theme$colhead$fg_fun, 
#                   bg_fun = theme$colhead$bg_fun, 
#                   fg_params = theme$colhead$fg_params, 
#                   bg_params = theme$colhead$bg_params, 
#                   padding=theme$colhead$padding)
# 
# g2 <- gtable_text(d, name="colhead-2",
#                   fg_fun = theme$colhead$fg_fun, 
#                   bg_fun = theme$colhead$bg_fun, 
#                   fg_params = theme$colhead$fg_params, 
#                   bg_params = theme$colhead$bg_params, 
#                   padding=theme$colhead$padding)
# 
# g <- cbind(g1, g2, size="first")
# 
# grid.draw(g)
# 
# dev.off()

# pdf("testplot.pdf")
# 
# library(grid)
# gt <- gtable(unit(1, "null"), unit(0.5, "null"))
# gt <- gtable_add_grob(gt, rectGrob(gp = gpar(fill = "black"), x=1, y=0.5), 1, 1)
# 
# plot(gt)
# plot(cbind(gt, gt))
# plot(rbind(gt, gt))
# 
# pad <- gtable_add_padding(gt, unit(1, "cm"))
# plot(pad)
# plot(cbind(pad, pad))
# plot(rbind(pad, pad))
# 
# pad <- gtable_add_padding(pad, unit(-0.5, "cm"))
# plot(pad)
# plot(cbind(pad, pad))
# plot(rbind(pad, pad))
# 
# dev.off()
# 
# print("ok")

# load("../tests/testthat/testdata/DBS.rda")
# suppressMessages(library(SummarizedExperiment))
#  
# d <- assays(DBS)$proportion
# theme <- ttheme_awesome()
# 
# d <- norm_and_cat(d,ncat=theme$core$ncircle, vmax=0.5)


# 
# ## map continuous values to [0,1]
# if (min(d) == max(d)){
#   if (min(d) != 0){
#     d_norm <- d/max(d)
#   } else {
#     d_norm <- d
#   }
# } else {
#   d_norm <- (d-min(d))/(max(d) - min(d))
# }
# 
# ## bin into n_circle categories and map to [0,0.5]
# d_norm <- apply(d_norm, 1:2, function(x) round(x*theme$core$n_circle)/(2*theme$core$n_circle))
# 
# cols <- colnames(d)
# rows <- rownames(d)
# 
# g <- gtable_circle(d_norm, name="core",
#                   fg_fun = theme$core$fg_fun, 
#                   bg_fun = theme$core$bg_fun, 
#                   fg_params = theme$core$fg_params, 
#                   bg_params = theme$core$bg_params, 
#                   padding=theme$core$padding)
# 
# if(!is.null(cols)){
#   gc <- gtable_text(t(cols), name="colhead",
#                      fg_fun = theme$colhead$fg_fun, 
#                      bg_fun = theme$colhead$bg_fun, 
#                      fg_params = theme$colhead$fg_params, 
#                      bg_params = theme$colhead$bg_params, 
#                      padding=theme$colhead$padding)
#   gc <- gtable_add_rows(gc, theme$rowhead$padding[1])
#   g <- rbind_2(gc, g, "max")
# 
# 
# if(!is.null(rows)){
#   if(!is.null(cols)) # need to add dummy cell
#     rows <- c("", rows)
#   gr <- gtable_text(rows, name="rowhead",
#                      fg_fun = theme$rowhead$fg_fun, 
#                      bg_fun = theme$rowhead$bg_fun, 
#                      fg_params = theme$rowhead$fg_params, 
#                      bg_params = theme$rowhead$bg_params,
#                      padding=theme$rowhead$padding)
#   g <- cbind_2(gr, g, "max")
# }
# 
# colnames(g) <- paste0("c", seq_len(ncol(g)))
# rownames(g) <- paste0("r", seq_len(nrow(g)))


# load("../tests/testthat/testdata/DBS.rda")
# suppressMessages(library(SummarizedExperiment))
#  
# d <- assays(DBS)$proportion
# d <- apply(d, 1:2, function(x) round(x*10)/20)
# theme <- ttheme_awesome()
# # 
# # fg_fun = theme$core$fg_fun
# # bg_fun = theme$core$bg_fun 
# # fg_params = theme$core$fg_params
# # bg_params = theme$core$bg_params
# # padding=theme$core$padding
# 
# 
# circle_small_plot <- function(nc=2, nr=2, padding=grid::unit(c(0.01,0.01), "npc")){
#   row_heights <- function(m){
#     do.call(grid::unit.c, apply(m, 1, function(l)
#       max(do.call(grid::unit.c, lapply(l, grid::grobHeight)))))
#   }
# 
#   col_widths <- function(m){
#     do.call(grid::unit.c, apply(m, 2, function(l)
#       max(do.call(grid::unit.c, lapply(l, grid::grobWidth)))))
#   }
# 
#   fg_params <- data.frame(r=c(unit(0.5, "npc")), fill=c("#6767f8"), lwd=c(0), col=c("white"), stringsAsFactors=F)
#   bg_params <- data.frame(fill=c("#f2f2f2"), lwd=0, col=c("white"), stringsAsFactors=F)
# 
# 
#   fg_params <- fg_params[rep(seq_len(nrow(fg_params)), each = nc*nr), ]
#   bg_params <- bg_params[rep(seq_len(nrow(bg_params)), each = nc*nr), ]
# 
#   labels <- do.call(mapply, c(fg_params, list(FUN = circle_grob, 
#                                               SIMPLIFY=FALSE)))
#   bkgds <- do.call(mapply, c(bg_params, list(FUN = rect_grob, 
#                                              SIMPLIFY=FALSE)))
# 
#   label_grobs <- matrix(labels, ncol = nc, byrow = FALSE)
#   bkgds_grobs <- matrix(bkgds, ncol = nc, byrow = FALSE)
# 
#   # widths <- rep(max(col_widths(label_grobs)), nc) + padding[1]
#   # heights <- rep(max(row_heights(label_grobs)), nr) + padding[2]
# 
#   widths  <- rep(unit(1/max(nc,nr), "npc"), nc) - padding[1]
#   heights <- rep(unit(1/max(nc,nr), "npc"), nr) - padding[2]
# 
#   ## place labels in a gtable
#   g <- gtable::gtable_matrix(paste0(name, "-fg"), 
#                      grobs = label_grobs, 
#                      widths = widths, 
#                      heights = heights, vp=vp)
# 
#   ## add the background
#   g <- gtable::gtable_add_grob(g, bkgds_grobs, 
#                                t=rep(seq_len(nr), length.out = nr*nc), 
#                                l=rep(seq_len(nc), each = nr), z=0, 
#                                name=paste0(name, "-bg"))
#   g <- gtable::gtable_add_col_space(g, padding[1])
#   g <- gtable::gtable_add_row_space(g, padding[2])
# 
#   g
# }
# 
# pdf(file="gtable_circle_small.pdf")
# 
# g <- circle_small_plot(nc=3, nr=2, padding=unit(c(0.01,0.01), "npc"))
# 
# grid.draw(g)
# dev.off()
# 
#  
# pdf(file="gtable_circle_small.pdf")
# 
# g1 <- circle_small_plot(nc=2, nr=4)
# g2 <- circle_small_plot(nc=1, nr=4)
# 
# g <- cbind_2(g1, g2, "max")
# 
# grid.draw(g)
# dev.off()
